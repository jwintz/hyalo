;;; hyalo-minibuffer.el --- Native Swift minibuffer bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Bridges Emacs's minibuffer to a native Swift panel.
;; Emacs owns filtering/ordering (vertico, orderless, fido); Swift owns rendering.
;;
;; Flow:
;;   User types in Swift TextField
;;     → channel callback injects text into Emacs minibuffer
;;     → completion framework processes input
;;     → timer extracts candidates + annotations as JSON
;;     → calls hyalo-minibuffer-update (defun registered by Swift module)
;;     → Swift decodes JSON, updates MinibufferViewModel
;;     → SwiftUI re-renders candidate list

;;; Code:

(require 'json)

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defvar hyalo-minibuffer--active nil
  "Non-nil when the Swift minibuffer panel is active.")

(defvar hyalo-minibuffer--session-id 0
  "Monotonically increasing session ID.  Prevents stale updates.")

(defvar hyalo-minibuffer--update-timer nil
  "Timer that pushes candidate updates to Swift.")

(defvar hyalo-minibuffer--inhibit nil
  "When non-nil, fall through to native Emacs minibuffer.
Used for recursive minibuffers, password prompts, y-or-n-p, etc.")

(defvar hyalo-minibuffer--prompt nil
  "The current minibuffer prompt text.")

(defvar hyalo-minibuffer--max-candidates 50
  "Maximum number of candidates to send to Swift for rendering performance.")

(defvar hyalo-minibuffer--update-delay 0.05
  "Timer delay in seconds for candidate extraction (coalesces rapid keystrokes).")

;; ---------------------------------------------------------------------------
;; Logging
;; ---------------------------------------------------------------------------

(defvar hyalo-minibuffer--debug t
  "When non-nil, log minibuffer bridge activity to *Messages*.")

(defun hyalo-minibuffer--log (fmt &rest args)
  "Log FMT with ARGS when debug is enabled."
  (when hyalo-minibuffer--debug
    (apply #'message (concat "[hyalo-minibuffer] " fmt) args)))

;; ---------------------------------------------------------------------------
;; Skip conditions
;; ---------------------------------------------------------------------------

(defun hyalo-minibuffer--should-skip-p ()
  "Return non-nil if we should fall through to native Emacs minibuffer."
  (or hyalo-minibuffer--inhibit
      ;; Recursive minibuffer (depth > 1)
      (> (minibuffer-depth) 1)
      ;; Password prompts
      (bound-and-true-p read-passwd--mode)
      ;; Detect password prompt text
      (let ((prompt (minibuffer-prompt)))
        (and prompt
             (or (string-match-p "\\b[Pp]ass\\(word\\|phrase\\)\\b" prompt)
                 (string-match-p "\\bPIN\\b" prompt))))
      ;; y-or-n-p and yes-or-no-p
      (and (minibuffer-prompt)
           (string-match-p "\\(y or n\\|yes or no\\)" (minibuffer-prompt)))))

;; ---------------------------------------------------------------------------
;; Candidate extraction — framework detection
;; ---------------------------------------------------------------------------

(defun hyalo-minibuffer--extract-candidates ()
  "Extract current completion candidates with annotations.
Detects vertico, fido-vertical-mode, or generic completion."
  (condition-case err
      (cond
       ;; Vertico (macOS desktop)
       ((bound-and-true-p vertico-mode)
        (hyalo-minibuffer--extract-vertico))

       ;; Fido / icomplete (iOS)
       ((and (bound-and-true-p fido-vertical-mode)
             (fboundp 'completion-all-sorted-completions))
        (hyalo-minibuffer--extract-fido))

       ;; Generic fallback
       (t
        (hyalo-minibuffer--extract-generic)))
    (error
     (hyalo-minibuffer--log "extract error: %s" (error-message-string err))
     nil)))

(defun hyalo-minibuffer--extract-vertico ()
  "Extract candidates from vertico's internal state."
  (let* ((cands (and (boundp 'vertico--candidates) vertico--candidates))
         (index (if (boundp 'vertico--index) vertico--index -1))
         (total (length cands))
         (limit (min total hyalo-minibuffer--max-candidates))
         (annotate-fn (hyalo-minibuffer--get-annotation-fn))
         (result nil))
    (hyalo-minibuffer--log "vertico: %d candidates, index=%d, annotate-fn=%s"
                           total index (if annotate-fn "yes" "no"))
    (dotimes (i limit)
      (let* ((cand (nth i cands))
             (annotation (if annotate-fn
                             (or (ignore-errors (funcall annotate-fn cand)) "")
                           "")))
        (push `((text . ,(substring-no-properties cand))
                (annotation . ,(string-trim annotation))
                (selected . ,(if (eq i index) t :json-false)))
              result)))
    (list (cons 'candidates (vconcat (nreverse result)))
          (cons 'selectedIndex index)
          (cons 'totalCandidates total))))

(defun hyalo-minibuffer--extract-fido ()
  "Extract candidates from fido/icomplete completion state."
  (let* ((completions (completion-all-sorted-completions
                       (minibuffer-prompt-end) (point-max)))
         (cands (when (consp completions)
                  (let ((head completions) result)
                    (while (consp head)
                      (push (car head) result)
                      (setq head (cdr head))
                      (when (eq head completions) (setq head nil)))
                    (nreverse result))))
         (total (length cands))
         (limit (min total hyalo-minibuffer--max-candidates))
         (annotate-fn (hyalo-minibuffer--get-annotation-fn))
         (result nil))
    (hyalo-minibuffer--log "fido: %d candidates" total)
    (dotimes (i limit)
      (let* ((cand (nth i cands))
             (annotation (if annotate-fn
                             (or (ignore-errors (funcall annotate-fn cand)) "")
                           "")))
        (push `((text . ,(substring-no-properties cand))
                (annotation . ,(string-trim annotation))
                (selected . ,(if (eq i 0) t :json-false)))
              result)))
    (list (cons 'candidates (vconcat (nreverse result)))
          (cons 'selectedIndex 0)
          (cons 'totalCandidates total))))

(defun hyalo-minibuffer--extract-generic ()
  "Extract candidates using `completion-all-completions' fallback."
  (let* ((contents (minibuffer-contents-no-properties))
         (completions (ignore-errors
                        (completion-all-completions
                         contents
                         minibuffer-completion-table
                         minibuffer-completion-predicate
                         (- (point) (minibuffer-prompt-end)))))
         ;; completion-all-completions may return a dotted list
         (cands (when (consp completions)
                  (let ((last (last completions)))
                    (when (cdr last) (setcdr last nil)))
                  completions))
         (total (length cands))
         (limit (min total hyalo-minibuffer--max-candidates))
         (annotate-fn (hyalo-minibuffer--get-annotation-fn))
         (result nil))
    (hyalo-minibuffer--log "generic: %d candidates" total)
    (dotimes (i limit)
      (let* ((cand (nth i cands))
             (annotation (if annotate-fn
                             (or (ignore-errors (funcall annotate-fn cand)) "")
                           "")))
        (push `((text . ,(if (stringp cand) (substring-no-properties cand) ""))
                (annotation . ,(string-trim annotation))
                (selected . ,(if (eq i 0) t :json-false)))
              result)))
    (list (cons 'candidates (vconcat (nreverse result)))
          (cons 'selectedIndex 0)
          (cons 'totalCandidates total))))

(defun hyalo-minibuffer--get-annotation-fn ()
  "Get the marginalia/completion annotation function if available."
  (condition-case nil
      (let* ((input (buffer-substring-no-properties
                     (minibuffer-prompt-end) (point-max)))
             (metadata (completion-metadata
                        input
                        minibuffer-completion-table
                        minibuffer-completion-predicate))
             (cat (completion-metadata-get metadata 'category)))
        (or (and (bound-and-true-p marginalia-mode)
                 cat
                 (alist-get cat (bound-and-true-p marginalia-annotator-registry)))
            (completion-metadata-get metadata 'annotation-function)))
    (error nil)))

;; ---------------------------------------------------------------------------
;; Update timer — push candidates to Swift
;; ---------------------------------------------------------------------------

(defun hyalo-minibuffer--schedule-update ()
  "Schedule a candidate update push to Swift."
  (when hyalo-minibuffer--update-timer
    (cancel-timer hyalo-minibuffer--update-timer))
  ;; Use run-with-timer (not idle timer) so it fires even during minibuffer input
  (setq hyalo-minibuffer--update-timer
        (run-with-timer hyalo-minibuffer--update-delay nil
                        #'hyalo-minibuffer--push-update)))

(defun hyalo-minibuffer--push-update ()
  "Extract candidates and push them to Swift."
  (setq hyalo-minibuffer--update-timer nil)
  (when (and hyalo-minibuffer--active (minibufferp))
    (condition-case err
        (let* ((session hyalo-minibuffer--session-id)
               (extracted (hyalo-minibuffer--extract-candidates))
               (input (minibuffer-contents-no-properties))
               (payload `((sessionId . ,session)
                          (prompt . ,(or hyalo-minibuffer--prompt ""))
                          (input . ,input)
                          ,@extracted))
               (json (json-encode payload)))
          (hyalo-minibuffer--log "push-update: input=%s candidates=%d json-len=%d"
                                 input
                                 (length (cdr (assq 'candidates extracted)))
                                 (length json))
          (when (and extracted (fboundp 'hyalo-minibuffer-update))
            (hyalo-minibuffer-update json)))
      (error
       (hyalo-minibuffer--log "push-update error: %s" (error-message-string err))))))

;; ---------------------------------------------------------------------------
;; Input injection — Swift → Emacs
;; ---------------------------------------------------------------------------

(defun hyalo-minibuffer--inject-input (text)
  "Replace minibuffer contents with TEXT from Swift panel."
  (hyalo-minibuffer--log "inject-input: %s" text)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (insert text)
    ;; Trigger completion framework refresh
    (cond
     ((bound-and-true-p vertico-mode)
      (when (fboundp 'vertico--exhibit)
        (vertico--exhibit)))
     ((bound-and-true-p fido-vertical-mode)
      (when (fboundp 'icomplete-exhibit)
        (icomplete-exhibit))))
    ;; Schedule candidate extraction after framework has processed
    (hyalo-minibuffer--schedule-update)))

;; ---------------------------------------------------------------------------
;; Selection — Swift → Emacs
;; ---------------------------------------------------------------------------

(defun hyalo-minibuffer--select-candidate (index)
  "Select candidate at INDEX and exit the minibuffer."
  (hyalo-minibuffer--log "select-candidate: index=%d" index)
  (when (minibufferp)
    (cond
     ;; Vertico: set index and exit
     ((bound-and-true-p vertico-mode)
      (when (and (fboundp 'vertico--goto)
                 (fboundp 'vertico-exit))
        (vertico--goto index)
        (vertico-exit)))
     ;; Fido/generic: insert the candidate text and exit
     (t
      (let* ((extracted (hyalo-minibuffer--extract-candidates))
             (candidates (cdr (assq 'candidates extracted)))
             (cand (and candidates
                        (< index (length candidates))
                        (elt candidates index))))
        (when cand
          (delete-minibuffer-contents)
          (insert (cdr (assq 'text cand)))
          (exit-minibuffer)))))))

;; ---------------------------------------------------------------------------
;; Abort — Swift → Emacs
;; ---------------------------------------------------------------------------

(defun hyalo-minibuffer--abort ()
  "Abort the current minibuffer session."
  (hyalo-minibuffer--log "abort")
  (when (minibufferp)
    (abort-recursive-edit)))

;; ---------------------------------------------------------------------------
;; Hooks — minibuffer lifecycle
;; ---------------------------------------------------------------------------

(defun hyalo-minibuffer--setup-hook ()
  "Called from `minibuffer-setup-hook'.  Show the Swift panel."
  (if (hyalo-minibuffer--should-skip-p)
      (hyalo-minibuffer--log "setup-hook: SKIPPED (skip condition met, depth=%d)"
                             (minibuffer-depth))
    (setq hyalo-minibuffer--session-id (1+ hyalo-minibuffer--session-id))
    (setq hyalo-minibuffer--prompt (minibuffer-prompt))
    (setq hyalo-minibuffer--active t)
    (hyalo-minibuffer--log "setup-hook: session=%d prompt=%S"
                           hyalo-minibuffer--session-id hyalo-minibuffer--prompt)
    ;; Show Swift panel
    (let* ((payload `((sessionId . ,hyalo-minibuffer--session-id)
                      (prompt . ,(or hyalo-minibuffer--prompt ""))
                      (input . "")
                      (candidates . [])
                      (selectedIndex . -1)
                      (totalCandidates . 0)))
           (json (json-encode payload)))
      (hyalo-minibuffer--log "setup-hook: calling hyalo-minibuffer-show, json-len=%d"
                             (length json))
      (when (fboundp 'hyalo-minibuffer-show)
        (hyalo-minibuffer-show json)))
    ;; Watch for input changes
    (add-hook 'post-command-hook #'hyalo-minibuffer--post-command nil t)
    ;; Schedule initial candidate push (after vertico has computed)
    (hyalo-minibuffer--schedule-update)))

(defun hyalo-minibuffer--exit-hook ()
  "Called from `minibuffer-exit-hook'.  Hide the Swift panel."
  (when hyalo-minibuffer--active
    (hyalo-minibuffer--log "exit-hook: hiding panel")
    (setq hyalo-minibuffer--active nil)
    (when hyalo-minibuffer--update-timer
      (cancel-timer hyalo-minibuffer--update-timer)
      (setq hyalo-minibuffer--update-timer nil))
    (remove-hook 'post-command-hook #'hyalo-minibuffer--post-command t)
    (when (fboundp 'hyalo-minibuffer-hide)
      (hyalo-minibuffer-hide))))

(defun hyalo-minibuffer--post-command ()
  "Called after each command in the minibuffer.  Schedule candidate update."
  (when hyalo-minibuffer--active
    (hyalo-minibuffer--schedule-update)))

;; ---------------------------------------------------------------------------
;; Display suppression — hide the Emacs-side completion UI
;; ---------------------------------------------------------------------------

;; For vertico: We suppress only the DISPLAY, not the computation.
;; vertico--exhibit computes candidates, then calls vertico--display-candidates.
;; We suppress vertico--display-candidates (the rendering part only).
(defun hyalo-minibuffer--suppress-vertico-display (&rest _args)
  "Advice to suppress vertico's candidate display when Swift panel is active.
Returns nil (for :before-while) to skip the display function."
  (not hyalo-minibuffer--active))

;; For icomplete: suppress the exhibit function that renders in the minibuffer.
;; Note: on iOS with fido-vertical-mode, we still need icomplete-exhibit to
;; compute completion-all-sorted-completions, so we only suppress the display.
(defun hyalo-minibuffer--suppress-icomplete-display (orig-fn &rest args)
  "Around advice for icomplete-exhibit.  Skip display when Swift panel active."
  (if hyalo-minibuffer--active
      ;; Still compute completions but don't display
      (let ((icomplete-mode nil))
        (apply orig-fn args))
    (apply orig-fn args)))

;; ---------------------------------------------------------------------------
;; Channel handlers (called from Swift via channel callbacks)
;; ---------------------------------------------------------------------------

(defun hyalo-channels--handle-minibuffer-input (text)
  "Handle input from the Swift minibuffer panel.
TEXT is the current content of the Swift text field."
  (hyalo-minibuffer--inject-input text))

(defun hyalo-channels--handle-minibuffer-select (index-str)
  "Handle candidate selection from the Swift minibuffer panel.
INDEX-STR is the selected candidate index as a string."
  (hyalo-minibuffer--select-candidate (string-to-number index-str)))

(defun hyalo-channels--handle-minibuffer-abort ()
  "Handle abort from the Swift minibuffer panel."
  (hyalo-minibuffer--abort))

;; ---------------------------------------------------------------------------
;; Minor mode
;; ---------------------------------------------------------------------------

;;;###autoload
(define-minor-mode hyalo-minibuffer-mode
  "Bridge Emacs minibuffer to native Swift panel."
  :global t
  :lighter nil
  (if hyalo-minibuffer-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'hyalo-minibuffer--setup-hook)
        (add-hook 'minibuffer-exit-hook #'hyalo-minibuffer--exit-hook)
        ;; Suppress vertico's candidate display (not computation)
        (when (fboundp 'vertico--display-candidates)
          (advice-add 'vertico--display-candidates :before-while
                      #'hyalo-minibuffer--suppress-vertico-display))
        ;; Suppress icomplete's display
        (when (fboundp 'icomplete-exhibit)
          (advice-add 'icomplete-exhibit :around
                      #'hyalo-minibuffer--suppress-icomplete-display)))
    (remove-hook 'minibuffer-setup-hook #'hyalo-minibuffer--setup-hook)
    (remove-hook 'minibuffer-exit-hook #'hyalo-minibuffer--exit-hook)
    (when (fboundp 'vertico--display-candidates)
      (advice-remove 'vertico--display-candidates
                     #'hyalo-minibuffer--suppress-vertico-display))
    (when (fboundp 'icomplete-exhibit)
      (advice-remove 'icomplete-exhibit
                     #'hyalo-minibuffer--suppress-icomplete-display))))

(provide 'hyalo-minibuffer)
;;; hyalo-minibuffer.el ends here
