;;; hyalo-minibuffer.el --- Native Swift minibuffer bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Bridges Emacs's minibuffer to a native Swift panel.
;; Emacs owns filtering/ordering (vertico, orderless, fido); Swift owns rendering.
;;
;; Flow:
;;   User types in the Emacs minibuffer
;;     → completion framework processes input
;;     → timer extracts candidates + annotations as JSON
;;     → calls hyalo-minibuffer-update (defun registered by Swift module)
;;     → Swift decodes JSON, updates MinibufferViewModel
;;     → SwiftUI re-renders the display-only panel

;;; Code:

(require 'json)

;; State

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

(defvar hyalo-minibuffer--history-mode nil
  "Non-nil when the current minibuffer session is free-text (no completion table).")

(defvar hyalo-minibuffer--face-remap-cookies nil
  "Face remap cookies for hiding minibuffer text while the Swift panel is active.
Uses face remapping instead of invisible overlay so cursor movement is unaffected.")

(defvar hyalo-minibuffer--max-candidates 50
  "Maximum number of candidates to send to Swift for rendering performance.")

(defvar hyalo-minibuffer--update-delay 0.05
  "Timer delay in seconds for candidate extraction (coalesces rapid keystrokes).
20 updates/sec is sufficient for smooth UX without overloading the Swift UI layer.")

;; Logging

(defvar hyalo-minibuffer--debug nil
  "When non-nil, log minibuffer bridge activity to *Messages*.")

(defun hyalo-minibuffer--log (fmt &rest args)
  "Log FMT with ARGS when debug is enabled."
  (when hyalo-minibuffer--debug
    (apply #'message (concat "[hyalo-minibuffer] " fmt) args)))

;; Skip conditions

(defun hyalo-minibuffer--should-skip-p ()
  "Return non-nil if we should fall through to native Emacs minibuffer."
  (or hyalo-minibuffer--inhibit
      (not (display-graphic-p))
      ;; Recursive minibuffer (depth > 2): skip for deep nesting
      (> (minibuffer-depth) 2)
      ;; Password prompts
      (bound-and-true-p read-passwd--mode)
      ;; Detect password prompt text
      (let ((prompt (minibuffer-prompt)))
        (and prompt
             (or (string-match-p "\\b[Pp]ass\\(word\\|phrase\\)\\b" prompt)
                 (string-match-p "\\bPIN\\b" prompt))))
      ;; y-or-n-p, yes-or-no-p, and map-y-or-n-p
      (and (minibuffer-prompt)
           (string-match-p "\\(y or n\\|yes or no\\|([ynYN!.,; ]\\)" (minibuffer-prompt)))
      ;; Ephemeral minibuffer with no real history variable
      (eq minibuffer-history-variable t)))

;; Text cleaning — strip invisible disambiguation chars

(defun hyalo-minibuffer--clean-text (str)
  "Strip consult tofu chars (Unicode PUA-B disambiguation markers) from STR.
Consult appends invisible U+100000..U+10FFFE chars to multi-source candidates.
`substring-no-properties' removes the `invisible' property but keeps the char."
  (replace-regexp-in-string "[\U00100000-\U0010FFFE]+" "" str))

;; Candidate extraction — framework detection

(defun hyalo-minibuffer--extract-candidates ()
  "Extract current completion candidates with annotations.
Detects vertico, fido-vertical-mode, or generic completion.
For free-text inputs (no completion table), extracts history."
  (condition-case err
      (cond
       ;; Free-text mode: filter history by current input
       (hyalo-minibuffer--history-mode
        (hyalo-minibuffer--extract-history-filtered))

       ;; Vertico (macOS desktop)
       ((bound-and-true-p vertico-mode)
        (hyalo-minibuffer--extract-vertico))

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
             (hilited (hyalo-minibuffer--hilit-candidate cand))
             (match-ranges (hyalo-minibuffer--extract-match-ranges hilited))
             (annotation (if annotate-fn
                             (or (ignore-errors (funcall annotate-fn cand)) "")
                           "")))
        (push `((text . ,(hyalo-minibuffer--clean-text
                          (substring-no-properties cand)))
                (annotation . ,(string-trim (substring-no-properties annotation)))
                (selected . ,(if (eq i index) t :json-false))
                (matchRanges . ,match-ranges))
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
        (push `((text . ,(hyalo-minibuffer--clean-text
                          (substring-no-properties cand)))
                (annotation . ,(string-trim (substring-no-properties annotation)))
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
        (push `((text . ,(hyalo-minibuffer--clean-text
                          (if (stringp cand) (substring-no-properties cand) "")))
                (annotation . ,(string-trim (substring-no-properties annotation)))
                (selected . ,(if (eq i 0) t :json-false)))
              result)))
    (list (cons 'candidates (vconcat (nreverse result)))
          (cons 'selectedIndex 0)
          (cons 'totalCandidates total))))

(defun hyalo-minibuffer--get-annotation-fn ()
  "Get the marginalia/completion annotation function if available."
  (condition-case err
      (let* ((input (buffer-substring-no-properties
                     (minibuffer-prompt-end) (point-max)))
             (metadata (completion-metadata
                        input
                        minibuffer-completion-table
                        minibuffer-completion-predicate))
             (cat (completion-metadata-get metadata 'category)))
        (hyalo-minibuffer--log "get-annotation-fn: cat=%s marginalia=%s"
                               cat (if (bound-and-true-p marginalia-mode) "on" "off"))
        ;; marginalia-annotators is an alist: (category annotator1 annotator2 ... builtin none)
        ;; car of the cdr gives the first (active) annotator function
        (let ((fn (or (and (bound-and-true-p marginalia-mode)
                           cat
                           (let ((entry (alist-get cat
                                                   (bound-and-true-p marginalia-annotators))))
                             (when entry
                               (let ((first (car entry)))
                                 (cond
                                  ((eq first 'none) nil)
                                  ((eq first 'builtin) nil)
                                  ((functionp first) first)
                                  (t nil))))))
                      (completion-metadata-get metadata 'annotation-function))))
          (hyalo-minibuffer--log "get-annotation-fn: resolved=%s" fn)
          fn))
    (error
     (hyalo-minibuffer--log "get-annotation-fn error: %s" (error-message-string err))
     nil)))

;; Match highlighting — extract face ranges for Swift bold rendering

(defun hyalo-minibuffer--hilit-candidate (cand)
  "Return a copy of CAND with completion face properties applied.
Uses vertico's buffer-local hilit function when available, then
`completion-lazy-hilit' (Emacs 30+), otherwise returns CAND unchanged."
  (condition-case nil
      (let ((str (copy-sequence cand)))
        (cond
         ;; vertico sets `vertico--hilit' buffer-locally in the minibuffer
         ((and (bound-and-true-p vertico-mode)
               (boundp 'vertico--hilit)
               (functionp vertico--hilit))
          (funcall vertico--hilit str))
         ;; Emacs 30+ lazy hilit
         ((fboundp 'completion-lazy-hilit)
          (completion-lazy-hilit str))
         (t str)))
    (error cand)))

(defconst hyalo-minibuffer--match-faces
  '(completions-common-part
    completions-first-difference
    orderless-match-face-0
    orderless-match-face-1
    orderless-match-face-2
    orderless-match-face-3)
  "Faces that indicate a completion match region.")

(defun hyalo-minibuffer--extract-match-ranges (str)
  "Return a vector of [start end] vectors for match-face regions in STR."
  (let ((len (length str))
        (pos 0)
        ranges)
    (while (< pos len)
      (let* ((face (get-text-property pos 'face str))
             (faces (cond ((listp face) face)
                          ((symbolp face) (list face))
                          (t nil)))
             (is-match (seq-some (lambda (f)
                                   (memq f hyalo-minibuffer--match-faces))
                                 faces))
             (next (or (next-single-property-change pos 'face str) len)))
        (when is-match
          (push (vector pos next) ranges))
        (setq pos next)))
    (vconcat (nreverse ranges))))

;; History extraction — for free-text minibuffers (no completion table)

(defvar hyalo-minibuffer--max-history-items 30
  "Maximum number of history items to send as candidates.")

(defun hyalo-minibuffer--extract-history ()
  "Extract history items from `minibuffer-history-variable' as candidates.
Used when there is no completion table (e.g. `read-shell-command')."
  (let* ((hist-var (and (boundp 'minibuffer-history-variable)
                        minibuffer-history-variable))
         (hist (and hist-var
                    (symbolp hist-var)
                    (not (eq hist-var t))
                    (boundp hist-var)
                    (symbol-value hist-var)))
         (items (if (listp hist)
                    (seq-take (seq-uniq (seq-filter #'stringp hist))
                              hyalo-minibuffer--max-history-items)
                  nil))
         (total (length items))
         (result nil))
    (hyalo-minibuffer--log "extract-history: var=%s items=%d" hist-var total)
    (dotimes (i total)
      (push `((text . ,(substring-no-properties (nth i items)))
              (annotation . "")
              (selected . ,(if (eq i 0) t :json-false)))
            result))
    (list (cons 'candidates (vconcat (nreverse result)))
          (cons 'selectedIndex (if (> total 0) 0 -1))
          (cons 'totalCandidates total))))

(defun hyalo-minibuffer--extract-history-filtered ()
  "Extract history items filtered by current minibuffer input."
  (let* ((input (minibuffer-contents-no-properties))
         (hist-var (and (boundp 'minibuffer-history-variable)
                        minibuffer-history-variable))
         (hist (and hist-var
                    (symbolp hist-var)
                    (not (eq hist-var t))
                    (boundp hist-var)
                    (symbol-value hist-var)))
         (all-items (if (listp hist)
                        (seq-uniq (seq-filter #'stringp hist))
                      nil))
         (filtered (if (string-empty-p input)
                       all-items
                     (seq-filter (lambda (item)
                                   (string-match-p (regexp-quote input) item))
                                 all-items)))
         (items (seq-take filtered hyalo-minibuffer--max-history-items))
         (total (length items))
         (result nil))
    (hyalo-minibuffer--log "extract-history-filtered: input=%S matches=%d" input total)
    (dotimes (i total)
      (push `((text . ,(substring-no-properties (nth i items)))
              (annotation . "")
              (selected . ,(if (eq i 0) t :json-false)))
            result))
    (list (cons 'candidates (vconcat (nreverse result)))
          (cons 'selectedIndex (if (> total 0) 0 -1))
          (cons 'totalCandidates total))))

;; Update timer — push candidates to Swift

(defun hyalo-minibuffer--schedule-update ()
  "Schedule a candidate update push to Swift.
If an update is already scheduled, do nothing to allow it to fire
rather than starving it through repeated cancellations."
  (unless hyalo-minibuffer--update-timer
    (setq hyalo-minibuffer--update-timer
          (run-with-timer hyalo-minibuffer--update-delay nil
                          #'hyalo-minibuffer--push-update))))

(defun hyalo-minibuffer--push-update ()
  "Extract candidates and push them to Swift."
  (setq hyalo-minibuffer--update-timer nil)
  (when (and hyalo-minibuffer--active (minibufferp))
    (condition-case err
        (let* ((t0 (float-time))
               (session hyalo-minibuffer--session-id)
               (extracted (hyalo-minibuffer--extract-candidates))
               (t1 (float-time))
               (input (minibuffer-contents-no-properties))
               (cursor-pos (- (point) (minibuffer-prompt-end)))
               (payload `((sessionId . ,session)
                          (prompt . ,(or hyalo-minibuffer--prompt ""))
                          (input . ,input)
                          (cursorPosition . ,cursor-pos)
                          ,@extracted))
               (json (json-encode payload))
               (t2 (float-time)))
          (when (and extracted (fboundp 'hyalo-minibuffer-update))
            (hyalo-minibuffer-update json)))
      (error
       (hyalo-minibuffer--log "push-update error: %s" (error-message-string err))))))

;; Selection — Swift → Emacs (click-to-select from overlay)

(defun hyalo-minibuffer--select-candidate (index)
  "Select candidate at INDEX and exit the minibuffer.
INDEX of -1 means confirm current input as-is (free-text mode)."
  (hyalo-minibuffer--log "select-candidate: index=%d historyMode=%s" index hyalo-minibuffer--history-mode)
  (when (minibufferp)
    (cond
     ;; Free-text confirm: index -1 means submit whatever is in the minibuffer
     ((= index -1)
      (hyalo-minibuffer--log "select-candidate: submitting=%S" (minibuffer-contents-no-properties))
      (exit-minibuffer))
     ;; History mode: insert selected history item and exit
     (hyalo-minibuffer--history-mode
      (let* ((extracted (hyalo-minibuffer--extract-history))
             (candidates (cdr (assq 'candidates extracted)))
             (cand (and candidates
                        (< index (length candidates))
                        (elt candidates index))))
        (when cand
          (delete-minibuffer-contents)
          (insert (cdr (assq 'text cand)))
          (exit-minibuffer))))
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

;; Abort — Swift → Emacs

(defun hyalo-minibuffer--abort ()
  "Abort the current minibuffer session."
  (hyalo-minibuffer--log "abort")
  (when (minibufferp)
    (abort-recursive-edit)))

;; Hooks — minibuffer lifecycle

(defun hyalo-minibuffer--setup-hook ()
  "Called from `minibuffer-setup-hook'.  Show the Swift panel."
  (if (hyalo-minibuffer--should-skip-p)
      (hyalo-minibuffer--log "setup-hook: SKIPPED (skip condition met, depth=%d, inhibit=%s)"
                             (minibuffer-depth) hyalo-minibuffer--inhibit)
    (setq hyalo-minibuffer--session-id (1+ hyalo-minibuffer--session-id))
    (setq hyalo-minibuffer--prompt (minibuffer-prompt))
    (setq hyalo-minibuffer--active t)
    ;; Detect free-text mode: no completion table means read-string/read-shell-command
    (setq hyalo-minibuffer--history-mode (null minibuffer-completion-table))
    ;; Read initial input (e.g. compile-command default inserted before setup-hook)
    (let* ((initial-input (minibuffer-contents-no-properties))
           (history-data (when hyalo-minibuffer--history-mode
                           (hyalo-minibuffer--extract-history)))
           (initial-candidates (if history-data
                                   (cdr (assq 'candidates history-data))
                                 []))
           (initial-selected (if history-data
                                 (cdr (assq 'selectedIndex history-data))
                               -1))
           (initial-total (if history-data
                              (cdr (assq 'totalCandidates history-data))
                            0)))
      (hyalo-minibuffer--log "setup-hook: session=%d depth=%d prompt=%S input=%S historyMode=%s"
                             hyalo-minibuffer--session-id
                             (minibuffer-depth)
                             hyalo-minibuffer--prompt
                             initial-input
                             hyalo-minibuffer--history-mode)
      ;; Show Swift panel
      (let* ((payload `((sessionId . ,hyalo-minibuffer--session-id)
                        (prompt . ,(or hyalo-minibuffer--prompt ""))
                        (input . ,initial-input)
                        (cursorPosition . ,(- (point) (minibuffer-prompt-end)))
                        (historyMode . ,(if hyalo-minibuffer--history-mode t :json-false))
                        (candidates . ,initial-candidates)
                        (selectedIndex . ,initial-selected)
                        (totalCandidates . ,initial-total)))
             (json (json-encode payload)))
        (hyalo-minibuffer--log "setup-hook: calling hyalo-minibuffer-show, json-len=%d"
                               (length json))
        (when (fboundp 'hyalo-minibuffer-show)
          (hyalo-minibuffer-show json))))
    ;; Hide the Emacs minibuffer text visually — the Swift overlay mirrors it.
    ;; Uses face remapping (foreground = background) instead of `invisible' overlay
    ;; so that cursor movement commands (C-b, C-a, M-b, arrows) work normally.
    (let ((bg (or (face-background 'default nil t) "black")))
      (setq hyalo-minibuffer--face-remap-cookies
            (list (face-remap-add-relative 'default :foreground bg)
                  (face-remap-add-relative 'minibuffer-prompt :foreground bg))))
    ;; Watch for input changes
    (add-hook 'post-command-hook #'hyalo-minibuffer--post-command nil t)
    ;; Schedule initial candidate push (after vertico has computed)
    (hyalo-minibuffer--schedule-update)))

(defun hyalo-minibuffer--exit-hook ()
  "Called from `minibuffer-exit-hook'.  Hide the Swift panel."
  (hyalo-minibuffer--log "exit-hook: depth=%d active=%s"
                         (minibuffer-depth) hyalo-minibuffer--active)
  (when hyalo-minibuffer--active
    (hyalo-minibuffer--log "exit-hook: hiding panel")
    (setq hyalo-minibuffer--active nil)
    (setq hyalo-minibuffer--history-mode nil)
    ;; Remove face remapping that hid minibuffer text
    (dolist (cookie hyalo-minibuffer--face-remap-cookies)
      (face-remap-remove-relative cookie))
    (setq hyalo-minibuffer--face-remap-cookies nil)
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

;; Display suppression — hide the Emacs-side completion UI

;; For vertico: We suppress only the DISPLAY, not the computation.
;; vertico--exhibit computes candidates, then calls vertico--display-candidates.
;; We suppress vertico--display-candidates (the rendering part only).
(defun hyalo-minibuffer--suppress-vertico-display (&rest _args)
  "Advice to suppress vertico's candidate display when Swift panel is active.
Returns nil (for :before-while) to skip the display function."
  (not hyalo-minibuffer--active))

;; Channel handlers (called from Swift via channel callbacks)

(defun hyalo-channels--handle-minibuffer-select (index-str)
  "Handle candidate selection from the Swift minibuffer panel.
INDEX-STR is the selected candidate index as a string."
  (hyalo-minibuffer--select-candidate (string-to-number index-str)))

(defun hyalo-channels--handle-minibuffer-abort ()
  "Handle abort from the Swift minibuffer panel."
  (hyalo-minibuffer--abort))

;; Minor mode

;;;###autoload
(define-minor-mode hyalo-minibuffer-mode
  "Bridge Emacs minibuffer to native Swift panel."
  :global t
  :lighter nil
  (if hyalo-minibuffer-mode
      (if (display-graphic-p)
          (progn
            (add-hook 'minibuffer-setup-hook #'hyalo-minibuffer--setup-hook)
            (add-hook 'minibuffer-exit-hook #'hyalo-minibuffer--exit-hook)
            ;; Suppress vertico's candidate display (not computation)
            (when (fboundp 'vertico--display-candidates)
              (advice-add 'vertico--display-candidates :before-while
                          #'hyalo-minibuffer--suppress-vertico-display)))
        (setq hyalo-minibuffer-mode nil)
        (message "hyalo-minibuffer-mode requires a graphical display"))
    (remove-hook 'minibuffer-setup-hook #'hyalo-minibuffer--setup-hook)
    (remove-hook 'minibuffer-exit-hook #'hyalo-minibuffer--exit-hook)
    (when (fboundp 'vertico--display-candidates)
      (advice-remove 'vertico--display-candidates
                      #'hyalo-minibuffer--suppress-vertico-display))))

(provide 'hyalo-minibuffer)
;;; hyalo-minibuffer.el ends here
