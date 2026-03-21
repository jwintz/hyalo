;;; hyalo-god-mode.el --- God-mode toolbar integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Syncs the Swift toolbar god-mode segment directly from `god-local-mode'.
;; There is no separate Hyalo minor mode: when god-mode is active for the
;; current buffer, the segment is visible; when god-mode is inactive, the
;; segment is hidden.
;;
;; State sync runs from `post-command-hook' so buffer-local changes and mode
;; toggles are reflected after each command. Debug logging is emitted only
;; when visibility or state changes.

;;; Code:

(require 'hyalo)
(require 'seq)

(defgroup hyalo-god-mode nil
  "God-mode display in the Hyalo toolbar."
  :group 'hyalo
  :prefix "hyalo-god-mode-")

(defvar hyalo-god-mode--installed nil
  "Non-nil when the Hyalo god-mode sync hook is installed.")

(defvar hyalo-god-mode--last-visible nil
  "Last god-mode visibility sent to Swift.")

(defvar hyalo-god-mode--last-state nil
  "Last god-mode state string sent to Swift.")

(defvar hyalo-god-mode--translation-advice-installed nil
  "Non-nil when the god-mode translation advice is installed.")

(defun hyalo-god-mode--log (fmt &rest args)
  "Log a formatted god-mode bridge message."
  (let ((msg (apply #'format fmt args)))
    (when (fboundp 'hyalo--boot-log)
      (hyalo--boot-log (concat "god-mode: " msg)))
    (when (bound-and-true-p hyalo-debug)
      (message "[hyalo:god-mode] %s" msg))))

(defun hyalo-god-mode--compute-state ()
  "Compute the current god-mode state string for the Swift segment."
  (cond
   ((not (bound-and-true-p god-local-mode))
    "inactive")
   ((and prefix-arg (numberp prefix-arg))
    "digit-argument")
   ((and prefix-arg (listp prefix-arg))
    "universal-argument")
   ((bound-and-true-p god-literal-sequence)
    "literal")
   ((and (this-command-keys-vector)
         (> (length (this-command-keys-vector)) 0)
         (let ((last-key (aref (this-command-keys-vector)
                               (1- (length (this-command-keys-vector))))))
           (and (characterp last-key) (char-equal last-key ?g))))
    "meta")
   ((and (this-command-keys-vector)
         (> (length (this-command-keys-vector)) 0)
         (let ((last-key (aref (this-command-keys-vector)
                               (1- (length (this-command-keys-vector))))))
           (and (characterp last-key) (char-equal last-key ?G))))
    "control-meta")
   (t "control")))

(defun hyalo-god-mode--state-from-key-sequence ()
  "Derive transient god-mode state from `this-command-keys-vector'."
  (let* ((keys (this-command-keys-vector))
         (desc (and keys (> (length keys) 0) (key-description keys)))
         (tokens (and desc (split-string desc " " t))))
    (cond
     ((not (bound-and-true-p god-local-mode))
      "inactive")
     ((or (eq this-command 'digit-argument)
          (eq this-command 'negative-argument)
          (member "-" tokens)
          (seq-some (lambda (token) (string-match-p "\\`[0-9]+\\'" token)) tokens))
      "digit-argument")
     ((or (eq this-command 'universal-argument)
          (eq this-command 'universal-argument-more)
          (equal (car tokens) "u"))
      "universal-argument")
     ((member god-literal-key tokens)
      "literal")
     ((equal (car tokens) "G")
      "control-meta")
     ((equal (car tokens) "g")
      "meta")
     (t
      "control"))))

(defun hyalo-god-mode--push-state (state)
  "Push STATE to the Swift toolbar if it changed."
  (unless (equal state hyalo-god-mode--last-state)
    (setq hyalo-god-mode--last-state state)
    (hyalo-god-mode--log "state -> %s" state)
    (if (fboundp 'hyalo-update-god-mode)
        (hyalo-update-god-mode state)
      (hyalo-god-mode--log "hyalo-update-god-mode unavailable"))))

(defun hyalo-god-mode--state-from-translation (key-string)
  "Derive transient god-mode state from translated KEY-STRING."
  (cond
   ((not (bound-and-true-p god-local-mode))
    "inactive")
   ((bound-and-true-p god-literal-sequence)
    "literal")
   ((and (stringp key-string) (string-prefix-p "C-M-" key-string))
    "control-meta")
   ((and (stringp key-string) (string-prefix-p "M-" key-string))
    "meta")
   (t
    "control")))

(defun hyalo-god-mode--after-key-translation (orig-fn key key-string-so-far)
  "Mirror god-mode transient translation state into the toolbar."
  (when (and (bound-and-true-p god-local-mode)
             key-string-so-far
             (stringp key)
             (string= key god-literal-key))
    (hyalo-god-mode--push-state "literal"))
  (let ((result (funcall orig-fn key key-string-so-far)))
    (when (bound-and-true-p god-local-mode)
      (hyalo-god-mode--push-state
       (hyalo-god-mode--state-from-translation result)))
    result))

(defun hyalo-god-mode--sync ()
  "Sync god-mode visibility and state to the Swift toolbar."
  (let* ((visible (bound-and-true-p god-local-mode))
         (state (if visible
                    (hyalo-god-mode--compute-state)
                  "inactive")))
    (unless (equal visible hyalo-god-mode--last-visible)
      (setq hyalo-god-mode--last-visible visible)
      (hyalo-god-mode--log "visibility -> %s" visible)
      (if (fboundp 'hyalo-set-god-mode-visible)
          (hyalo-set-god-mode-visible visible)
        (hyalo-god-mode--log "hyalo-set-god-mode-visible unavailable")))
    (hyalo-god-mode--push-state state)))

(defun hyalo-god-mode--pre-command-sync ()
  "Sync transient god-mode state before the command executes."
  (when (bound-and-true-p god-local-mode)
    (hyalo-god-mode--push-state (hyalo-god-mode--state-from-key-sequence))))

;;;###autoload
(defun hyalo-god-mode-setup ()
  "Install the Hyalo god-mode bridge."
  (interactive)
  (unless hyalo-god-mode--installed
    (unless hyalo-god-mode--translation-advice-installed
      (advice-add 'god-key-string-after-consuming-key :around
                  #'hyalo-god-mode--after-key-translation)
      (setq hyalo-god-mode--translation-advice-installed t))
    (add-hook 'pre-command-hook #'hyalo-god-mode--pre-command-sync)
    (add-hook 'post-command-hook #'hyalo-god-mode--sync)
    (setq hyalo-god-mode--installed t)
    (hyalo-god-mode--log "bridge installed"))
  (hyalo-god-mode--sync))

;;;###autoload
(defun hyalo-god-mode-teardown ()
  "Remove the Hyalo god-mode bridge."
  (interactive)
  (when hyalo-god-mode--installed
    (when hyalo-god-mode--translation-advice-installed
      (advice-remove 'god-key-string-after-consuming-key
                     #'hyalo-god-mode--after-key-translation)
      (setq hyalo-god-mode--translation-advice-installed nil))
    (remove-hook 'pre-command-hook #'hyalo-god-mode--pre-command-sync)
    (remove-hook 'post-command-hook #'hyalo-god-mode--sync)
    (setq hyalo-god-mode--installed nil)
    (setq hyalo-god-mode--last-visible nil)
    (setq hyalo-god-mode--last-state nil)
    (hyalo-god-mode--log "bridge removed"))
  (when (fboundp 'hyalo-set-god-mode-visible)
    (hyalo-set-god-mode-visible nil))
  (when (fboundp 'hyalo-update-god-mode)
    (hyalo-update-god-mode "inactive")))

(provide 'hyalo-god-mode)
;;; hyalo-god-mode.el ends here
