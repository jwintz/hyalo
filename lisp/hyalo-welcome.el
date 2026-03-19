;;; hyalo-welcome.el --- Welcome panel with project.el integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Shows a floating welcome panel (via Swift/KelyphosWelcomeView) when
;; Emacs launches without file arguments.  Lists known projects from
;; `project-known-project-roots' and allows opening them directly.
;;
;; The panel is shown after the IDE shell is fully visible
;; (called from hyalo-window--post-setup).  It auto-dismisses when
;; the user opens a file or switches to a real buffer.

;;; Code:

(require 'hyalo)
(require 'json)
(require 'project)

;; MARK: - Project Gathering

(defun hyalo-welcome--gather-projects ()
  "Gather known projects from project.el as a JSON string.
Returns a JSON array of {\"name\": ..., \"path\": ...} objects."
  (let ((roots (when (fboundp 'project-known-project-roots)
                 (project-known-project-roots))))
    (json-encode
     (mapcar (lambda (root)
               (let ((expanded (expand-file-name root)))
                 `((name . ,(file-name-nondirectory
                             (directory-file-name expanded)))
                   (path . ,(directory-file-name expanded)))))
             (or roots '())))))

;; MARK: - Show / Dismiss

(defvar hyalo-welcome--active nil
  "Non-nil while the welcome panel is visible and the frame is hidden.")

(defun hyalo-welcome-show ()
  "Show the welcome panel if conditions are met.
Conditions: graphical display, not batch mode, no file arguments.
The Emacs frame is kept invisible while the welcome panel is shown."
  (interactive)
  (when (and (hyalo-available-p)
             (hyalo-splash--should-display-p)
             (fboundp 'hyalo-show-welcome))
    (let ((projects-json (hyalo-welcome--gather-projects)))
      (hyalo-show-welcome projects-json))
    (setq hyalo-welcome--active t)
    ;; Keep the frame hidden — Emacs may auto-show it after startup.
    ;; Use a timer to re-hide it in case Emacs reveals it after we return.
    (make-frame-invisible nil t)
    (run-with-timer 0.1 nil #'hyalo-welcome--ensure-frame-hidden)
    (run-with-timer 0.5 nil #'hyalo-welcome--ensure-frame-hidden)
    ;; Auto-dismiss hooks
    (add-hook 'find-file-hook #'hyalo-welcome--auto-dismiss)
    (add-hook 'window-buffer-change-functions #'hyalo-welcome--on-buffer-change)))

(defun hyalo-welcome--ensure-frame-hidden ()
  "Re-hide the Emacs frame if it became visible while welcome is active."
  (when hyalo-welcome--active
    (make-frame-invisible nil t)))

(defun hyalo-welcome-dismiss ()
  "Dismiss the welcome panel and reveal the Emacs frame."
  (interactive)
  (setq hyalo-welcome--active nil)
  (when (fboundp 'hyalo-dismiss-welcome)
    (hyalo-dismiss-welcome))
  ;; Remove auto-dismiss hooks
  (remove-hook 'find-file-hook #'hyalo-welcome--auto-dismiss)
  (remove-hook 'window-buffer-change-functions #'hyalo-welcome--on-buffer-change)
  ;; Reveal the Emacs frame (it was kept hidden while welcome was shown)
  (hyalo-welcome--reveal-frame))

(defun hyalo-welcome--reveal-frame ()
  "Make the Emacs frame visible, raise it, and focus it."
  (make-frame-visible)
  (raise-frame)
  (select-frame-set-input-focus (selected-frame)))

;; MARK: - Project Selection Callback

(defun hyalo-welcome--on-project-selected (path)
  "Handle project selection from the welcome panel.
PATH is the selected project path, or \"__new_file__\" for scratch."
  (hyalo-welcome-dismiss)
  (cond
   ((string= path "__new_file__")
    (switch-to-buffer (get-buffer-create "*scratch*")))
   ((file-directory-p path)
    (if (fboundp 'project-switch-project)
        (project-switch-project path)
      (dired path)))
   (t
    (find-file path)))
  ;; Bring Emacs frame to front and focus it
  (raise-frame)
  (select-frame-set-input-focus (selected-frame)))

;; MARK: - Auto-Dismiss

(defun hyalo-welcome--auto-dismiss ()
  "Dismiss the welcome panel when a file is opened."
  (hyalo-welcome-dismiss))

(defun hyalo-welcome--on-buffer-change (_window)
  "Dismiss welcome when a real buffer appears.
Ignores minibuffer and internal buffers."
  (let ((buf (current-buffer)))
    (unless (or (minibufferp buf)
                (string-prefix-p " " (buffer-name buf))
                (string= (buffer-name buf) "*splash*")
                (string= (buffer-name buf) "*scratch*")
                (string= (buffer-name buf) "*Messages*"))
      (hyalo-welcome-dismiss))))

(provide 'hyalo-welcome)
;;; hyalo-welcome.el ends here
