;;; hyalo-navigator.el --- Navigator data management -*- lexical-binding: t; -*-

;;; Commentary:
;; Pushes buffer list to the Swift navigator and triggers file tree rebuilds.
;; File tree construction is handled entirely in Swift via the ProjectNavigator
;; package — Emacs only provides the project root directory path.

;;; Code:

(require 'json)

;; MARK: - Buffer List Debounce

(defvar hyalo-navigator--buffer-update-timer nil
  "Timer for debouncing buffer-list-update-hook.")

(defun hyalo-navigator--on-buffer-change ()
  "Called when buffers change to refresh the navigator buffer list.
Debounced to 100ms to coalesce rapid buffer creation/deletion bursts."
  (when (and (boundp 'hyalo-channels--initialized) hyalo-channels--initialized)
    (when hyalo-navigator--buffer-update-timer
      (cancel-timer hyalo-navigator--buffer-update-timer))
    (setq hyalo-navigator--buffer-update-timer
          (run-with-timer 0.1 nil
                          (lambda ()
                            (hyalo-navigator--update-buffers)
                            (setq hyalo-navigator--buffer-update-timer nil))))))

(defun hyalo-navigator-refresh ()
  "Refresh all navigator data (buffers and file tree).
Pushes the buffer list, sets the project root (no-op when unchanged),
and explicitly rebuilds the file tree including git status badges."
  (hyalo-navigator--update-buffers)
  (hyalo-navigator--push-project-root)
  (when (fboundp 'hyalo-navigator-refresh-file-tree)
    (hyalo-navigator-refresh-file-tree)))

(defun hyalo-navigator--update-buffers ()
  "Push current buffer list to the Swift navigator."
  (when (fboundp 'hyalo-navigator-update-buffers)
    (let* ((bufs (cl-remove-if
                  (lambda (b)
                    (string-prefix-p " " (buffer-name b)))
                  (buffer-list)))
           (data (mapcar
                  (lambda (buf)
                    `((id . ,(buffer-name buf))
                      (name . ,(buffer-name buf))
                      (path . ,(or (buffer-file-name buf) ""))
                      (modified . ,(if (buffer-modified-p buf) t :json-false))
                      (icon . ,(hyalo-navigator--buffer-icon buf))))
                  bufs)))
      (hyalo-navigator-update-buffers (json-encode data)))))

(defun hyalo-navigator--buffer-icon (buf)
  "Return an SF Symbol name for BUF based on its major mode."
  (with-current-buffer buf
    (cond
     ((derived-mode-p 'prog-mode) "doc.text")
     ((derived-mode-p 'text-mode) "doc.plaintext")
     ((derived-mode-p 'dired-mode) "folder")
     ((derived-mode-p 'term-mode) "terminal")
     (t "doc"))))

;; MARK: - Project Root

(defun hyalo-navigator--push-project-root ()
  "Push the project root to Swift so it can build the file tree.
File tree construction is handled entirely in Swift."
  (when (fboundp 'hyalo-navigator-set-project-root)
    (let ((root (hyalo-navigator--get-project-root)))
      (when root
        (hyalo-navigator-set-project-root (expand-file-name root))))))

(defun hyalo-navigator--get-project-root ()
  "Get the current project root directory.
Returns nil when no project or git repository is found, rather than
falling back to `default-directory'.  Scanning arbitrary directories
(especially the home directory) would freeze the file tree builder."
  (or (when-let* ((proj (project-current)))
        (if (fboundp 'project-root)
            (project-root proj)
          (car (project-roots proj))))
      (locate-dominating-file default-directory ".git")))

;; Hook to auto-refresh on buffer changes
(add-hook 'buffer-list-update-hook #'hyalo-navigator--on-buffer-change)

(provide 'hyalo-navigator)
;;; hyalo-navigator.el ends here
