;;; hyalo-channels.el --- Channel communication management -*- lexical-binding: t; -*-

;;; Commentary:
;; Manages bidirectional async channels between Swift and Emacs Lisp.

;;; Code:

(require 'hyalo-source-control nil t)

(defvar hyalo-channels--initialized nil
  "Non-nil when all channels have been set up.")

(defun hyalo-channels-setup ()
  "Open all async channels for Swift<->Emacs communication."
  (condition-case err
      (progn
        (when (fboundp 'hyalo-setup-navigator-channel)
          (hyalo-setup-navigator-channel))
        (when (fboundp 'hyalo-setup-editor-tab-channel)
          (hyalo-setup-editor-tab-channel))
        (when (fboundp 'hyalo-setup-status-channel)
          (hyalo-setup-status-channel))
        (when (fboundp 'hyalo-setup-toolbar-channel)
          (hyalo-setup-toolbar-channel))
        (when (fboundp 'hyalo-setup-command-palette-channel)
          (hyalo-setup-command-palette-channel))
        (when (fboundp 'hyalo-setup-search-channel)
          (hyalo-setup-search-channel))
        (when (fboundp 'hyalo-setup-appearance-channel)
          (hyalo-setup-appearance-channel))
        (when (fboundp 'hyalo-setup-diagnostics-channel)
          (hyalo-setup-diagnostics-channel))
        (when (fboundp 'hyalo-setup-package-channel)
          (hyalo-setup-package-channel))
        (when (fboundp 'hyalo-setup-source-control-channel)
          (hyalo-setup-source-control-channel))
        (setq hyalo-channels--initialized t)
        (message "Hyalo: All channels initialized"))
    (error (message "Hyalo: Channel setup error: %s" (error-message-string err)))))

(defun hyalo-channels-teardown ()
  "Close all channels (cleanup)."
  (setq hyalo-channels--initialized nil))

;; Channel callback handlers (called from Swift via channel)

(defun hyalo-channels--handle-branch-switch (branch)
  "Handle branch switch request from toolbar.  BRANCH is the target branch name."
  (when (and (fboundp 'magit-checkout) (require 'magit nil t))
    (magit-checkout branch)))

(defun hyalo-channels--handle-search (query)
  "Handle search request from navigator.  QUERY is the search string.
Runs ripgrep (rg) synchronously and pushes structured results as JSON.
rg respects .gitignore and skips .git, binary files by default."
  (when (and (fboundp 'hyalo-update-search-results)
             (not (string-empty-p query)))
    (let* ((root (expand-file-name
                  (or (when-let* ((proj (project-current)))
                        (if (fboundp 'project-root)
                            (project-root proj)
                          (car (project-roots proj))))
                      default-directory)))
           (default-directory root)
           (results nil)
           (id 0)
           (file-set (make-hash-table :test 'equal)))
      (hyalo-log 'search "query=%s root=%s" query root)
      (condition-case err
          (let ((buf (generate-new-buffer " *rg-search*")))
            (unwind-protect
                (let ((exit-code (call-process
                                  "rg" nil buf nil
                                  "--no-heading" "--line-number"
                                  "--column" "--max-count=500"
                                  "--glob=!.build"
                                  query root)))
                  (hyalo-log 'search "rg exit-code=%d" exit-code)
                  (when (= exit-code 0)
                    (with-current-buffer buf
                      (goto-char (point-min))
                      (let ((lines (split-string (buffer-string) "\n" t)))
                        (hyalo-log 'search "rg returned %d lines" (length lines))
                        (dolist (line lines)
                          (when (string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)" line)
                            (setq id (1+ id))
                            (let ((file (match-string 1 line)))
                              (puthash file t file-set)
                              (push `((id . ,(number-to-string id))
                                      (file . ,file)
                                      (line . ,(string-to-number (match-string 2 line)))
                                      (column . ,(string-to-number (match-string 3 line)))
                                      (text . ,(string-trim (match-string 4 line)))
                                      (matchRange . nil))
                                    results))))))))
              (kill-buffer buf)))
        (error
         (hyalo-log 'search "rg error: %s" (error-message-string err))))
      (hyalo-log 'search "pushing %d results in %d files" id (hash-table-count file-set))
      (hyalo-update-search-results
       (json-encode (vconcat (nreverse results))))
      ;; Push status counts
      (when (fboundp 'hyalo-update-search-status)
        (hyalo-update-search-status id (hash-table-count file-set))))))

(defun hyalo-channels--handle-search-navigate (location)
  "Handle search result navigation.  LOCATION is \"file:line:col\"."
  (let* ((parts (split-string location ":"))
         (file (car parts))
         (line (string-to-number (or (nth 1 parts) "1")))
         (col (string-to-number (or (nth 2 parts) "0"))))
    (hyalo-channels--fast-find-file file)
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)))

(defun hyalo-channels--fast-find-file (file-path)
  "Open FILE-PATH using decomposed file visiting.
Bypasses `find-file-noselect' overhead (~1s from file handler dispatch
and coding system probing) by manually creating the buffer, inserting
contents, setting the major mode, and running `after-find-file'.
Already-visited buffers are simply switched to."
  (let ((existing (get-file-buffer file-path)))
    (if existing
        (switch-to-buffer existing)
      (let ((buf (create-file-buffer file-path)))
        (with-current-buffer buf
          (insert-file-contents file-path t)
          (normal-mode t)
          (after-find-file nil nil t))
        (switch-to-buffer buf)))))

(defun hyalo-channels--handle-open-file (path)
  "Handle file open request from command palette.  PATH is the file path."
  (hyalo-channels--fast-find-file path)
  ;; Push state to all UI components after file opens
  (when (fboundp 'hyalo-push-active-buffer-state)
    (hyalo-push-active-buffer-state)))

(defun hyalo-channels--handle-switch-buffer (buffer-name)
  "Handle buffer switch request from navigator.  BUFFER-NAME is the target."
  (switch-to-buffer buffer-name)
  ;; Push state to all UI components after buffer switch
  (when (fboundp 'hyalo-push-active-buffer-state)
    (hyalo-push-active-buffer-state)))

(defun hyalo-channels--handle-find-file (file-path)
  "Handle file open request from navigator.  FILE-PATH is the target."
  (hyalo-channels--fast-find-file file-path)
  ;; Push state to all UI components after file opens
  (when (fboundp 'hyalo-push-active-buffer-state)
    (hyalo-push-active-buffer-state)))

(defun hyalo-channels--handle-execute-command (command)
  "Handle command execution from command palette.  COMMAND is the command name."
  (let ((sym (intern command)))
    (when (commandp sym)
      (call-interactively sym))))

(defun hyalo-channels--handle-appearance-mode (mode)
  "Handle appearance mode change from Swift panel.
MODE is \"light\", \"dark\", or \"auto\"."
  (when (featurep 'hyalo-themes)
    (pcase mode
      ("light" (hyalo-theme-sync 'light))
      ("dark"  (hyalo-theme-sync 'dark))
      ("auto"  (let ((current (if (boundp 'ns-system-appearance)
                                  ns-system-appearance
                                'dark)))
                 (hyalo-theme-sync current))))))

(defun hyalo-channels--handle-opacity-change (opacity)
  "Handle opacity slider change from Swift panel.
OPACITY is a float 0.0-1.0.  Updates the fringe alpha in
`ns-alpha-elements' to min(OPACITY + offset, 1.0)."
  (when (featurep 'hyalo-appearance)
    (hyalo-appearance--update-fringe-alpha opacity)))

(defun hyalo-channels--handle-diagnostic-navigate (location)
  "Handle diagnostic navigation from utility area.
LOCATION is \"file:line:col\"."
  (let* ((parts (split-string location ":"))
         (file (car parts))
         (line (string-to-number (or (nth 1 parts) "1")))
         (col (string-to-number (or (nth 2 parts) "0"))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)))

(defun hyalo-channels--handle-package-refresh ()
  "Handle package refresh request from toolbar popover."
  (when (featurep 'hyalo-package)
    (hyalo-package--refresh)))

(defun hyalo-channels--handle-package-upgrade-all ()
  "Handle package upgrade-all request from toolbar popover."
  (when (featurep 'hyalo-package)
    (hyalo-package--upgrade-all)))

(defun hyalo-channels--handle-package-upgrade-single (name)
  "Handle single package upgrade request.  NAME is the package name string."
  (when (featurep 'hyalo-package)
    (hyalo-package--upgrade-single name)))

(defun hyalo-channels--handle-package-list ()
  "Handle list-packages request from toolbar popover."
  (when (featurep 'hyalo-package)
    (hyalo-package--list-packages)))

(defun hyalo-channels--handle-show-commit (hash)
  "Handle commit click from navigator or inspector.
HASH is the full git commit hash.  Shows the commit via magit."
  (when (and hash (not (string-empty-p hash)))
    (if (and (fboundp 'magit-show-commit) (require 'magit nil t))
        (magit-show-commit hash)
      (message "magit not available"))))

(defun hyalo-channels--handle-show-diff (path)
  "Handle changed file click from navigator.
PATH is the absolute file path.  Opens the file and shows its
unstaged diff via magit-diff-buffer-file."
  (when (and path (not (string-empty-p path)))
    (find-file path)
    (if (and (fboundp 'magit-diff-buffer-file) (require 'magit nil t))
        (magit-diff-buffer-file)
      (message "magit not available"))))

(provide 'hyalo-channels)
;;; hyalo-channels.el ends here
