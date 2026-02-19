;;; hyalo-source-control.el --- Source control data push -*- lexical-binding: t; -*-

;;; Commentary:
;; Pushes git changed files and commit history to the Swift navigator.
;; All data is read-only — write operations (stage/unstage/commit) stay in magit/vc.
;; Uses hook-driven debounced updates instead of polling.

;;; Code:

(require 'json)
(require 'cl-lib)

;; Debounce timer for source control updates
(defvar hyalo-source-control--timer nil
  "Debounce timer for source control updates.")

(defun hyalo-source-control--schedule-update ()
  "Schedule a source control update after 3s debounce.
Intended to be called from `after-save-hook'."
  (when hyalo-source-control--timer
    (cancel-timer hyalo-source-control--timer))
  (setq hyalo-source-control--timer
        (run-with-timer 3 nil #'hyalo-source-control--do-update)))

(defun hyalo-source-control--do-update ()
  "Perform the source control update."
  (hyalo-source-control--update-changes)
  (hyalo-source-control--update-history))

;; MARK: - Changed Files

(defun hyalo-source-control--update-changes ()
  "Push git changed files to the navigator."
  (when (fboundp 'hyalo-update-changed-files)
    (let* ((root (hyalo-source-control--project-root))
           (default-directory (or root default-directory))
           (results nil))
      (condition-case nil
          (let ((lines (process-lines "git" "status" "--porcelain")))
            (dolist (line lines)
              (when (>= (length line) 4)
                (let* ((code (substring line 0 2))
                       (file (string-trim (substring line 3)))
                       ;; Handle renames
                       (file (if (string-match " -> \\(.*\\)" file)
                                 (match-string 1 file)
                               file))
                       (full-path (expand-file-name file root))
                       (status (cond
                                ((string-match-p "M" code) "M")
                                ((string-match-p "A" code) "A")
                                ((string-match-p "D" code) "D")
                                ((string-match-p "R" code) "R")
                                ((string-match-p "\\?" code) "?")
                                (t "?")))
                       ;; Staged if first char is not space and not ?
                       (staged (and (not (string= (substring code 0 1) " "))
                                    (not (string= (substring code 0 1) "?")))))
                  (push `((id . ,full-path)
                          (fileName . ,(file-name-nondirectory file))
                          (filePath . ,full-path)
                          (status . ,status)
                          (isStaged . ,(if staged t :json-false)))
                        results)))))
        (error nil))
      (hyalo-update-changed-files
       (json-encode (vconcat (nreverse results)))))))

;; MARK: - Commit History

(defun hyalo-source-control--update-history ()
  "Push git commit history (last 50) to the navigator.
Skipped if the repo has no commits."
  (when (fboundp 'hyalo-update-commit-history)
    (let* ((root (hyalo-source-control--project-root))
           (default-directory (or root default-directory)))
      (if (and root
               (fboundp 'hyalo-status--git-has-commits-p)
               (not (hyalo-status--git-has-commits-p (expand-file-name root))))
          ;; No commits — push empty history
          (hyalo-update-commit-history (json-encode (vector)))
        ;; Has commits — fetch history
        (let ((commits nil))
          (condition-case nil
              (let ((lines (process-lines
                            "git" "log" "-50"
                            "--format=%H%n%h%n%s%n%an%n%ae%n%aI%n%D%n---")))
                (while (>= (length lines) 7)
                  (let ((full-hash (pop lines))
                        (short-hash (pop lines))
                        (message (pop lines))
                        (author (pop lines))
                        (email (pop lines))
                        (date (pop lines))
                        (decorate (pop lines)))
                    ;; Pop separator
                    (when (and lines (string= (car lines) "---"))
                      (pop lines))
                    ;; Parse refs and tag from decoration
                    (let* ((refs-raw (when (and decorate (not (string-empty-p decorate)))
                                       (split-string decorate ", " t)))
                           (tag (cl-find-if
                                 (lambda (r) (string-prefix-p "tag: " r))
                                 (or refs-raw nil)))
                           (branch-refs (cl-remove-if
                                         (lambda (r)
                                           (or (string-prefix-p "tag: " r)
                                               (string-prefix-p "HEAD" r)))
                                         (or refs-raw nil))))
                      (push `((id . ,full-hash)
                              (hash . ,short-hash)
                              (fullHash . ,full-hash)
                              (message . ,message)
                              (author . ,author)
                              (authorEmail . ,email)
                              (date . ,date)
                              (refs . ,(vconcat branch-refs))
                              (tag . ,(if tag (substring tag 5) "")))
                            commits)))))
            (error nil))
          (hyalo-update-commit-history
           (json-encode (vconcat (nreverse commits)))))))))

;; MARK: - Utility

(defun hyalo-source-control--project-root ()
  "Get the project root directory."
  (or (when-let* ((proj (project-current)))
        (if (fboundp 'project-root)
            (project-root proj)
          (car (project-roots proj))))
      (locate-dominating-file default-directory ".git")))

(provide 'hyalo-source-control)
;;; hyalo-source-control.el ends here
