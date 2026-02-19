;;; hyalo-status.el --- Status bar updates via hooks -*- lexical-binding: t; -*-

;;; Commentary:
;; Pushes cursor position, mode, encoding, etc. to the Swift status bar
;; using Emacs hooks and debounced timers instead of polling.
;;
;; Performance architecture:
;; - Buffer switch hook (`window-buffer-change-functions') does ZERO subprocess work.
;;   It pushes only in-process state (navigator selection, cached project root)
;;   and schedules deferred work via timers.
;; - All git subprocess calls (branch, file status, history) are deferred behind
;;   debounce timers so the Emacs event loop returns immediately.
;; - Project root is cached per directory to avoid repeated `locate-dominating-file'.
;; - Branch list is cached per project root; only the current branch name is refreshed.

;;; Code:

(require 'json)

;; MARK: - Debounce Timers

(defvar hyalo-status--cursor-timer nil
  "Debounce timer for cursor/status bar updates.")
(defvar hyalo-status--tabs-timer nil
  "Debounce timer for editor tab updates.")
(defvar hyalo-status--branch-timer nil
  "Debounce timer for branch info updates.")
(defvar hyalo-status--navigator-timer nil
  "Debounce timer for navigator refresh on buffer switch.")
(defvar hyalo-status--file-info-timer nil
  "Debounce timer for file info + git history updates.")

;; MARK: - Caches

(defvar hyalo-status--last-tab-state nil
  "Last serialized tab state to avoid redundant pushes.")

(defvar hyalo-status--last-file-info-path nil
  "Last file path for which file info was pushed.")

(defvar hyalo-status--project-root-cache (make-hash-table :test 'equal)
  "Cache: directory -> git project root.
Avoids repeated `locate-dominating-file' walks up the tree.")

(defvar hyalo-status--last-project-root nil
  "Last known project root, used to detect project switches.")

(defvar hyalo-status--branch-cache (make-hash-table :test 'equal)
  "Cache: project-root -> (current-branch . branch-list-vector).
Branch list rarely changes; invalidated on save and project switch.")

(defvar hyalo-status--has-commits-cache (make-hash-table :test 'equal)
  "Cache: project-root -> t/nil for whether the repo has any commits.
Invalidated when branch cache is invalidated (on save, magit refresh).")

;; MARK: - Hook-based Setup/Teardown

(defun hyalo-status-setup ()
  "Register hooks for event-driven status updates."
  (hyalo-status-teardown)
  ;; Status bar (line/col/mode): debounced on cursor movement
  (add-hook 'post-command-hook #'hyalo-status--schedule-cursor-update)
  ;; Editor tabs: on buffer list changes and saves
  (add-hook 'buffer-list-update-hook #'hyalo-status--schedule-tabs-update)
  (add-hook 'after-save-hook #'hyalo-status--schedule-tabs-update)
  (add-hook 'first-change-hook #'hyalo-status--schedule-tabs-update)
  ;; Buffer switch: immediate selection + deferred git work
  (add-hook 'window-buffer-change-functions #'hyalo-status--on-buffer-change)
  ;; Project switch: advice catches project-switch-project dispatch
  (advice-add 'project-switch-project :after #'hyalo-status--on-project-switch)
  ;; Special mode hooks: dired/magit may open without triggering window-buffer-change
  (add-hook 'dired-mode-hook #'hyalo-status--on-special-buffer-enter)
  (add-hook 'magit-status-mode-hook #'hyalo-status--on-special-buffer-enter)
  (add-hook 'magit-log-mode-hook #'hyalo-status--on-special-buffer-enter)
  (add-hook 'magit-diff-mode-hook #'hyalo-status--on-special-buffer-enter)
  ;; Branch info: invalidate cache on save (commits may change branch)
  (add-hook 'after-save-hook #'hyalo-status--invalidate-branch-cache)
  (add-hook 'after-save-hook #'hyalo-status--schedule-branch-update)
  ;; Magit: refresh all navigator views after any git operation completes
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'hyalo-status--on-magit-refresh)))

(defun hyalo-status-teardown ()
  "Remove all hooks and cancel pending timers."
  (remove-hook 'post-command-hook #'hyalo-status--schedule-cursor-update)
  (remove-hook 'buffer-list-update-hook #'hyalo-status--schedule-tabs-update)
  (remove-hook 'after-save-hook #'hyalo-status--schedule-tabs-update)
  (remove-hook 'first-change-hook #'hyalo-status--schedule-tabs-update)
  (remove-hook 'window-buffer-change-functions #'hyalo-status--on-buffer-change)
  (advice-remove 'project-switch-project #'hyalo-status--on-project-switch)
  (remove-hook 'dired-mode-hook #'hyalo-status--on-special-buffer-enter)
  (remove-hook 'magit-status-mode-hook #'hyalo-status--on-special-buffer-enter)
  (remove-hook 'magit-log-mode-hook #'hyalo-status--on-special-buffer-enter)
  (remove-hook 'magit-diff-mode-hook #'hyalo-status--on-special-buffer-enter)
  (remove-hook 'after-save-hook #'hyalo-status--invalidate-branch-cache)
  (remove-hook 'after-save-hook #'hyalo-status--schedule-branch-update)
  (remove-hook 'magit-post-refresh-hook #'hyalo-status--on-magit-refresh)
  (dolist (sym '(hyalo-status--cursor-timer
                 hyalo-status--tabs-timer
                 hyalo-status--branch-timer
                 hyalo-status--navigator-timer
                 hyalo-status--file-info-timer))
    (when (symbol-value sym)
      (cancel-timer (symbol-value sym))
      (set sym nil))))

;; MARK: - Debounced Dispatchers

(defun hyalo-status--schedule-cursor-update ()
  "Schedule a status bar update after 50ms debounce."
  (when hyalo-status--cursor-timer
    (cancel-timer hyalo-status--cursor-timer))
  (setq hyalo-status--cursor-timer
        (run-with-timer 0.05 nil #'hyalo-status--push-cursor)))

(defun hyalo-status--schedule-tabs-update ()
  "Schedule an editor tabs update after 200ms debounce."
  (when hyalo-status--tabs-timer
    (cancel-timer hyalo-status--tabs-timer))
  (setq hyalo-status--tabs-timer
        (run-with-timer 0.2 nil #'hyalo-status--push-editor-tabs)))

(defun hyalo-status--schedule-branch-update ()
  "Schedule a branch info update after 1s debounce."
  (when hyalo-status--branch-timer
    (cancel-timer hyalo-status--branch-timer))
  (setq hyalo-status--branch-timer
        (run-with-timer 1.0 nil #'hyalo-status--push-branch-info)))

(defun hyalo-status--schedule-file-info-update ()
  "Schedule a file info + git history update after 100ms debounce.
Coalesces rapid tab switches into a single git subprocess batch."
  (when hyalo-status--file-info-timer
    (cancel-timer hyalo-status--file-info-timer))
  (setq hyalo-status--file-info-timer
        (run-with-timer 0.1 nil #'hyalo-status--push-file-info)))

(defun hyalo-status--schedule-navigator-refresh ()
  "Schedule a navigator file tree + source control refresh after 500ms debounce."
  (when hyalo-status--navigator-timer
    (cancel-timer hyalo-status--navigator-timer))
  (setq hyalo-status--navigator-timer
        (run-with-timer 0.5 nil #'hyalo-status--do-navigator-refresh)))

;; MARK: - Buffer Change Handler (ZERO subprocess cost)

;; MARK: - Centralized Active Buffer State Push

(defun hyalo-push-active-buffer-state ()
  "Push complete state for active buffer to all Swift UI components.
Centralizes buffer/file/tab updates in one place to ensure consistency.
Called from on-buffer-change, buffer list selection, and file navigator."
  (let ((buf-name (buffer-name))
        (file-path (buffer-file-name)))
    ;; Update buffer list selection
    (when (fboundp 'hyalo-navigator-set-active-buffer)
      (hyalo-navigator-set-active-buffer buf-name))
    ;; Update file navigator (if visiting a file)
    (when (and file-path (fboundp 'hyalo-navigator-set-active-file))
      (hyalo-navigator-set-active-file file-path))
    ;; Update editor tab bar
    (when (fboundp 'hyalo-select-editor-tab)
      (hyalo-select-editor-tab buf-name))))

(defun hyalo-status--on-buffer-change (_frame)
  "Handle buffer switch in FRAME.  Zero subprocess cost.
Called from `window-buffer-change-functions'.  Performs only in-process
state updates (navigator selection, cached project root lookup) and
schedules all git subprocess work via debounce timers.

Internal buffers (space-prefixed or *...*) are skipped for navigator
selection to avoid transient minibuf/elog noise clobbering the UI."
  (let ((buf-name (buffer-name))
        (file-path (buffer-file-name)))
    ;; Skip internal buffers (space prefix or *...*) for navigator selection
    (unless (or (string-prefix-p " " buf-name)
                (string-prefix-p "*" buf-name))
      ;; Phase 1: Immediate — push all UI state in one place
      (hyalo-push-active-buffer-state))
    ;; Phase 2: Cached project root lookup (hash table, no filesystem walk)
    (let ((old-root hyalo-status--last-project-root))
      (hyalo-status--update-project-root-cached)
      ;; Phase 3: Deferred — schedule git subprocess work
      (hyalo-status--schedule-branch-update)
      (hyalo-status--schedule-file-info-update)
      ;; On project switch: push project name and root immediately (zero cost),
      ;; then refresh navigator buffer list + source control via timers
      (when (not (equal old-root hyalo-status--last-project-root))
        ;; Immediate project name push — toolbar updates right away
        (when-let* ((root hyalo-status--last-project-root)
                    (name (file-name-nondirectory (directory-file-name root))))
          (when (fboundp 'hyalo-set-project-name)
            (hyalo-set-project-name name))
          ;; Immediate project root push — file tree rebuilds right away
          (when (fboundp 'hyalo-navigator-set-project-root)
            (hyalo-navigator-set-project-root (expand-file-name root))))
        ;; Invalidate branch cache for the new project root
        (when hyalo-status--last-project-root
          (remhash hyalo-status--last-project-root hyalo-status--branch-cache))
        (hyalo-status--schedule-navigator-refresh)
        (when (fboundp 'hyalo-source-control--schedule-update)
          (hyalo-source-control--schedule-update))))))

(defun hyalo-status--on-project-switch (&rest _)
  "Handle `project-switch-project' dispatch.
Forces a buffer change pass so project root, branch, and navigator update.
Runs as :after advice on `project-switch-project'."
  (hyalo-status--on-buffer-change nil))

(defun hyalo-status--on-special-buffer-enter ()
  "Handle entry into a special buffer (dired, magit, etc.).
These buffers have no `buffer-file-name' but carry a meaningful
`default-directory' that should trigger project root resolution."
  (hyalo-status--on-buffer-change nil))

(defun hyalo-status--on-magit-refresh ()
  "Handle magit buffer refresh after a git operation.
Runs from `magit-post-refresh-hook' which fires after every git
operation (commit, push, pull, fetch, stage, unstage, rebase, etc.).
Invalidates caches and schedules deferred updates for all navigator
views: branch info, source control changes, file tree, and file info."
  (hyalo-status--invalidate-branch-cache)
  ;; Reset file info cache so the current file's git status re-fetches
  (setq hyalo-status--last-file-info-path nil)
  ;; Schedule all deferred updates
  (hyalo-status--schedule-branch-update)
  (hyalo-status--schedule-file-info-update)
  (hyalo-status--schedule-navigator-refresh)
  (when (fboundp 'hyalo-source-control--schedule-update)
    (hyalo-source-control--schedule-update)))

;; MARK: - Cached Project Root

(defun hyalo-status--update-project-root-cached ()
  "Update `default-directory' and project root from cache.
On cache miss, falls back to `locate-dominating-file' and stores the result.
Uses `buffer-file-name' directory when available, otherwise `default-directory'.
This ensures special buffers (dired, magit) also trigger project root switches."
  (let* ((dir (or (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  (and default-directory
                       (expand-file-name default-directory))))
         (cached (when dir (gethash dir hyalo-status--project-root-cache))))
    (when dir
      (if cached
          ;; Cache hit: use stored root (may be :none if not in a git repo)
          (unless (eq cached :none)
            (setq default-directory cached)
            (setq hyalo-status--last-project-root cached))
        ;; Cache miss: walk filesystem once, store result
        (let ((git-root (locate-dominating-file dir ".git")))
          (if git-root
              (let ((expanded (expand-file-name git-root)))
                (puthash dir expanded hyalo-status--project-root-cache)
                (setq default-directory expanded)
                (setq hyalo-status--last-project-root expanded))
            (puthash dir :none hyalo-status--project-root-cache)))))))

(defun hyalo-status--invalidate-branch-cache ()
  "Invalidate branch and has-commits caches for the current project root.
Called on save — a commit or branch operation may have changed the branch."
  (when hyalo-status--last-project-root
    (remhash hyalo-status--last-project-root hyalo-status--branch-cache)
    (remhash hyalo-status--last-project-root hyalo-status--has-commits-cache)))

(defun hyalo-status--do-navigator-refresh ()
  "Refresh the navigator file tree and source control data.
Pushes the project root to Swift (which rebuilds the tree) and
refreshes the buffer list."
  (when (fboundp 'hyalo-navigator-refresh)
    (hyalo-navigator-refresh))
  (when (fboundp 'hyalo-source-control--do-update)
    (hyalo-source-control--do-update)))

;; MARK: - Push Functions

(defun hyalo-status--push-cursor ()
  "Push current cursor status to the Swift status bar."
  (when (fboundp 'hyalo-status-update)
    (condition-case nil
        (hyalo-status-update
         (line-number-at-pos)
         (current-column)
         (format-mode-line mode-name)
         (symbol-name buffer-file-coding-system)
         (hyalo-status--line-ending)
         (hyalo-status--indent-style)
         (hyalo-status--indent-width)
         (hyalo-status--file-type)
         (hyalo-status--file-size)
         (hyalo-status--minor-modes-json)
         (hyalo-status--modeline-lhs)
         (hyalo-status--modeline-rhs))
      (error nil))))

(defun hyalo-status--push-editor-tabs ()
  "Push the current buffer list as editor tabs to Swift.
Only pushes when the tab set or active tab changes."
  (when (fboundp 'hyalo-update-editor-tabs)
    (condition-case nil
        (let* ((current-name (buffer-name))
               (buffers (cl-remove-if
                         (lambda (b)
                           (let ((name (buffer-name b)))
                             (or (string-prefix-p " " name)
                                 (string-prefix-p "*" name))))
                         (buffer-list)))
               (tab-data
                (mapcar
                 (lambda (b)
                   (let* ((name (buffer-name b))
                          (file (buffer-file-name b))
                          (modified (buffer-modified-p b))
                          (icon (cond
                                 ((string-suffix-p ".swift" (or name "")) "swift")
                                 ((string-suffix-p ".el" (or name "")) "doc.text")
                                 ((string-suffix-p ".md" (or name "")) "doc.plaintext")
                                 ((string-suffix-p ".json" (or name "")) "curlybraces")
                                 ((string-suffix-p ".html" (or name ""))
                                  "chevron.left.forwardslash.chevron.right")
                                 (t "doc"))))
                     `((id . ,name)
                       (name . ,name)
                       (icon . ,icon)
                       (isModified . ,(if modified t :json-false))
                       (isTemporary . :json-false)
                       (filePath . ,file))))
                 buffers))
               (state-key (format "%s|%s" current-name
                                  (mapconcat (lambda (b) (buffer-name b)) buffers ","))))
          (unless (equal state-key hyalo-status--last-tab-state)
            (setq hyalo-status--last-tab-state state-key)
            (hyalo-update-editor-tabs (json-encode (vconcat tab-data)))
            (when (fboundp 'hyalo-select-editor-tab)
              (hyalo-select-editor-tab current-name))))
      (error nil))))

(defun hyalo-status--push-branch-info ()
  "Push git branch info and project name to the toolbar.
Uses cached branch list; only refreshes current branch name."
  (when (fboundp 'hyalo-update-branch-info)
    (condition-case nil
        (let* ((root (or hyalo-status--last-project-root
                         (hyalo-status--project-root)))
               (project-name (when root (file-name-nondirectory
                                         (directory-file-name root)))))
          ;; Push project name
          (when (and project-name (fboundp 'hyalo-set-project-name))
            (hyalo-set-project-name project-name))
          ;; Current branch: always fresh (single fast subprocess)
          (when root
            (let* ((branch (hyalo-status--git-branch root))
                   (cached (gethash root hyalo-status--branch-cache))
                   (branches (if cached
                                 (cdr cached)
                               ;; Cache miss: fetch full branch list once
                               (let ((bl (hyalo-status--git-branches root)))
                                 (puthash root (cons branch bl) hyalo-status--branch-cache)
                                 bl))))
              ;; Update cached current branch
              (puthash root (cons branch branches) hyalo-status--branch-cache)
              (when branch
                (hyalo-update-branch-info
                 (json-encode `((currentBranch . ,branch)
                                (branches . ,(or branches (vector))))))))))
      (error nil))))

(defun hyalo-status--project-root ()
  "Get the project root directory."
  (or (when-let* ((proj (project-current)))
        (if (fboundp 'project-root)
            (project-root proj)
          (car (project-roots proj))))
      (locate-dominating-file default-directory ".git")))

(defun hyalo-status--git-has-commits-p (root)
  "Return non-nil if the git repo at ROOT has at least one commit.
Uses a cache to avoid repeated subprocess calls.  Returns nil for
repos with `git init` but no initial commit."
  (let ((cached (gethash root hyalo-status--has-commits-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((result (let ((default-directory root))
                      (condition-case nil
                          (progn (ignore (process-lines "git" "rev-parse" "--verify" "HEAD"))
                                 t)
                        (error nil)))))
        (puthash root result hyalo-status--has-commits-cache)
        result))))

(defun hyalo-status--git-branch (root)
  "Get current git branch for project at ROOT.
Returns empty string if the repo has no commits."
  (let ((default-directory root))
    (if (not (hyalo-status--git-has-commits-p root))
        ;; No commits: try symbolic-ref for the configured default branch name
        (condition-case nil
            (car (process-lines "git" "symbolic-ref" "--short" "HEAD"))
          (error ""))
      (condition-case nil
          (car (process-lines "git" "rev-parse" "--abbrev-ref" "HEAD"))
        (error "")))))

(defun hyalo-status--git-branches (root)
  "Get list of git branches for project at ROOT."
  (let ((default-directory root))
    (condition-case nil
        (let ((branches (process-lines "git" "branch" "--format=%(refname:short)")))
          (when branches (vconcat branches)))
      (error nil))))

(defun hyalo-status--push-file-info ()
  "Push file info for the current buffer to the inspector.
Spawns git subprocesses for file status, last commit, and history."
  (when (and (fboundp 'hyalo-update-file-info)
             buffer-file-name
             (not (equal buffer-file-name hyalo-status--last-file-info-path)))
    (setq hyalo-status--last-file-info-path buffer-file-name)
    (condition-case nil
        (let* ((file buffer-file-name)
               (attrs (file-attributes file))
               (name (file-name-nondirectory file))
               (ext (or (file-name-extension file) ""))
               (size (if attrs
                         (let ((s (file-attribute-size attrs)))
                           (cond
                            ((null s) "—")
                            ((< s 1024) (format "%d B" s))
                            ((< s (* 1024 1024)) (format "%.1f KB" (/ s 1024.0)))
                            (t (format "%.1f MB" (/ s (* 1024.0 1024.0))))))
                       "—"))
               (created (if attrs
                            (format-time-string "%Y-%m-%d %H:%M"
                                                (file-attribute-access-time attrs))
                          "—"))
               (modified (if attrs
                             (format-time-string "%Y-%m-%d %H:%M"
                                                 (file-attribute-modification-time attrs))
                           "—"))
               (perms (if attrs (file-attribute-modes attrs) "—"))
               (encoding (symbol-name buffer-file-coding-system))
               (line-endings (hyalo-status--line-ending))
               (indent-style (hyalo-status--indent-style))
               (indent-width (hyalo-status--indent-width))
               (git-status (hyalo-status--git-file-status file))
               (last-commit (hyalo-status--git-last-commit file)))
          (hyalo-update-file-info
           (json-encode
            `((name . ,name)
              (type . ,ext)
              (path . ,file)
              (size . ,size)
              (created . ,created)
              (modified . ,modified)
              (permissions . ,perms)
              (encoding . ,encoding)
              (lineEndings . ,line-endings)
              (indentStyle . ,indent-style)
              (indentWidth . ,indent-width)
              (gitStatus . ,git-status)
              (lastCommit . ,last-commit))))
          ;; Also push git history for this file
          (hyalo-status--push-git-history file))
      (error nil))))

(defun hyalo-status--git-file-status (file)
  "Get git status for FILE."
  (let ((default-directory (file-name-directory file)))
    (condition-case nil
        (let ((output (car (process-lines
                            "git" "status" "--porcelain" "--"
                            (file-name-nondirectory file)))))
          (if (or (null output) (string-empty-p output))
              "Clean"
            (let ((code (substring output 0 2)))
              (cond
               ((string-match-p "M" code) "Modified")
               ((string-match-p "A" code) "Added")
               ((string-match-p "D" code) "Deleted")
               ((string-match-p "\\?" code) "Untracked")
               (t code)))))
      (error "Unknown"))))

(defun hyalo-status--git-last-commit (file)
  "Get last git commit message for FILE.
Returns nil if the repo has no commits."
  (let* ((default-directory (file-name-directory file))
         (root (locate-dominating-file default-directory ".git")))
    (when (and root (hyalo-status--git-has-commits-p (expand-file-name root)))
      (condition-case nil
          (let ((output (car (process-lines
                              "git" "log" "-1" "--format=%s" "--"
                              (file-name-nondirectory file)))))
            (if (or (null output) (string-empty-p output)) nil output))
        (error nil)))))

(defun hyalo-status--push-git-history (file)
  "Push git commit history for FILE to the inspector.
Enriched with short hash, author email, refs, and tags.
Skipped if the repo has no commits."
  (when (fboundp 'hyalo-update-git-history)
    (let* ((dir (file-name-directory file))
           (root (locate-dominating-file dir ".git")))
      (if (and root (not (hyalo-status--git-has-commits-p (expand-file-name root))))
          ;; No commits — push empty history
          (hyalo-update-git-history (json-encode (vector)))
        (condition-case nil
            (let* ((default-directory (file-name-directory file))
                   (lines (process-lines
                           "git" "log" "-20"
                           "--format=%H%n%h%n%s%n%an%n%ae%n%aI%n%D%n---"
                           "--" (file-name-nondirectory file)))
                   (commits (list)))
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
                    (push `((hash . ,short-hash)
                            (fullHash . ,full-hash)
                            (message . ,message)
                            (author . ,author)
                            (authorEmail . ,email)
                            (date . ,date)
                            (refs . ,(vconcat branch-refs))
                            (tag . ,(if tag (substring tag 5) "")))
                          commits))))
              (when commits
                (hyalo-update-git-history
                 (json-encode (vconcat (nreverse commits))))))
          (error nil))))))

;; MARK: - Status Bar Helpers

(defun hyalo-status--minor-modes-json ()
  "Return a JSON array of active minor mode names."
  (let ((modes nil))
    (dolist (mode minor-mode-alist)
      (when (and (symbolp (car mode))
                 (symbol-value (car mode)))
        (let ((lighter (format-mode-line (cadr mode))))
          (when (and lighter (not (string-blank-p lighter)))
            (push (string-trim lighter) modes)))))
    (json-encode (vconcat (nreverse modes)))))

(defun hyalo-status--modeline-lhs ()
  "Return the left-hand side of the mode line as a string."
  (condition-case nil
      (format-mode-line mode-line-format)
    (error "")))

(defun hyalo-status--modeline-rhs ()
  "Return the right-hand side segments of the mode line.
Extracts the portion after the align-to spacer, if any."
  (condition-case nil
      (let ((full (format-mode-line mode-line-format)))
        (if (string-match "   +" full)
            (string-trim (substring full (match-end 0)))
          ""))
    (error "")))

(defun hyalo-status--line-ending ()
  "Return the current line ending style."
  (let ((eol (coding-system-eol-type buffer-file-coding-system)))
    (cond
     ((eq eol 0) "LF")
     ((eq eol 1) "CRLF")
     ((eq eol 2) "CR")
     (t "LF"))))

(defun hyalo-status--indent-style ()
  "Return the current indentation style."
  (if indent-tabs-mode "Tabs" "Spaces"))

(defun hyalo-status--indent-width ()
  "Return the current indentation width."
  (if indent-tabs-mode tab-width
    (or (and (boundp 'c-basic-offset) c-basic-offset)
        (and (boundp 'python-indent-offset) python-indent-offset)
        tab-width)))

(defun hyalo-status--file-type ()
  "Return a file type description for the current buffer."
  (or (and buffer-file-name
           (file-name-extension buffer-file-name))
      (format-mode-line mode-name)))

(defun hyalo-status--file-size ()
  "Return the file size as a human-readable string."
  (if buffer-file-name
      (let ((size (file-attribute-size (file-attributes buffer-file-name))))
        (if size
            (cond
             ((< size 1024) (format "%d B" size))
             ((< size (* 1024 1024)) (format "%.1f KB" (/ size 1024.0)))
             (t (format "%.1f MB" (/ size (* 1024.0 1024.0)))))
          ""))
    ""))

;; MARK: - Channel Callbacks (user-initiated changes from status bar)

(defun hyalo-status--set-encoding (encoding)
  "Set buffer encoding from status bar.  ENCODING is the coding system name."
  (let ((coding (intern encoding)))
    (when (coding-system-p coding)
      (set-buffer-file-coding-system coding))))

(defun hyalo-status--set-line-ending (line-ending)
  "Set line ending from status bar.  LINE-ENDING is \"LF\", \"CRLF\", or \"CR\"."
  (let ((eol (cond
              ((string= line-ending "LF") 'unix)
              ((string= line-ending "CRLF") 'dos)
              ((string= line-ending "CR") 'mac)
              (t 'unix))))
    (set-buffer-file-coding-system
     (coding-system-change-eol-conversion buffer-file-coding-system eol))))

(defun hyalo-status--set-indent (indent-json)
  "Set indentation from status bar.  INDENT-JSON is \"style:width\"."
  (let* ((parts (split-string indent-json ":"))
         (style (car parts))
         (width (string-to-number (or (nth 1 parts) "4"))))
    (setq indent-tabs-mode (string= style "Tabs"))
    (setq tab-width width)))

(provide 'hyalo-status)
;;; hyalo-status.el ends here
