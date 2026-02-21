;;; hyalo-environment.el --- Environment detection and display -*- lexical-binding: t; -*-

;;; Commentary:
;; Detects user/host info and development environments (pixi, npm, swift,
;; bun, rust, conda, etc.) and pushes state to the SwiftUI breadcrumb.
;;
;; Uses `hyalo-status--project-root' for consistent project detection,
;; ensuring a single source of truth across all hyalo modules.
;;
;; State is pushed via hooks (no polling):
;;   - find-file-hook
;;   - window-buffer-change-functions
;;   - after-save-hook
;;   - project-switch-project-hook
;;   - hyalo-sync--push (periodic)

;;; Code:

;; -----------------------------------------------------------------------------
;; Logging
;; -----------------------------------------------------------------------------

(defun hyalo-environment--log (format-string &rest args)
  "Log environment detection debug message."
  (when (fboundp 'hyalo-log)
    (apply #'hyalo-log 'environment format-string args))
  (message "[hyalo-env] %s" (apply #'format format-string args)))

;; -----------------------------------------------------------------------------
;; User/Host Detection
;; -----------------------------------------------------------------------------

(defun hyalo-environment--user-host-info ()
  "Return user@hostname info as an alist."
  `((username . ,(user-login-name))
    (hostname . ,(system-name))))

;; -----------------------------------------------------------------------------
;; Development Environment Detection
;; -----------------------------------------------------------------------------
;;
;; Uses `hyalo-status--project-root' for consistent project detection across
;; all hyalo modules. This ensures a single source of truth for project root.

(defun hyalo-environment--detect-pixi ()
  "Detect Pixi environment in current project."
  (let* ((project-root (hyalo-status--project-root))
         (env-name (getenv "PIXI_ENVIRONMENT_NAME"))
         (pixi-dir (when project-root
                     (locate-dominating-file project-root ".pixi"))))
    (when (or env-name pixi-dir)
      `((type . "pixi")
        (name . ,(or env-name "default"))
        (icon . "shippingbox")
        (isActive . ,(if env-name t nil))
        (path . ,(or pixi-dir project-root "/"))))))

(defun hyalo-environment--detect-conda ()
  "Detect Conda environment in current project."
  (let* ((project-root (hyalo-status--project-root))
         (env-name (getenv "CONDA_DEFAULT_ENV"))
         (env-file (when project-root
                     (locate-dominating-file project-root "environment.yml"))))
    (when (or env-name env-file)
      `((type . "conda")
        (name . ,(or env-name "base"))
        (icon . "c.circle.fill")
        (isActive . ,(if env-name t nil))
        (path . ,(or env-file project-root "/"))))))

(defun hyalo-environment--detect-npm ()
  "Detect NPM/Node environment in current project."
  (let* ((project-root (hyalo-status--project-root))
         (package-json (when project-root
                         (locate-dominating-file project-root "package.json")))
         (node-modules (when project-root
                         (locate-dominating-file project-root "node_modules"))))
    (when (or package-json node-modules)
      `((type . "npm")
        (name . ,(or (hyalo-environment--node-version) "node"))
        (icon . "n.circle.fill")
        (isActive . nil)
        (path . ,(or package-json project-root "/"))))))

(defvar hyalo-environment--node-version-cache nil
  "Cache for Node.js version to avoid repeated subprocess calls.")

(defun hyalo-environment--node-version ()
  "Get Node.js version if available.
Caches the result to avoid repeated subprocess calls."
  (or hyalo-environment--node-version-cache
      (setq hyalo-environment--node-version-cache
            (condition-case nil
                (with-temp-buffer
                  (when (eq 0 (call-process "node" nil t nil "--version"))
                    (string-trim (buffer-string))))
              (error nil)))))

(defun hyalo-environment--detect-bun ()
  "Detect Bun environment in current project."
  (let* ((project-root (hyalo-status--project-root))
         (bun-lock (when project-root
                     (or (locate-dominating-file project-root "bun.lockb")
                         (locate-dominating-file project-root "bun.lock")))))
    (when bun-lock
      `((type . "bun")
        (name . "bun")
        (icon . "b.circle.fill")
        (isActive . nil)
        (path . ,bun-lock)))))

(defun hyalo-environment--detect-swift ()
  "Detect Swift environment in current project."
  (let* ((project-root (hyalo-status--project-root))
         (package-swift (when project-root
                          (locate-dominating-file project-root "Package.swift")))
         (swift-version (when project-root
                          (locate-dominating-file project-root ".swift-version")))
         (sources-dir (when project-root
                        (locate-dominating-file project-root "Sources")))
         (has-swift-files (and sources-dir
                               (directory-files sources-dir t "\\.swift$" nil 1))))
    (when (or package-swift swift-version has-swift-files)
      `((type . "swift")
        (name . ,(or (hyalo-environment--swift-version) "swift"))
        (icon . "swift")
        (isActive . nil)
        (path . ,(or package-swift sources-dir project-root "/"))))))

(defvar hyalo-environment--swift-version-cache nil
  "Cache for Swift version to avoid repeated subprocess calls.")

(defun hyalo-environment--swift-version ()
  "Get Swift version if available.
Caches the result to avoid repeated subprocess calls."
  (or hyalo-environment--swift-version-cache
      (setq hyalo-environment--swift-version-cache
            (condition-case nil
                (with-temp-buffer
                  (when (eq 0 (call-process "swift" nil t nil "--version"))
                    (when (string-match "Swift version \\([^ ]+\\)" (buffer-string))
                      (match-string 1 (buffer-string)))))
              (error nil)))))

(defun hyalo-environment--detect-rust ()
  "Detect Rust environment in current project."
  (let* ((project-root (hyalo-status--project-root))
         (cargo-toml (when project-root
                       (locate-dominating-file project-root "Cargo.toml")))
         (rust-toolchain (when project-root
                           (locate-dominating-file project-root "rust-toolchain.toml"))))
    (when (or cargo-toml rust-toolchain)
      `((type . "rust")
        (name . ,(or (hyalo-environment--rust-version) "rust"))
        (icon . "r.circle.fill")
        (isActive . nil)
        (path . ,(or cargo-toml project-root "/"))))))

(defun hyalo-environment--rust-version ()
  "Get Rust version if available."
  (condition-case nil
      (with-temp-buffer
        (when (eq 0 (call-process "rustc" nil t nil "--version"))
          (when (string-match "rustc \\([^ ]+\\)" (buffer-string))
            (match-string 1 (buffer-string)))))
    (error nil)))

(defun hyalo-environment--detect-python-venv ()
  "Detect Python virtualenv in current project."
  (let* ((project-root (hyalo-status--project-root))
         (venv-dir (when project-root
                     (or (locate-dominating-file project-root ".venv")
                         (locate-dominating-file project-root "venv")))))
    (when venv-dir
      `((type . "python")
        (name . "venv")
        (icon . "play.circle.fill")
        (isActive . nil)
        (path . ,venv-dir)))))

(defun hyalo-environment--detect-docker ()
  "Detect Docker environment in current project."
  (let* ((project-root (hyalo-status--project-root))
         (dockerfile (when project-root
                       (locate-dominating-file project-root "Dockerfile")))
         (compose (when project-root
                    (or (locate-dominating-file project-root "docker-compose.yml")
                        (locate-dominating-file project-root "docker-compose.yaml")))))
    (when (or dockerfile compose)
      `((type . "docker")
        (name . "docker")
        (icon . "shippingbox")
        (isActive . nil)
        (path . ,(or dockerfile compose project-root "/"))))))

(defun hyalo-environment--detect-all ()
  "Detect all environments in priority order.
Returns nil if no project root is found, allowing preservation
of previous environment state for non-project buffers."
  (let* ((project-root (hyalo-status--project-root))
         (default-directory (or default-directory "/"))
         (buffer-name (buffer-name)))
    ;; Only detect when we have a valid project
    (when project-root
      (let ((environments (delq nil
                               (list
                                (hyalo-environment--detect-pixi)
                                (hyalo-environment--detect-conda)
                                (hyalo-environment--detect-bun)
                                (hyalo-environment--detect-npm)
                                (hyalo-environment--detect-swift)
                                (hyalo-environment--detect-rust)
                                (hyalo-environment--detect-python-venv)
                                (hyalo-environment--detect-docker)))))
        ;; Only log when environments actually found or changed
        (when (> (length environments) 0)
          (hyalo-environment--log "detect-all: buffer=%s found=%d envs"
                                 buffer-name (length environments)))
        environments))))

;; -----------------------------------------------------------------------------
;; State Push
;; -----------------------------------------------------------------------------

(defvar hyalo-environment--last-user-host nil
  "Cache of last user/host info to avoid redundant pushes.")

(defvar hyalo-environment--last-environments nil
  "Cache of last environment list to avoid redundant pushes.")

(defvar hyalo-environment--last-had-project nil
  "Track whether we had a project context on last push.")

(defvar hyalo-environment--push-idle-timer nil
  "Timer for debounced push.")

(defun hyalo-environment--push ()
  "Push user/host and environment state to Swift.
Skips temp/internal buffers. Debounces rapid calls."
  ;; Skip temp/internal buffers entirely
  (let ((name (buffer-name)))
    (when (and name
               (not (minibufferp))
               (not (string-prefix-p " " name))
               (not (string-match-p "temp" name))
               (not (string-match-p "^\\*" name)))
      ;; Cancel any pending timer and schedule new one (debounce)
      (when hyalo-environment--push-idle-timer
        (cancel-timer hyalo-environment--push-idle-timer))
      (setq hyalo-environment--push-idle-timer
            (run-with-idle-timer 0.1 nil #'hyalo-environment--do-push)))))

(defun hyalo-environment--do-push ()
  "Actually perform the push to Swift.
Called after debounce timer fires."
  (let* ((user-host (hyalo-environment--user-host-info))
         (project-root (hyalo-status--project-root))
         (has-project (not (null project-root)))
         (had-project hyalo-environment--last-had-project)
         ;; Only detect when in project, otherwise preserve last
         (environments (if has-project
                          (hyalo-environment--detect-all)
                        hyalo-environment--last-environments)))
    ;; Update state tracking
    (setq hyalo-environment--last-had-project has-project)
    ;; Push user/host if changed
    (unless (equal user-host hyalo-environment--last-user-host)
      (setq hyalo-environment--last-user-host user-host)
      (when (fboundp 'hyalo-update-user-host)
        (hyalo-update-user-host (json-encode user-host))))
    ;; Push environments if changed or project state changed
    (when (or (not (equal environments hyalo-environment--last-environments))
              (not (eq has-project had-project)))
      (setq hyalo-environment--last-environments environments)
      (when (fboundp 'hyalo-update-environments)
        (hyalo-update-environments (json-encode (vconcat (or environments '()))))))))

;; -----------------------------------------------------------------------------
;; Hook Management
;; -----------------------------------------------------------------------------

(defun hyalo-environment--on-buffer-change (&rest _)
  "Hook for buffer/window changes."
  (hyalo-environment--push))

(defun hyalo-environment--on-find-file ()
  "Hook for find-file."
  (hyalo-environment--push))

(defun hyalo-environment--on-save ()
  "Hook for after-save."
  (hyalo-environment--push))

(defun hyalo-environment-setup ()
  "Set up environment detection hooks and push initial state."
  ;; Add hooks (no polling, debounced in push)
  (add-hook 'find-file-hook #'hyalo-environment--on-find-file)
  (add-hook 'window-buffer-change-functions #'hyalo-environment--on-buffer-change)
  (add-hook 'after-save-hook #'hyalo-environment--on-save)
  (add-hook 'project-switch-project-hook #'hyalo-environment-refresh)
  ;; Initial push after Emacs settles
  (run-with-idle-timer 0.5 nil #'hyalo-environment--do-push))

;; -----------------------------------------------------------------------------
;; Cleanup
;; -----------------------------------------------------------------------------

(defun hyalo-environment-teardown ()
  "Remove environment detection hooks and cancel pending timers."
  (when hyalo-environment--push-idle-timer
    (cancel-timer hyalo-environment--push-idle-timer)
    (setq hyalo-environment--push-idle-timer nil))
  (remove-hook 'find-file-hook #'hyalo-environment--on-find-file)
  (remove-hook 'window-buffer-change-functions #'hyalo-environment--on-buffer-change)
  (remove-hook 'after-save-hook #'hyalo-environment--on-save)
  (remove-hook 'project-switch-project-hook #'hyalo-environment-refresh))

;; -----------------------------------------------------------------------------
;; Interactive Commands
;; -----------------------------------------------------------------------------

;;;###autoload
(defun hyalo-environment-refresh ()
  "Manually refresh environment detection and push to SwiftUI.
Clears all caches to force fresh detection."
  (interactive)
  (setq hyalo-environment--last-user-host nil)
  (setq hyalo-environment--last-environments nil)
  (setq hyalo-environment--last-had-project nil)
  (setq hyalo-environment--swift-version-cache nil)
  (setq hyalo-environment--node-version-cache nil)
  ;; Cancel any pending debounced push
  (when hyalo-environment--push-idle-timer
    (cancel-timer hyalo-environment--push-idle-timer)
    (setq hyalo-environment--push-idle-timer nil))
  ;; Immediate push
  (hyalo-environment--do-push)
  (message "Hyalo: Environment refreshed (%d envs detected)"
           (length hyalo-environment--last-environments)))

;;;###autoload
(defun hyalo-environment-diagnostics ()
  "Show detailed environment detection diagnostics."
  (interactive)
  (let* ((project-root (hyalo-status--project-root))
         (buffer-dir default-directory)
         (envs (hyalo-environment--detect-all)))
    (with-output-to-temp-buffer "*Hyalo Environment Diagnostics*"
      (princ (format "Environment Detection Diagnostics\n"))
      (princ (format "================================\n\n"))
      (princ (format "Project root:     %s\n" (or project-root "NIL")))
      (princ (format "Buffer directory: %s\n" buffer-dir))
      (princ (format "Buffer name:      %s\n" (buffer-name)))
      (princ (format "Major mode:       %s\n" major-mode))
      (princ (format "\nDetected environments: %d\n" (length envs)))
      (if envs
          (dolist (env envs)
            (princ (format "  - %s: %s (%s)\n"
                          (cdr (assoc 'type env))
                          (cdr (assoc 'name env))
                          (cdr (assoc 'path env)))))
        (princ "  None detected\n"))
      (princ "\nCheck *Messages* buffer for detailed logs.\n"))))

;; -----------------------------------------------------------------------------
;; Channel Callbacks (called from Swift)
;; -----------------------------------------------------------------------------

(defun hyalo-environment--switch (env-type)
  "Switch to environment of type ENV-TYPE."
  (message "Hyalo: Switch to %s environment" env-type))

(defun hyalo-environment--open-terminal ()
  "Open terminal in current project root."
  (message "Hyalo: Open terminal in %s" default-directory))

(provide 'hyalo-environment)
;;; hyalo-environment.el ends here
