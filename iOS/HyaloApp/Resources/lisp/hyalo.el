;;; hyalo.el --- Core loader for hyalo dynamic module -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Version: 2.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Core loader for the Hyalo Emacs dynamic module.
;; This file is responsible for:
;; - Building the Swift dynamic module (.dylib) when needed
;; - Loading the dynamic module via `module-load'
;; - Feature detection and availability checks
;;
;; Window setup and channel initialization are in hyalo-window.el.
;; Feature-specific functionality is in separate files:
;; - hyalo-channels.el: Bidirectional Swift<->Emacs Lisp channels
;; - hyalo-window.el: Window controller interface, keybindings, setup orchestration
;; - hyalo-navigator.el: Navigator data (buffers, files, search)
;; - hyalo-status.el: Status bar data push
;; - hyalo-appearance.el: Theme and vibrancy synchronization

;;; Code:

(require 'cl-lib)

(defgroup hyalo nil
  "Hyalo IDE shell for Emacs."
  :group 'frames
  :prefix "hyalo-")

(defcustom hyalo-auto-build t
  "If non-nil, automatically build the module when loading if needed."
  :type 'boolean
  :group 'hyalo)

;;; Logging

(defvar hyalo-elog nil
  "Hyalo module logger (nil if elog not loaded).")

(defun hyalo-trace (context msg &rest args)
  "Log trace level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-trace))
        (apply #'elog-trace hyalo-elog formatted args)
      ;; Trace ignored if elog not available
      nil)))

(defun hyalo-debug (context msg &rest args)
  "Log debug level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-debug))
        (apply #'elog-debug hyalo-elog formatted args)
      ;; Debug ignored if elog not available
      nil)))

(defun hyalo-info (context msg &rest args)
  "Log info level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-info))
        (apply #'elog-info hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] %s" out)))))

(defun hyalo-warn (context msg &rest args)
  "Log warning level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-warn))
        (apply #'elog-warn hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] WARNING: %s" out)))))

(defun hyalo-error (context msg &rest args)
  "Log error level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-error))
        (apply #'elog-error hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] ERROR: %s" out)))))

(defun hyalo-log (context msg &rest args)
  "Log MSG with ARGS in CONTEXT using elog if available, otherwise `message'.
This is a compatibility wrapper around `hyalo-info'.
It attempts to detect error/warning messages to route them appropriate."
  (cond
   ((or (string-match-p "ERROR" msg) (string-match-p "failed" msg))
    (apply #'hyalo-error context msg args))
   ((string-match-p "WARNING" msg)
    (apply #'hyalo-warn context msg args))
   (t
    (apply #'hyalo-info context msg args))))

(defun hyalo-log-init ()
  "Initialize hyalo logger if elog is available."
  (when (fboundp 'elog-logger)
    (setq hyalo-elog
           (elog-logger
            :name "hyalo"
            :level 'info
            :buffer "*elog*"
            :handlers '(buffer)))))

;; Initialize logger when elog becomes available
(with-eval-after-load 'elog
  (hyalo-log-init))

;;; Module State

(defvar hyalo--loaded nil
  "Non-nil when the dynamic module has been successfully loaded.")

(defvar hyalo--base-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Base directory where Sources/ and .build/ are located (parent of lisp/).")

;;; Build Support

(defun hyalo--source-files ()
  "Return a list of all Swift source files for the module."
  (let ((sources-dir (expand-file-name "Sources" hyalo--base-dir)))
    (when (file-directory-p sources-dir)
      (directory-files-recursively sources-dir "\\.swift$"))))

(defun hyalo--package-file ()
  "Return the path to Package.swift if it exists."
  (let ((pkg (expand-file-name "Package.swift" hyalo--base-dir)))
    (when (file-exists-p pkg) pkg)))

(defun hyalo--needs-rebuild-p ()
  "Return non-nil if the module needs to be rebuilt.
Compares source file timestamps against `.build/build.db' which SPM
always updates on every build (even incremental ones that skip
relinking).  The dylib timestamp is unreliable because Swift 6.2
skips relinking when the ABI is unchanged."
  (let ((dylib (hyalo--find-dylib)))
    (if (null dylib)
        t
      (let* ((build-db (expand-file-name ".build/build.db" hyalo--base-dir))
             (ref-time (if (file-exists-p build-db)
                           (file-attribute-modification-time
                            (file-attributes build-db))
                         ;; No build.db: fall back to dylib time
                         (file-attribute-modification-time
                          (file-attributes dylib))))
             (pkg-file (hyalo--package-file))
             (source-files (hyalo--source-files)))
        (or
         (and pkg-file
              (time-less-p ref-time
                           (file-attribute-modification-time
                            (file-attributes pkg-file))))
         (cl-some
          (lambda (src)
            (time-less-p ref-time
                         (file-attribute-modification-time
                          (file-attributes src))))
          source-files))))))

(defconst hyalo--module-build-activity-id "module-build"
  "Stable activity ID for module build.")

(defun hyalo-build (&optional release)
  "Build the hyalo module using Swift Package Manager.
With prefix arg RELEASE, build in release mode.

When the module is loaded, uses the Swift-side async build process
with full lifecycle tracking (start, real-time logs, finish) in
the activity viewer.  No polling — uses Process pipes.

When the module is not yet loaded (initial build), falls back to
synchronous `call-process-shell-command'.

Returns non-nil on success (sync) or t immediately (async)."
  (interactive "P")
  (let* ((default-directory hyalo--base-dir)
         (config (if release "release" "debug")))
    (hyalo-log 'core "Building (%s)..." config)
    (if (fboundp 'hyalo-async-build)
        ;; Async build via Swift Process — full lifecycle tracking
        (progn
          (hyalo-async-build hyalo--base-dir config)
          t)
      ;; Sync fallback for initial build (module not yet loaded)
      (let* ((buffer-name "*hyalo-build*")
             (cmd (format "swift build -c %s" config))
             (result (call-process-shell-command cmd nil buffer-name t)))
        (if (= result 0)
            (progn
              (hyalo-log 'core "Build successful")
              t)
          (pop-to-buffer buffer-name)
          (hyalo-log 'core "Build failed (see %s)" buffer-name)
          nil)))))

(defun hyalo-rebuild-and-reload ()
  "Rebuild the module synchronously and reload it into Emacs.
Uses synchronous `call-process-shell-command' (not the async
build) because the reload must happen after the build completes."
  (interactive)
  (let* ((default-directory hyalo--base-dir)
         (buffer-name "*hyalo-build*")
         (cmd "swift build -c debug")
         (result (call-process-shell-command cmd nil buffer-name t)))
    (if (= result 0)
        (let ((dylib-path (hyalo--find-dylib)))
          (if dylib-path
              (condition-case err
                  (progn
                    (module-load dylib-path)
                    (setq hyalo--loaded t)
                    (hyalo-log 'core "Rebuilt and reloaded from %s" dylib-path)
                    t)
                (error
                 (hyalo-log 'core "Reload failed: %s" (error-message-string err))
                 nil))
            (hyalo-log 'core "No dylib found after build")
            nil))
      (pop-to-buffer buffer-name)
      (hyalo-log 'core "Rebuild failed (see %s)" buffer-name)
      nil)))

;;; Module Path Discovery

(defun hyalo--find-dylib ()
  "Find the path to the libHyalo.dylib dynamic module.
Returns the most recently modified dylib (release or debug)."
  (let* ((release (expand-file-name ".build/release/libHyalo.dylib" hyalo--base-dir))
         (debug (expand-file-name ".build/debug/libHyalo.dylib" hyalo--base-dir))
         (release-exists (file-exists-p release))
         (debug-exists (file-exists-p debug)))
    (cond
     ((and release-exists debug-exists)
      ;; Return whichever was modified most recently
      (let ((release-time (file-attribute-modification-time (file-attributes release)))
            (debug-time (file-attribute-modification-time (file-attributes debug))))
        (if (time-less-p debug-time release-time)
            release
          debug)))
     (release-exists release)
     (debug-exists debug)
     (t nil))))

;;; Module Loading

;;;###autoload
(defun hyalo-load ()
  "Load the hyalo dynamic module.
Always loads the existing dylib immediately if available.  If
`hyalo-auto-build' is non-nil and the module needs rebuilding,
the rebuild runs in the background after loading (via the async
build process with activity viewer tracking).
Returns non-nil on success, nil on failure."
  (interactive)
  (cond
   (hyalo--loaded
    (hyalo-log 'core "Already loaded")
    t)
   (t
    (let ((dylib-path (hyalo--find-dylib)))
      (if dylib-path
          ;; Dylib exists — load it, then optionally rebuild in background
          (let ((loaded (hyalo--do-load)))
            (when (and loaded hyalo-auto-build (hyalo--needs-rebuild-p))
              (hyalo-log 'core "Module outdated — rebuilding in background...")
              (hyalo-async-build hyalo--base-dir "debug"))
            loaded)
        ;; No dylib — must build synchronously first
        (if hyalo-auto-build
            (progn
              (hyalo-log 'core "Module not found, building...")
              (if (hyalo-build t)
                  (hyalo--do-load)
                (hyalo-log 'core "Build failed")
                nil))
          (hyalo-log 'core "Dynamic module not found. Run: swift build")
          nil))))))

(defun hyalo--do-load ()
  "Internal function to load the dylib."
  (let ((dylib-path (hyalo--find-dylib)))
    (if (null dylib-path)
        (progn
          (hyalo-log 'core "Dynamic module not found. Run: swift build")
          nil)
      (condition-case err
          (progn
            (module-load dylib-path)
            (setq hyalo--loaded t)
            (hyalo-log 'core "Loaded from %s" dylib-path)
            ;; Start the build watcher so external swift build
            ;; completions are detected via file system events.
            (when (fboundp 'hyalo-start-build-watcher)
              (hyalo-start-build-watcher hyalo--base-dir))
            ;; Setup module reload channel
            (when (fboundp 'hyalo-activity-set-module-reload)
              (hyalo-activity-set-module-reload))
            t)
        (error
         (hyalo-log 'core "Failed to load module: %s" (error-message-string err))
         nil)))))

;;; Module Availability

(defun hyalo-available-p ()
  "Return non-nil if the hyalo module is loaded and available."
  hyalo--loaded)

(defun hyalo-ensure ()
  "Ensure the module is loaded.  Signal error if loading fails."
  (unless (or hyalo--loaded (hyalo-load))
    (user-error "Hyalo: Module not available.  Run: swift build")))

;;; Module Version

(defun hyalo-version-check ()
  "Display the version string of the loaded module."
  (interactive)
  (if (and hyalo--loaded (fboundp 'hyalo-version))
      (message "Hyalo %s" (hyalo-version))
    (message "Hyalo module not loaded")))

(provide 'hyalo)
;;; hyalo.el ends here
