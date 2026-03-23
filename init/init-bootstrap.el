;;; init-bootstrap.el --- Bootstrap configuration -*- lexical-binding: t; -*-

;; Minimal bootstrap for the standalone Hyalo configuration.

;;; Code:

;;; Early Variables (before package-initialize)

;; Prevent demap load cycle during eager macro-expansion
;; Must be defined BEFORE package-initialize
(defvar demap--tools-demap-defined-start-p t)

;; cua-mode is referenced by the built-in Options menu toggle handler
;; during package archive download, before cua-base.el is loaded.
(defvar cua-mode nil)

;;; Performance & Output

;; debug-on-error is deferred until after init completes.
;; During a fresh install, benign errors from autoload files (e.g.,
;; void-function in modus-themes) and network hiccups would trigger
;; the interactive debugger.  The Emacs frame is invisible at this
;; point (proxy window is showing), so the debugger blocks forever
;; with no user interaction possible.  Use --debug-init for debugging.
;; Enabled via emacs-startup-hook below (after gc restore).

;; Temporarily reduce garbage collection during startup
(defvar hyalo--default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; Suppress internal redisplay during init (frame is invisible anyway).
;; Only safe during normal startup — bootstrap needs sit-for redisplay
;; for the loading proxy progress messages.
(unless hyalo--needs-bootstrap
  (setq inhibit-redisplay t))
(defvar hyalo--default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Re-enable redisplay (suppressed during normal init)
            (setq inhibit-redisplay nil)
            ;; gcmh-style GC: keep a high threshold during editing and
            ;; only collect during idle.  Prevents GC pauses mid-keystroke.
            (setq gc-cons-threshold (* 64 1024 1024))
            (run-with-idle-timer 15 t #'garbage-collect)
            ;; Merge rather than replace: packages may have added handlers
            ;; during init that we want to preserve.
            (setq file-name-handler-alist
                  (delete-dups
                   (append file-name-handler-alist
                           hyalo--default-file-name-handler-alist)))
            ;; Enable debug-on-error now that the frame is (or will be)
            ;; visible.  Safe to interact with the debugger from here.
            (setq debug-on-error t)))

;; Prevent early UI allocation
(setq-default tool-bar-mode nil)
(setq-default scroll-bar-mode nil)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Suppress warnings during startup
(setq byte-compile-warnings nil)
(setq warning-suppress-types '((comp) (bytecomp)))
(setq warning-minimum-level :error)

;; Native compilation: limit to 1 background subprocess so Emacs
;; stays responsive.  Default 0 means half of (num-processors),
;; which saturates the CPU on multi-core machines.
(when (and (featurep 'native-compile)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-jobs-number 1))

;;; Directories

(setq user-emacs-directory (expand-file-name ".local/" emacs-config-dir))

(dolist (subdir '("" "auto-save" "auto-save-list" "transient" "eshell" "etc" "elpa"))
  (make-directory (expand-file-name subdir user-emacs-directory) t))

(setq auto-save-list-file-prefix (locate-user-emacs-file "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "auto-save/") t)))
(setq custom-file (locate-user-emacs-file "etc/custom.el"))
(setq package-user-dir (locate-user-emacs-file "elpa/"))

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; Package System

(require 'cl-lib)
(require 'package)
(unless (eq window-system 'ios)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Cache package-initialize results (Doom-style autoload cache).
;; package-initialize scans every installed package, reads its -pkg.el
;; and loads its -autoloads.el.  This takes 200-400ms on a typical
;; install.  We cache the resulting autoloads into a single file and
;; set package--initialized, package-alist, and load-path directly,
;; bypassing the per-package stat() calls on subsequent starts.
;;
;; Cache is invalidated when:
;;  - The cache file does not exist
;;  - elpa/ directory mtime is newer than the cache
;;  - Emacs version changes (byte-code compatibility)

(defvar hyalo--package-cache-file
  (expand-file-name "package-cache.el" user-emacs-directory)
  "File caching package-initialize results for faster startup.")

(defun hyalo--package-cache-valid-p ()
  "Return non-nil if the package cache exists and is newer than elpa/."
  (let ((cache-file hyalo--package-cache-file)
        (elpa-dir package-user-dir))
    (and (file-exists-p cache-file)
         (file-exists-p elpa-dir)
         ;; Cache must be newer than last elpa/ modification
         (time-less-p
          (file-attribute-modification-time (file-attributes elpa-dir))
          (file-attribute-modification-time (file-attributes cache-file))))))

(defun hyalo--package-cache-write ()
  "Dump current package state to cache file for fast reload.
Called after a full package-initialize so all autoloads are active."
  (with-temp-file hyalo--package-cache-file
    (let ((print-level nil)
          (print-length nil))
      (insert ";;; hyalo package cache -*- lexical-binding: t; -*-\n")
      (insert ";; Auto-generated — do not edit.  Delete to rebuild.\n")
      (insert (format ";; Emacs %s\n\n" emacs-version))
      ;; Save the Emacs version for invalidation
      (insert (format "(unless (equal emacs-version %S)\n  (error \"Package cache version mismatch\"))\n\n"
                      emacs-version))
      ;; Restore package-alist (needed for package-installed-p, describe-package, etc.)
      (insert (format "(setq package-alist '%S)\n\n" package-alist))
      ;; Restore load-path entries added by package-initialize
      ;; (only the elpa/ entries, not the whole load-path)
      (let ((elpa-paths (cl-remove-if-not
                         (lambda (p) (string-prefix-p (expand-file-name package-user-dir) p))
                         load-path)))
        (insert (format "(setq load-path (append '%S load-path))\n\n" elpa-paths)))
      ;; Load all autoload files — this is the expensive part we're caching
      (insert ";; Concatenated autoloads\n")
      (dolist (pkg-desc (apply #'append (mapcar #'cdr package-alist)))
        (let* ((dir (package-desc-dir pkg-desc))
               (auto-file (expand-file-name
                           (format "%s-autoloads.el" (package-desc-name pkg-desc))
                           dir)))
          (when (file-exists-p auto-file)
            (insert (format "\n;;; %s\n" (package-desc-name pkg-desc)))
            (insert (format "(let ((load-file-name %S))\n" auto-file))
            ;; Insert autoload file contents, stripping header/footer
            (let ((contents (with-temp-buffer
                              (insert-file-contents auto-file)
                              (buffer-string))))
              ;; Remove the ;;; commentary and provide/require boilerplate
              (insert contents))
            (insert ")\n"))))
      ;; Mark as initialized
      (insert "\n(setq package--initialized t)\n")))
  ;; Byte-compile the cache for faster loading (~15ms → ~5ms)
  (byte-compile-file hyalo--package-cache-file))

(defun hyalo--package-cache-delete ()
  "Delete the package cache and its byte-compiled form."
  (dolist (ext '(".el" ".elc"))
    (let ((f (concat (file-name-sans-extension hyalo--package-cache-file) ext)))
      (when (file-exists-p f) (delete-file f)))))

;; Saved here (before profiler is loaded) and injected later by init.el.
(defvar hyalo--package-init-timing nil
  "Cons of (PHASE-SYMBOL . ELAPSED-SECONDS) for package init profiling.")

(defun hyalo--package-initialize-cached ()
  "Load packages from cache if valid, otherwise initialize and build cache."
  (let ((start (float-time)))
    (if (hyalo--package-cache-valid-p)
        ;; Fast path: load the single cache file
        (condition-case err
            (progn
              (load hyalo--package-cache-file nil 'nomessage)
              (setq hyalo--package-init-timing
                    (cons 'package-cache-load (- (float-time) start))))
          (error
           ;; Cache is corrupt or version mismatch — fall back to full init
           (message "Package cache invalid (%s), rebuilding…" (error-message-string err))
           (hyalo--package-cache-delete)
           (hyalo--package-initialize-cached)))
      ;; Slow path: full package-initialize, then write cache
      (let ((debug-on-error nil))
        (package-initialize))
      (setq hyalo--package-init-timing
            (cons 'package-initialize (- (float-time) start)))
      ;; Write cache for next startup
      (hyalo--package-cache-write))))

;; Some packages (e.g. modus-themes) have autoload files that call their own
;; macros before those macros are loaded.  debug-on-error t would enter the
;; debugger on those benign errors.  Silence them during package-initialize.
(hyalo--package-initialize-cached)

;; Restore load-prefer-newer NOW (not on startup-hook) so that edits to
;; lisp/ and init/ files are always picked up on relaunch, even if stale
;; .elc files exist.  The package cache (the only file that benefited from
;; nil) has already been loaded above.
(setq load-prefer-newer t)

;; Rebuild package cache after any install so next startup is fast.
(advice-add 'package-install :after
            (lambda (&rest _)
              (hyalo--package-cache-delete)
              (hyalo--package-cache-write))
            '((name . hyalo-rebuild-cache)))
(advice-add 'package-delete :after
            (lambda (&rest _)
              (hyalo--package-cache-delete))
            '((name . hyalo-invalidate-cache)))

(defun hyalo-package-cache-rebuild ()
  "Force rebuild of the package autoload cache."
  (interactive)
  (hyalo--package-cache-delete)
  (package-initialize)
  (hyalo--package-cache-write)
  (message "Package cache rebuilt: %s" hyalo--package-cache-file))

;; package-refresh-contents is deferred to the first interactive
;; package-install via a transient hook.  See below, after hyalo-lib
;; is loaded (transient hook macro lives there).

(unless (or (package-installed-p 'use-package)
            (eq window-system 'ios))
  ;; The global message redirect (installed in init.el) forwards all
  ;; `message' output to the loading proxy.  We only need explicit
  ;; step messages here for key milestones.
  (condition-case err
      (progn
        (when (fboundp 'hyalo-set-loading-message)
          (hyalo-set-loading-message "Refreshing package archives…") (sit-for 0.01))
        (package-refresh-contents)
        (when (fboundp 'hyalo-set-loading-message)
          (hyalo-set-loading-message "Installing use-package…") (sit-for 0.01))
        (package-install 'use-package))
    (error
     (message "Bootstrap: use-package install failed (%s); continuing without it"
              (error-message-string err)))))

(require 'use-package)
(setq use-package-always-ensure (not (eq window-system 'ios)))
;; Reduce macroexpansion overhead across ~30+ use-package forms.
(setq use-package-expand-minimally t)

;;; Logging (elog)

(defvar hyalo-init-start-time (float-time)
  "Timestamp when initialization started.")

(defun hyalo-log-step (msg)
  "Log initialization step MSG with elapsed time.
Uses elog if available, otherwise falls back to message."
  (let ((now (float-time)))
    (if (and (featurep 'elog) (boundp 'emacs-logger))
        (elog-info emacs-logger "[init] %s (%.3fs)" msg (- now hyalo-init-start-time))
      (message "[init] %s (%.3fs)" msg (- now hyalo-init-start-time)))))

(use-package elog
  :ensure t
  :if (not (eq window-system 'ios))
  :vc (:url "https://github.com/Kinneyzhang/elog" :rev :newest)
  ;; :vc checks git state even when installed (~15-20ms).
  :commands (elog-logger elog-info elog-debug elog-warn elog-error)
  :init
  (require 'elog)
  :config
  ;; Initialize main Emacs logger
  (defvar emacs-logger
    (elog-logger
     :name "emacs"
     :level 'info
     :buffer "*elog*"
     :handlers '(buffer))))

(hyalo-log-step "Packages initialized")

;;; Load Paths

(add-to-list 'load-path (expand-file-name "init" emacs-config-dir))
(add-to-list 'load-path (expand-file-name "lisp" emacs-config-dir))

;; hyalo-lib provides transient hooks, first-use hooks, and incremental
;; idle loading.  Load it here so init files can use these utilities.
(require 'hyalo-lib)

;; Defer package-refresh-contents to first interactive package-install.
;; Avoids blocking startup with network I/O when the cache is stale.
;; The Swift package manager panel calls hyalo-package--refresh for
;; explicit manual refresh.
(unless (eq window-system 'ios)
  (hyalo-add-transient-hook 'package-install
    (let ((last-refresh-file (expand-file-name "elpa/.last-package-refresh"
                                               user-emacs-directory))
          (refresh-interval (* 24 60 60)))
      (when (or (not (file-exists-p last-refresh-file))
                (> (- (float-time)
                      (float-time (file-attribute-modification-time
                                   (file-attributes last-refresh-file))))
                   refresh-interval))
        (message "Refreshing package contents...")
        (package-refresh-contents)
        (write-region "" nil last-refresh-file)))))

;; Register heavy packages for incremental idle loading.
;; These are deferred at init but preloaded during idle time so first
;; use is instant.  Loading starts 2s after startup, one package per
;; 0.75s idle interval, pausing if the user types.
(unless (eq window-system 'ios)
  (hyalo-load-packages-incrementally
   '(magit magit-git magit-process magit-section
     vertico consult orderless marginalia
     corfu markdown-mode diff-hl which-key
     eglot project)))

;;; Environment & Shell

;; exec-path-from-shell spawns a login shell to extract PATH (~300-800ms).
;; Defer to first input: PATH is only needed when invoking external commands,
;; not during init.  The shell is still spawned, just after the frame is visible.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (and (memq window-system '(mac ns x)) (not (eq window-system 'ios)))
    ;; Non-interactive, non-login shell: reads .zshenv only (fastest).
    ;; PATH-setting tools (pixi, homebrew) belong in .zshenv.
    (setq exec-path-from-shell-arguments nil)
    ;; Run on first input, or after 2s idle (whichever comes first).
    ;; The idle fallback ensures PATH is set before incremental package
    ;; loading starts (eglot, magit need correct PATH).
    (defvar hyalo--exec-path-initialized nil)
    (defun hyalo--exec-path-deferred ()
      "Initialize exec-path-from-shell exactly once."
      (unless hyalo--exec-path-initialized
        (setq hyalo--exec-path-initialized t)
        (exec-path-from-shell-initialize)))
    (add-hook 'hyalo-first-input-hook #'hyalo--exec-path-deferred)
    (run-with-idle-timer 2 nil #'hyalo--exec-path-deferred)))

;;; iOS-Specific Setup

;; iOS uses bundled packages instead of ELPA
(when (eq window-system 'ios)
  ;; Set paths for bundled resources
  (when (getenv "HYALO_BUNDLE_PATH")
    (let ((bundle-path (getenv "HYALO_BUNDLE_PATH")))
      ;; Add bundled lisp directory to load-path
      (add-to-list 'load-path (expand-file-name "lisp" bundle-path))
      ;; Set up bundled packages directory
      (setq package-user-dir (expand-file-name "bundled-elpa" bundle-path))
      ;; Initialize package system with bundled packages
      (setq package-archives nil)))
  ;; iOS uses local storage in app container
  (setq user-emacs-directory (expand-file-name "emacs.d" (or (getenv "HOME") "~")))
  ;; Disable native compilation (not available on iOS)
  (setq native-comp-async-jobs-number 0)
  (setq native-comp-enable-async-compilation nil))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
