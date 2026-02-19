;;; init-bootstrap.el --- Bootstrap configuration -*- lexical-binding: t; -*-

;; Minimal bootstrap for hyalo standalone.
;; Based on emacs.d/init/init-bootstrap.el.

;;; Code:

;;; ===========================================================================
;;; Early Variables (before package-initialize)
;;; ===========================================================================

;; Prevent demap load cycle during eager macro-expansion
;; Must be defined BEFORE package-initialize
(defvar demap--tools-demap-defined-start-p t)

;; cua-mode is referenced by the built-in Options menu toggle handler
;; during package archive download, before cua-base.el is loaded.
;; Pre-declare it to avoid a void-variable error that aborts the bootstrap.
(defvar cua-mode nil)

;;; ===========================================================================
;;; Performance & Output
;;; ===========================================================================

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
(defvar hyalo--default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold hyalo--default-gc-cons-threshold)
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
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(scroll-bar-mode . nil) default-frame-alist)

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

;;; ===========================================================================
;;; Directories
;;; ===========================================================================

(setq user-emacs-directory (expand-file-name ".local/" emacs-config-dir))

(dolist (subdir '("" "auto-save" "auto-save-list" "transient" "eshell" "etc" "elpa"))
  (make-directory (expand-file-name subdir user-emacs-directory) t))

(setq auto-save-list-file-prefix (locate-user-emacs-file "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "auto-save/") t)))
(setq custom-file (locate-user-emacs-file "etc/custom.el"))
(setq package-user-dir (locate-user-emacs-file "elpa/"))

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; ===========================================================================
;;; Package System
;;; ===========================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Some packages (e.g. modus-themes) have autoload files that call their own
;; macros before those macros are loaded.  debug-on-error t would enter the
;; debugger on those benign errors.  Silence them during package-initialize.
(let ((debug-on-error nil))
  (package-initialize))

;; package-refresh-contents is deferred to the first interactive
;; package-install via a transient hook.  See below, after hyalo-lib
;; is loaded (transient hook macro lives there).

(unless (package-installed-p 'use-package)
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
(setq use-package-always-ensure t)

;;; ===========================================================================
;;; Logging (elog)
;;; ===========================================================================

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
  :vc (:url "https://github.com/Kinneyzhang/elog" :rev :newest)
  :demand t
  :config
  ;; Initialize main Emacs logger
  (defvar emacs-logger
    (elog-logger
     :name "emacs"
     :level 'info
     :buffer "*elog*"
     :handlers '(buffer))))

(hyalo-log-step "Packages initialized")

;;; ===========================================================================
;;; Load Paths
;;; ===========================================================================

(add-to-list 'load-path (expand-file-name "init" emacs-config-dir))
(add-to-list 'load-path (expand-file-name "lisp" emacs-config-dir))

;; hyalo-lib provides transient hooks, first-use hooks, and incremental
;; idle loading.  Load it here so init files can use these utilities.
(require 'hyalo-lib)

;; Defer package-refresh-contents to first interactive package-install.
;; Avoids blocking startup with network I/O when the cache is stale.
;; The Swift package manager panel calls hyalo-package--refresh for
;; explicit manual refresh.
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
      (write-region "" nil last-refresh-file))))

;; Register heavy packages for incremental idle loading.
;; These are deferred at init but preloaded during idle time so first
;; use is instant.  Loading starts 2s after startup, one package per
;; 0.75s idle interval, pausing if the user types.
(hyalo-load-packages-incrementally
 '(magit magit-git magit-process magit-section
   vertico consult orderless marginalia
   corfu markdown-mode diff-hl which-key
   eglot project))

;;; ===========================================================================
;;; Environment & Shell
;;; ===========================================================================

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    ;; Non-interactive login shell: faster startup, loads .zshenv/.zprofile only
    ;; Move PATH exports from .zshrc to .zshenv for this to work
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
