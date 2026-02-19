;;; init.el -*- lexical-binding: t; no-byte-compile: t -*-

;; Hyalo standalone init.
;; Test with: emacs --init-directory /path/to/hyalo

;;; ===========================================================================
;;; Bootstrap
;;; ===========================================================================

(defvar emacs-config-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The root directory of this Emacs configuration.")

(add-to-list 'load-path (expand-file-name "init" emacs-config-dir))
(add-to-list 'load-path (expand-file-name "lisp" emacs-config-dir))

;; Frame exists now.  If early-init.el loaded the dylib successfully,
;; install the SwiftUI chrome IMMEDIATELY — before init-bootstrap, which
;; blocks on the network during a fresh install.  The user sees the Hyalo
;; shell during the entire package bootstrap rather than after it.
;;
;; The loading proxy window is shown ONLY when a bootstrap will occur
;; (no .local or .local/elpa directory).  Normal startups skip the proxy
;; and the frame is revealed immediately after decoration.
;;
;; hyalo-window--early-setup is idempotent (guarded by early-setup-done).
;; If the dylib was not available in early-init (never built), this is a
;; no-op; init-hyalo.el will load and retry after the package system is up.
(defvar hyalo--needs-bootstrap
  (let ((local-dir (expand-file-name ".local/" emacs-config-dir)))
    (or (not (file-directory-p local-dir))
        (not (file-directory-p (expand-file-name "elpa/" local-dir)))))
  "Non-nil when a package bootstrap is needed (.local or .local/elpa missing).")

(when (and initial-window-system
           (fboundp 'hyalo-available-p) (hyalo-available-p))
  ;; Tell Swift whether to show the loading proxy before decoration.
  (when (fboundp 'hyalo-set-needs-bootstrap)
    (hyalo-set-needs-bootstrap hyalo--needs-bootstrap))
  (require 'hyalo-window)
  (hyalo-window--early-setup))

;; Check for module support early
(unless module-file-suffix
  (error "Emacs was not compiled with dynamic module support (--with-modules)"))

;;; ---------------------------------------------------------------------------
;;; Global message redirect to loading proxy
;;; ---------------------------------------------------------------------------
;;
;; Forward `message' output to the loading proxy window for the entire
;; duration of init.  This covers: init-bootstrap (package-refresh,
;; use-package install), all use-package :ensure installs across init
;; files, and package-vc-install calls.
;;
;; sit-for 0.01 (10ms) instead of 0: sit-for 0 returns immediately if
;; no input is pending; SwiftUI renders via a Core Animation display link
;; observer which needs at least one run loop iteration to fire.  10ms
;; is enough for one display cycle (16ms at 60 Hz) without meaningfully
;; slowing init.
(defvar hyalo--init-msg-redirecting nil
  "Non-nil when a message redirect is in progress (prevents recursion).")

(defun hyalo--init-message-redirect (fmt &rest args)
  "Forward FMT/ARGS to the loading proxy, avoiding re-entrancy."
  (unless hyalo--init-msg-redirecting
    (let ((hyalo--init-msg-redirecting t))
      (when (and (fboundp 'hyalo-set-loading-message) fmt (stringp fmt))
        (condition-case nil
            (let ((msg (string-trim (apply #'format fmt args))))
              (unless (or (string-empty-p msg)
                          ;; Skip noisy internal messages
                          (string-match-p "^Loading " msg)
                          (string-match-p "^Compiling " msg)
                          (string-match-p "^For information about" msg))
                (hyalo-set-loading-message msg)
                (sit-for 0.01)))
          (error nil))))))

(when (and hyalo--needs-bootstrap (fboundp 'hyalo-set-loading-message))
  (advice-add 'message :after #'hyalo--init-message-redirect
              '((name . hyalo-init-redirect))))

(when (and hyalo--needs-bootstrap (fboundp 'hyalo-set-loading-message))
  (hyalo-set-loading-message "Bootstrapping packages…") (sit-for 0.01))
(require 'init-bootstrap)

;;; ===========================================================================
;;; Modules
;;; ===========================================================================

;; hyalo-lib is loaded by init-bootstrap after load-path setup
(require 'init-core)

;; Helper to trace module initialization time
(defmacro init--require-with-trace (feature &optional filename)
  `(let ((start (float-time)))
     (require ,feature ,filename)
     (when (and (fboundp 'elog-info) (boundp 'emacs-logger))
       (elog-info emacs-logger "[%s] Loaded (%.3fs)" ,feature (- (float-time) start)))))

(when hyalo--needs-bootstrap
  (when (fboundp 'hyalo-set-loading-message) (hyalo-set-loading-message "Loading editor settings…") (sit-for 0.01)))
(init--require-with-trace 'init-emacs)
(init--require-with-trace 'init-appearance)
(init--require-with-trace 'init-editing)
(init--require-with-trace 'init-help)
(init--require-with-trace 'init-completion)
(when hyalo--needs-bootstrap
  (when (fboundp 'hyalo-set-loading-message) (hyalo-set-loading-message "Loading tools…") (sit-for 0.01)))
(init--require-with-trace 'init-tools)
(init--require-with-trace 'init-modes)
(init--require-with-trace 'init-tengwar)
(init--require-with-trace 'init-markdown)
(init--require-with-trace 'init-header)
;; init-hyalo MUST load before init-agents so the module is available
;; and the frame is visible BEFORE any package-vc-install calls that
;; may block for a long time (git clone).
(when hyalo--needs-bootstrap
  (when (fboundp 'hyalo-set-loading-message) (hyalo-set-loading-message "Initializing IDE shell…") (sit-for 0.01)))
(init--require-with-trace 'init-hyalo)
(when hyalo--needs-bootstrap
  (when (fboundp 'hyalo-set-loading-message) (hyalo-set-loading-message "Loading agents…") (sit-for 0.01)))
(init--require-with-trace 'init-agents)

;;; ===========================================================================
;;; Finalize
;;; ===========================================================================

;; Remove the global message redirect now that init is complete.
;; From this point, `message' goes to the echo area as usual.
(advice-remove 'message #'hyalo--init-message-redirect)

(let ((duration (float-time (time-subtract (current-time) before-init-time))))
  (when (and (fboundp 'elog-info) (boundp 'emacs-logger))
    (elog-info emacs-logger "[init] Complete (%.3fs)" duration)))

;;; init.el ends here
