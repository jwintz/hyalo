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

;; Check for module support early
(unless module-file-suffix
  (error "Emacs was not compiled with dynamic module support (--with-modules)"))

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

(init--require-with-trace 'init-emacs)
(init--require-with-trace 'init-appearance)
(init--require-with-trace 'init-editing)
(init--require-with-trace 'init-help)
(init--require-with-trace 'init-completion)
(init--require-with-trace 'init-tools)
(init--require-with-trace 'init-modes)
(init--require-with-trace 'init-tengwar)
(init--require-with-trace 'init-markdown)
(init--require-with-trace 'init-header)
;; init-hyalo MUST load before init-agents so the module is available
;; and the frame is visible BEFORE any package-vc-install calls that
;; may block for a long time (git clone).
(init--require-with-trace 'init-hyalo)
(init--require-with-trace 'init-agents)

;;; ===========================================================================
;;; Finalize
;;; ===========================================================================

(let ((duration (float-time (time-subtract (current-time) before-init-time))))
  (when (and (fboundp 'elog-info) (boundp 'emacs-logger))
    (elog-info emacs-logger "[init] Complete (%.3fs)" duration)))

;;; init.el ends here
