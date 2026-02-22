;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs before frame creation and before init.el.
;; Goals here:
;;  1. Configure default-frame-alist (visibility, size, chrome suppression).
;;  2. Load the Hyalo dylib so it is present the moment init.el gets a frame.
;;  3. Redirect eln-cache.
;;  4. Install a failsafe visibility hook in case init.el aborts.

;;; Code:

;;; ---------------------------------------------------------------------------
;;; Boot logger — princ to stderr + /tmp/hyalo-boot.log
;;; ---------------------------------------------------------------------------

;; Used before the Hyalo module (and its elog integration) is available.
;; Writes to stderr (visible in Terminal) and to a file (visible without
;; a window, e.g., when launched from Finder or the dock).
(defun hyalo--boot-log (msg)
  "Write MSG to stderr and /tmp/hyalo-boot.log."
  (let ((line (concat "[hyalo:boot] " msg "\n")))
    (princ line #'external-debugging-output)
    (write-region line nil "/tmp/hyalo-boot.log" 'append 'silent)))

(hyalo--boot-log "early-init started")

;;; ---------------------------------------------------------------------------
;;; Frame defaults — hide until chrome is installed
;;; ---------------------------------------------------------------------------

;; Frames start invisible.  hyalo-window--early-setup (called at the top of
;; init.el, before the blocking package bootstrap) calls make-frame-visible
;; once the SwiftUI chrome is installed.  The failsafe hook at the bottom of
;; this file catches the case where init.el aborts before that point.
(push '(visibility . nil) default-frame-alist)
(push '(width  . 81) default-frame-alist)
(push '(height . 45) default-frame-alist)

;; Suppress chrome that Hyalo replaces with its own toolbar.
(push '(tool-bar-lines       . 0)   default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;; Prevent frame resizing during font setup.
(setq frame-inhibit-implied-resize t)
;; Allow pixel-level resize (not snapped to character cell grid).
;; Must be set here, before the initial frame is created, so that the NS port
;; configures contentResizeIncrements to (1,1) at window creation time.
;; Setting this later (e.g. in :config of use-package ns-win) is too late —
;; the NSWindow has already been configured with character-cell increments.
(setq frame-resize-pixelwise t)

;;; ---------------------------------------------------------------------------
;;; Load-suffix reduction
;;; ---------------------------------------------------------------------------

;; Hyalo does not load .so modules via require.  Removing the suffix
;; eliminates one stat() call per require.
(when (and (boundp 'load-suffixes) (member ".so" load-suffixes))
  (setq load-suffixes (remove ".so" load-suffixes)))

;;; ---------------------------------------------------------------------------
;;; eln-cache redirect
;;; ---------------------------------------------------------------------------

;; The target directory must exist before startup-redirect-eln-cache is
;; called.  On a fresh install (.local/ does not exist yet), the native
;; compilation subsystem silently fails without this make-directory.
(when (fboundp 'startup-redirect-eln-cache)
  (let ((eln-dir (expand-file-name ".local/eln-cache/" user-emacs-directory)))
    (make-directory eln-dir t)
    (startup-redirect-eln-cache eln-dir)))

;;; ---------------------------------------------------------------------------
;;; Load the Hyalo dylib — before frame creation, before package bootstrap
;;; ---------------------------------------------------------------------------

;; At early-init time there is no frame yet, so window decoration cannot
;; happen here.  What we CAN do is load the dylib so that by the time
;; init.el starts (and has a real frame), hyalo-available-p returns t
;; immediately — no build step, no delay.
;;
;; hyalo-window--early-setup is called at the very top of init.el, before
;; init-bootstrap (which blocks on network during a fresh install).
;;
;; If the dylib does not exist (truly fresh source checkout, never built),
;; this silently skips.  The failsafe hook below then ensures the frame
;; is still shown.
(let* ((base-dir user-emacs-directory)   ; --init-directory = project root
       (lisp-dir (expand-file-name "lisp" base-dir)))
  ;; Do NOT load the AppKit/SwiftUI module in terminal (-nw) mode.
  ;; The module links against AppKit and segfaults when loaded without
  ;; a graphical session.  `initial-window-system' is nil for -nw.
  (when (and initial-window-system
             (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)
    (condition-case err
        (progn
          (require 'hyalo)
          (if (hyalo-load)
              (hyalo--boot-log "dylib loaded")
            (hyalo--boot-log "dylib not found or build failed — will retry in init.el")))
      (error
       (hyalo--boot-log (format "dylib load error: %s" (error-message-string err)))))))

;;; ---------------------------------------------------------------------------
;;; Failsafe visibility hook
;;; ---------------------------------------------------------------------------

;; window-setup-hook fires after init.el completes AND after the window
;; system is initialized.  hyalo-window-setup registers on this hook at
;; default depth (0); this failsafe runs at depth 90, i.e., AFTER
;; hyalo-window--post-setup has had its chance to call make-frame-visible.
;;
;; Previously this was on emacs-startup-hook, which fires BEFORE
;; window-setup-hook.  That caused the failsafe to prematurely close
;; the proxy and reveal the frame before post-setup ran, leading to
;; double window transitions and a crash in _NSWindowTransformAnimation
;; dealloc during the Core Animation transaction flush.
(add-hook 'window-setup-hook
          (lambda ()
            (let ((vis (frame-visible-p (selected-frame))))
              (hyalo--boot-log (format "failsafe: frame-visible=%s" vis))
              (when (and (display-graphic-p) (not vis))
                (hyalo--boot-log "FAILSAFE: revealing frame")
                (when (fboundp 'hyalo-loading-done)
                  (hyalo--boot-log "FAILSAFE: calling hyalo-loading-done")
                  (condition-case err
                      (hyalo-loading-done)
                    (error (hyalo--boot-log
                            (format "FAILSAFE: hyalo-loading-done error: %s"
                                    (error-message-string err))))))
                (hyalo--boot-log "FAILSAFE: calling make-frame-visible")
                (condition-case err
                    (make-frame-visible)
                  (error (hyalo--boot-log
                          (format "FAILSAFE: make-frame-visible error: %s"
                                  (error-message-string err))))))))
          90)

(hyalo--boot-log "early-init complete")

;;; early-init.el ends here
