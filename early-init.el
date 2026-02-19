;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Hide frames until they are fully set up (chrome installed, theme synced).
;; hyalo-window--post-setup calls (make-frame-visible) once the IDE shell is
;; ready.  The after-make-frame-functions hook handles subsequent frames.
(push '(visibility . nil) default-frame-alist)

;; Set initial frame geometry early so the frame is already the correct
;; size when hyalo-window--early-setup decorates it.  Without this, the
;; frame starts at Emacs's default size and jumps when init-appearance.el
;; applies the final geometry.
(push '(width . 81) default-frame-alist)
(push '(height . 45) default-frame-alist)

;; Prevent frame resizing during font setup — saves time on NS/macOS.
(setq frame-inhibit-implied-resize t)

;; Reduce load-suffixes: Hyalo does not load .so modules via require
;; (the Swift dylib is loaded explicitly via module-load).  Removing
;; the dynamic module suffix eliminates one stat call per require.
(when (and (boundp 'load-suffixes) (member ".so" load-suffixes))
  (setq load-suffixes (remove ".so" load-suffixes)))

;; Redirect native compilation cache to .local/eln-cache/ so it aligns
;; with user-emacs-directory (set to .local/ in init-bootstrap.el).
;; Without this, Emacs writes .eln files to .local/eln-cache/ but searches
;; in the top-level eln-cache/, causing recompilation on every launch.
;;
;; At early-init.el time, user-emacs-directory still points to the
;; --init-directory value.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name ".local/eln-cache/" user-emacs-directory)))

;;; early-init.el ends here

