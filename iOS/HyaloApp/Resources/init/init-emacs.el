;;; init-emacs.el --- Core Emacs settings -*- lexical-binding: t; -*-

;; Cursor, startup, UI, built-in packages.
;; Based on emacs.d/init/init-emacs.el (minimal subset).

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  ;; Startup
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message t)
  (initial-buffer-choice t)
  (initial-scratch-message "")
  ;; Cursor
  (cursor-in-non-selected-windows nil)
  (cursor-type '(hbar . 2))
  (x-stretch-cursor nil)
  ;; Backups
  (make-backup-files nil)
  ;; Scrolling
  ;; Vertical scroll margin: lines of continuity when scrolling
  (scroll-margin 0)
  ;; Horizontal scroll margin: cols from window edge before hscroll
  (hscroll-margin 2)
  ;; Horizontal scroll step: cols to scroll when point moves off-screen
  (hscroll-step 1)
  ;; Conservative scrolling: don't recenter automatically when scrolling far
  ;; Value over 100 prevents auto-recentering; 10 allows recenter if far off-screen
  (scroll-conservatively 10)
  ;; Preserve screen position when scrolling
  (scroll-preserve-screen-position t)
  ;; Disable auto-window-vscroll to reduce cursor lag with tall lines
  (auto-window-vscroll nil)
  ;; Mouse wheel scroll amount: 2 lines default, hscroll with shift
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 2)
  ;; Performance: faster scrolling over unfontified regions
  ;; Brief syntax highlighting inaccuracies may occur but self-correct quickly
  (fast-but-imprecise-scrolling t)
  ;; Performance: skip fontification during input to reduce lag
  (redisplay-skip-fontification-on-input t)
  ;; Performance: skip case-insensitive second pass over auto-mode-alist
  (auto-mode-case-fold nil)
  ;; Performance: disable bidirectional text scanning (LTR-only IDE)
  (bidi-inhibit-bpa t)
  ;; Performance: do not render highlights in non-focused windows
  (highlight-nonselected-windows nil)
  ;; Performance: read 64kb chunks from subprocesses (default 4kb)
  (read-process-output-max (* 64 1024))
  ;; Misc
  (use-short-answers t)
  (use-dialog-box nil)
  (frame-title-format nil)
  :config
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (setq inhibit-compacting-font-caches t)
  ;; Performance: disable bidirectional reordering (macOS-only, LTR-only IDE)
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  (blink-cursor-mode 0)
  (show-paren-mode 1)
  ;; cua-mode variable is read by the built-in Options menu toggle
  ;; but cua-base is not loaded by default; define it to avoid void error.
  (defvar cua-mode nil)
  (if (display-graphic-p)
      (menu-bar-mode t) ;; When nil, focus problem on OSX
    (menu-bar-mode -1))
  ;; Remove continuation indicators in fringe
  (setq-default fringe-indicator-alist
                (delq (assq 'continuation fringe-indicator-alist)
                      fringe-indicator-alist))
  ;; Coding systems
  (set-default-coding-systems 'utf-8)
  ;; macOS-specific scrolling: disable native smooth scroll for ultra-scroll
  (when (eq system-type 'darwin)
    (setq mac-redisplay-dont-reset-vscroll t
          mac-mouse-wheel-smooth-scroll nil)))

;;;; Built-in Packages

(use-package recentf
  :ensure nil
  :defer 1
  :init
  (defvar recentf-mode nil)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-max-saved-items 200)
  :config
  ;; Suppress "Loading .../recentf.eld...done" message
  (advice-add 'recentf-load-list :around
              (lambda (orig-fun &rest args)
                (cl-letf (((symbol-function 'load-file)
                           (lambda (file) (load file nil t))))
                  (apply orig-fun args))))
  (recentf-mode 1))

(use-package saveplace
  :ensure nil
  :defer 1
  :init
  (defvar save-place-mode nil)
  :config
  (save-place-mode 1))

(use-package savehist
  :ensure nil
  :defer 1
  :init
  (defvar savehist-mode nil)
  :config
  (savehist-mode 1))

(use-package autorevert
  :ensure nil
  :defer 1
  :init (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers nil)
  (auto-revert-interval 3))

;;;; Pixel-Perfect Scrolling (ultra-scroll)

;; ultra-scroll provides smooth pixel-precise scrolling on macOS
;; Requires Emacs 29+ with pixel-scroll-precision-mode
(use-package ultra-scroll
  :ensure t
  :vc (:url "https://github.com/jdtsmith/ultra-scroll" :rev :newest)
  :when (fboundp 'pixel-scroll-precision-mode)
  :hook (emacs-startup . ultra-scroll-mode)
  :config
  ;; Hide modes that may conflict with smooth scrolling during scroll
  (add-hook 'ultra-scroll-hide-functions #'hl-todo-mode)
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode))

;;;; Smooth Keyboard Scrolling (good-scroll)

;; good-scroll adds animated interpolation for keyboard-driven scrolling
;; (Page Up/Down, C-v/M-v, etc.) - complements ultra-scroll which handles
;; trackpad/mouse input. Converts line-based scrolling to pixel-based.
(use-package good-scroll
  :ensure t
  :hook (emacs-startup . good-scroll-mode)
  :config
  ;; Coexist with ultra-scroll: good-scroll handles interpolation only,
  ;; ultra-scroll handles mouse wheel smoothing. Prevent good-scroll from
  ;; interfering with mouse wheel by using line-based functions.
  (add-hook 'good-scroll-mode-hook
            (lambda ()
              (if good-scroll-mode
                  (setq mwheel-scroll-up-function #'scroll-up
                        mwheel-scroll-down-function #'scroll-down))))

  ;; Fix out-of-bounds errors when commands call scroll functions programmatically
  ;; (e.g., ledger-mode calling move-end-of-line which triggers scrolling)
  (defun hyalo/good-scroll--point-at-top-p ()
    "Check if point is at top of window, handling narrowed buffers."
    (save-restriction
      (widen)
      (<= (line-number-at-pos (max (point) (point-min)) t)
          (1+ (line-number-at-pos (min (window-start) (point-max)) t)))))

  ;; Override good-scroll's point-at-top check with our safer version
  (advice-add 'good-scroll--point-at-top-p :override #'hyalo/good-scroll--point-at-top-p)

  ;; Convert line count to pixel height for good-scroll movement
  (defun hyalo/good-scroll--convert-line-to-step (line)
    "Convert LINE count to pixel height for scrolling."
    (cond ((integerp line) (* line (line-pixel-height)))
          ((or (null line) (memq '- line))
           (- (good-scroll--window-usable-height)
              (* next-screen-context-lines (line-pixel-height))))
          (t (line-pixel-height))))

  ;; Advise scroll-up to use good-scroll interpolation when active
  (defun hyalo/good-scroll--scroll-up (orig-fn &optional arg)
    "Around advice for scroll-up using good-scroll interpolation."
    (if good-scroll-mode
        (good-scroll-move (hyalo/good-scroll--convert-line-to-step arg))
      (funcall orig-fn arg)))

  ;; Advise scroll-down to use good-scroll interpolation when active
  (defun hyalo/good-scroll--scroll-down (orig-fn &optional arg)
    "Around advice for scroll-down using good-scroll interpolation."
    (if good-scroll-mode
        (good-scroll-move (- (hyalo/good-scroll--convert-line-to-step arg)))
      (funcall orig-fn arg)))

  (advice-add 'scroll-up :around #'hyalo/good-scroll--scroll-up)
  (advice-add 'scroll-down :around #'hyalo/good-scroll--scroll-down))

(provide 'init-emacs)
;;; init-emacs.el ends here
