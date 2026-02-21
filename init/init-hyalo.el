;;; init-hyalo.el --- Hyalo: macOS Liquid Glass integration -*- lexical-binding: t; -*-

;; Module loading, window controller setup, keybindings, channels.

;;; Code:

;;;; macOS Settings

(use-package ns-win
  :ensure nil
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  ;; frame-resize-pixelwise and frame-inhibit-implied-resize are set in early-init.el
  (set-frame-parameter nil 'internal-border-width 0)
  (add-to-list 'default-frame-alist '(internal-border-width . 0))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (set-frame-parameter nil 'ns-transparent-titlebar t)
  ;; Unbind suspend-frame (useless in GUI)
  (general-unbind "C-z" "C-x C-z"))

;;;; Hyalo Core Module

(use-package hyalo
  :ensure nil
  :if (eq window-system 'ns)
  :custom
  (hyalo-auto-build t)
  :config
  (hyalo-load)
  ;; Decorate the window immediately after the module loads.
  ;; This installs the SwiftUI chrome (toolbar, navigator, inspector)
  ;; BEFORE the rest of init.el runs.  If later init steps block
  ;; (e.g., package-vc-install for missing packages), the user sees
  ;; the Hyalo shell with a placeholder rather than a raw Emacs frame.
  (when (hyalo-available-p)
    (require 'hyalo-window)
    (hyalo-window--early-setup)))

;;;; Hyalo Window Controller

;; Keybindings: C-c t {n,i,u,m} for navigator, inspector, utility, minimap
(when (and initial-window-system (hyalo-available-p))
  (with-eval-after-load 'general
    (leader-def
      "tn" '(hyalo-toggle-navigator :wk "navigator")
      "ti" '(hyalo-toggle-inspector :wk "inspector")
      "tu" '(hyalo-toggle-utility-area :wk "utility area")
      "tm" '(demap-toggle :wk "minimap")))
  ;; Setup window controller after first frame is ready
  (add-hook 'window-setup-hook #'hyalo-window-setup)
  (when (fboundp 'hyalo--boot-log)
    (hyalo--boot-log "init-hyalo: window-setup-hook registered")))

;;;; Hyalo Status Bar

(use-package hyalo-status
  :ensure nil
  :if (eq window-system 'ns))

;;;; Hyalo Navigator

(use-package hyalo-navigator
  :ensure nil
  :if (eq window-system 'ns))

;;;; Hyalo Source Control

(use-package hyalo-source-control
  :ensure nil
  :if (eq window-system 'ns))

;;;; Hyalo Menu

(use-package hyalo-menu
  :ensure nil
  :if (display-graphic-p)
  :demand t)

;;;; Hyalo System (fork-emacs, reveal-in-finder, share, emoji-picker)

(use-package hyalo-system
  :ensure nil
  :if (eq system-type 'darwin)
  :commands (fork-emacs hyalo-reveal-in-finder hyalo-share hyalo-show-emoji-picker)
  :general
  (leader-def
    "fE" '(fork-emacs :wk "fork emacs")
    "l" '(:ignore t :wk "macOS")
    "lr" '(hyalo-reveal-in-finder :wk "reveal in finder")
    "ls" '(hyalo-share :wk "share...")
    "le" '(hyalo-show-emoji-picker :wk "emoji picker")
    "lm" '(hyalo-toggle-macos-menu-bar :wk "toggle menu bar")))

;;;; Window Dividers - thin 1px

(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places '(right bottom))
(window-divider-mode 1)

;;;; Hyalo Native Compilation Status

(use-package hyalo-compile
  :ensure nil
  :if (eq window-system 'ns)
  :config
  (hyalo-compile-setup))

;;;; Hyalo Package Management

(use-package hyalo-package
  :ensure nil
  :if (eq window-system 'ns)
  :config
  (hyalo-package-setup))

;;;; Hyalo Activities (tab-bar / activities.el breadcrumb)

(use-package init-activities
  :ensure nil
  :if (eq window-system 'ns)
  :demand t)

;;;; Hyalo Keycast

(use-package hyalo-keycast
  :ensure nil
  :if (eq window-system 'ns)
  :commands (hyalo-keycast-mode))

;;;; Hyalo Appearance

(use-package hyalo-appearance
  :ensure nil
  :if (eq window-system 'ns)
  :config
  ;; Push theme-derived settings to Swift (background color, dividers, fringe).
  ;; Opacity and material persist in UserDefaults, managed by the Swift panel.
  (hyalo-appearance-sync))

(provide 'init-hyalo)
;;; init-hyalo.el ends here
