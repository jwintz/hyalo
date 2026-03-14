;;; init-appearance.el --- Visual appearance -*- lexical-binding: t; -*-

;; Fonts, icons, themes, and visual configuration for hyalo standalone.
;; Theme stack: modus-themes -> nano-themes (nano-light/nano-dark)
;; Optional: ef-themes, iota-dimmer

;;; Code:

;;;; Fonts

(defvar hyalo-font-mono "SF Mono"
  "Default monospace font family.")

(defvar hyalo-font-prose "Lilex"
  "Default variable-pitch (prose) font family.")

(defvar hyalo-font-height 110
  "Default font height (1/10th of a point).")

(defvar hyalo-font-weight 'light
  "Default font weight.")

;; Set font preferences before loading themes
(set-face-attribute 'default nil
                    :family hyalo-font-mono
                    :height hyalo-font-height
                    :weight hyalo-font-weight)
(set-face-attribute 'variable-pitch nil
                    :family hyalo-font-prose
                    :height hyalo-font-height
                    :weight hyalo-font-weight)
(set-face-attribute 'fixed-pitch nil
                    :family hyalo-font-mono
                    :height hyalo-font-height
                    :weight hyalo-font-weight)

;;;; Line Spacing

;; SF Mono 11pt has ~13px line height. Adding 0.15 ratio (~2px) gives
;; a 1.15x ratio matching Xcode/VS Code density for code editing.
(setq-default line-spacing 0.15)

;;;; Frame Defaults

(setq default-frame-alist
      '((min-height . 1)
        (height     . 45)
        (min-width  . 1)
        (width      . 81)))

;; Transparent background for Liquid Glass pass-through.
;; Only on macOS (ns).
(when (eq window-system 'ns)
  (push '(alpha-background . 0.0) default-frame-alist)
  (push '(ns-alpha-elements . (ns-alpha-default ns-alpha-glyphs))
        default-frame-alist))
(setq widget-image-enable nil)

;;;; Fringe Configuration

;; Fringes must be visible for gutter indicators (diff-hl, flycheck, etc.)
(when (display-graphic-p)
  (setq-default left-fringe-width 8
                right-fringe-width 8)
  (setq-default fringes-outside-margins t)
  ;; Apply to current frame if already created
  (when (frame-live-p (selected-frame))
    (set-frame-parameter nil 'left-fringe 8)
    (set-frame-parameter nil 'right-fringe 8)))

;;;; Theme Dependencies

;; modus-themes: accessibility-focused base theme engine (required by nano-themes)
(use-package modus-themes
  :ensure t
  :demand t
  :config
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t))

;; ef-themes: additional theme collection by Protesilaos.
;; Deferred to idle — only needed when switching to an ef-* theme.
;; modus-themes-include-derivatives-mode is enabled on load so ef themes
;; are registered in modus-themes-registered-items when needed.
(use-package ef-themes
  :ensure t
  :defer t
  :commands (ef-themes-select ef-themes-toggle)
  :init
  (run-with-idle-timer 2 nil
                       (lambda ()
                         (require 'ef-themes)
                         (modus-themes-include-derivatives-mode 1))))

;;;; Nano Themes (modus-based, merged into lisp/)

;; nano-themes.el, nano-light-theme.el, nano-dark-theme.el live in lisp/
;; which is already on load-path. Add it to custom-theme-load-path so
;; load-theme can find the -theme.el files.
(add-to-list 'custom-theme-load-path
             (expand-file-name "lisp" emacs-config-dir))

(use-package nano-themes
  :ensure nil
  :config
  ;; Add palette overrides for better magit diff readability
  ;; Nano themes use semantic colors (green/red/yellow) but modus-themes
  ;; expects specific diff color names. Map them for proper contrast.
  ;; Dark theme: muted backgrounds with semantic foregrounds
  (setq nano-dark-palette-overrides
        '((bg-added          "#2d4a3e")   ; dark muted green background
          (bg-added-faint    "#253d32")   ; slightly darker
          (bg-added-refine   "#3d5a4e")   ; highlight version
          (fg-added          "#a6e3a1")   ; nano green (semantic)
          (fg-added-intense  "#7bc77f")   ; brighter for emphasis
          (bg-removed        "#4a2d35")   ; dark muted red background
          (bg-removed-faint  "#3d2529")   ; slightly darker
          (bg-removed-refine "#5a3d45")   ; highlight version
          (fg-removed        "#f38ba8")   ; nano red (semantic)
          (fg-removed-intense "#e06c84")  ; brighter for emphasis
          (bg-changed        "#4a452d")   ; dark muted yellow background
          (bg-changed-faint  "#3d3925")   ; slightly darker
          (fg-changed        "#f9e2af")   ; nano yellow (semantic)
          (fg-changed-intense "#e8d49e"))  ; brighter for emphasis
        )
  ;; Light theme: light backgrounds with darker foregrounds
  (setq nano-light-palette-overrides
        '((bg-added          "#d8f8d1")   ; light green background
          (bg-added-faint    "#e8fce8")   ; very light
          (bg-added-refine   "#c8f0c1")   ; slightly darker
          (fg-added          "#006000")   ; dark green foreground
          (fg-added-intense  "#004000")   ; darker for emphasis
          (bg-removed        "#f8d8d5")   ; light red background
          (bg-removed-faint  "#fce8e8")   ; very light
          (bg-removed-refine "#f0c8c1")   ; slightly darker
          (fg-removed        "#8f1313")   ; dark red foreground
          (fg-removed-intense "#600000")  ; darker for emphasis
          (bg-changed        "#f8f0d0")   ; light yellow background
          (bg-changed-faint  "#fcf8e8")   ; very light
          (fg-changed        "#553d00")   ; dark yellow/brown foreground
          (fg-changed-intense "#332200"))  ; darker for emphasis
        ))

;;;; Hyalo Theme (theme sync with Swift appearance)

(require 'hyalo-themes)
(hyalo-theme-setup)

;;;; Terminal Transparency (iota-theme-transparent)

;; Only enable in terminal mode (-nw)
;; iota-theme-transparent removes background colors from all faces
;; except excluded ones (selection, completion, etc.) to allow
;; terminal transparency to show through.
;; Use initial-window-system instead of display-graphic-p because
;; the latter returns nil during early init before frames are created.
(when (not initial-window-system)
  (require 'iota-theme-transparent)
  (iota-theme-transparent-mode 1))

;;;; Icons

;; Icons aren't visible at startup (no files open yet).
;; Defer to first buffer switch for ~10-15ms savings.
(use-package nerd-icons
  :ensure t
  :defer t
  :commands (nerd-icons-icon-for-file nerd-icons-icon-for-dir
             nerd-icons-icon-for-mode nerd-icons-icon-for-buffer)
  :init
  (add-hook 'hyalo-first-buffer-hook
            (lambda ()
              (require 'nerd-icons)))
  :config
  (setf (cdr (assoc ".?" nerd-icons-dir-icon-alist))
        '(nerd-icons-faicon "nf-fa-folder"))
  (setq nerd-icons-default-file-icon
        '(nerd-icons-faicon "nf-fa-file")))

;;;; Highlighting

(use-package hl-line
  :ensure nil)

;;;; Lin (enhanced hl-line for selection UIs)

(defface lin-violet
  '((default :foreground unspecified :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#c5beda")
    (((class color) (min-colors 88) (background dark))
     :background "#655594")
    (t :background "magenta"))
  "Violet face for `lin-face', matching vscodet accent-background."
  :group 'lin-faces)

(defface lin-violet-override-fg
  '((default :inherit lin-violet)
    (((background light))
     :foreground "black")
    (((background dark))
     :foreground "white"))
  "Like `lin-violet' but also sets a foreground."
  :group 'lin-faces)

(use-package lin
  :ensure t
  :custom
  (lin-face 'lin-violet)
  :config
  (lin-global-mode 1))

;;;; Inactive Window Dimming

(use-package iota-dimmer
  :ensure nil
  :custom
  (iota-dimmer-fraction 0.40)
  :config
  (iota-dimmer-mode 1))

;;;; Minimap

(use-package hide-mode-line
  :ensure t)

(use-package demap
  :ensure t
  :defer t
  :commands (demap-toggle demap-open demap-close)
  :custom
  (demap-minimap-window-side 'right)
  (demap-minimap-window-width 20)
  :config
  (require 'hyalo-minimap)
  (add-hook 'demap-minimap-construct-hook #'hyalo-minimap-setup)
  (add-hook 'demap-minimap-window-set-hook #'hyalo-minimap-setup))

;;;; Olivetti (centered writing mode)

(use-package olivetti
  :ensure t
  :defer t
  :init
  (autoload 'olivetti-mode "olivetti" "Writing mode" t))

;;;; Mixed Pitch Fonts

;; Enables variable-pitch fonts for prose (headings, text) while keeping
;; fixed-pitch for code elements (indentation, tables, code blocks).
;; Automatically enabled in text-heavy modes.
(use-package mixed-pitch
  :ensure t
  :hook ((markdown-mode . mixed-pitch-mode)
         (org-mode . mixed-pitch-mode)
         (rst-mode . mixed-pitch-mode)
         (adoc-mode . mixed-pitch-mode)
         (Info-mode . mixed-pitch-mode))
  :config
  ;; Faces that should remain fixed-pitch in mixed-pitch buffers
  (dolist (face '(font-lock-comment-face
                  line-number
                  line-number-current-line))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;;;; Splash Screen

(use-package hyalo-splash
  :ensure nil
  :config
  (hyalo-splash-setup))

(provide 'init-appearance)
;;; init-appearance.el ends here
