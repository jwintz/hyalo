;;; hyalo-appearance.el --- Appearance management -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Appearance management for hyalo standalone.
;; Based on ~/Syntropment/emacs.d/lisp/hyalo-appearance.el.
;;
;; Handles:
;; - System appearance detection (light/dark)
;; - Frame transparency (alpha-background)
;; - Background color sync to Swift (theme-derived)
;; - Window divider color matching Swift panels
;; - Fringe color matching theme
;;
;; Opacity and vibrancy material are managed exclusively by the Swift
;; appearance panel and persisted in UserDefaults.

;;; Code:

(require 'hyalo)

(defgroup hyalo-appearance nil
  "Appearance settings for hyalo."
  :group 'hyalo
  :prefix "hyalo-appearance-")

(defcustom hyalo-appearance-alpha-elements
  '((ns-alpha-default . 0.0)
    (ns-alpha-glyphs  . 0.0)
    (ns-alpha-fringe  . 0.10))
  "Alist of (element . alpha) pairs for per-element transparency.
Each entry is either a bare symbol (uses `alpha-background' as the
alpha value) or a cons cell (SYMBOL . FLOAT) where FLOAT is 0.0-1.0.
The ns-alpha-fringe value is overridden at runtime by the opacity
slider (computed as min(opacity + offset, 1.0)).
Supported elements: ns-alpha-default, ns-alpha-glyphs, ns-alpha-fringe,
ns-alpha-box, ns-alpha-stipple, ns-alpha-relief, ns-alpha-all."
  :type '(repeat (choice symbol (cons symbol number)))
  :group 'hyalo-appearance)

(defcustom hyalo-appearance-fringe-alpha-offset 0.10
  "Offset added to the opacity slider value for fringe alpha.
The fringe alpha is computed as min(opacity + offset, 1.0).
This keeps fringes slightly more visible than the editor background."
  :type 'number
  :group 'hyalo-appearance)

;;; Core Application Functions

(defun hyalo-appearance--apply-background-color ()
  "Push the current theme background color to Swift.
Opacity and material are managed by the Swift appearance panel
and persisted in UserDefaults — Emacs does not touch them."
  (when (hyalo-available-p)
    (let* ((bg (or (face-background 'default nil 'default)
                   "#0c0a09"))
           (hex-color (if (string-prefix-p "#" bg)
                          bg
                        (apply #'format "#%02x%02x%02x"
                               (mapcar (lambda (c) (/ c 256))
                                       (color-values bg))))))
      (when (fboundp 'hyalo-set-background-color)
        (hyalo-set-background-color hex-color)))))

(defun hyalo-appearance--apply-window-appearance (appearance)
  "Apply window APPEARANCE to Swift side."
  (when (hyalo-available-p)
    (when (fboundp 'hyalo-set-workspace-appearance)
      (hyalo-set-workspace-appearance (symbol-name appearance)))))

(defun hyalo-appearance--apply-frame-settings ()
  "Apply frame-level transparency settings."
  (dolist (f (frame-list))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (set-frame-parameter f 'alpha-background 0.0)
      (set-frame-parameter f 'ns-alpha-elements
                           hyalo-appearance-alpha-elements)
      (set-frame-parameter f 'left-fringe 8)
      (set-frame-parameter f 'right-fringe 8)
      (set-frame-parameter f 'internal-border-width 0))))

(defun hyalo-appearance--setup-frame (&optional frame)
  "Apply appearance settings to FRAME (or current frame).
Used as `after-make-frame-functions' hook."
  (let ((f (or frame (selected-frame))))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (set-frame-parameter f 'alpha-background 0.0)
      (set-frame-parameter f 'ns-alpha-elements
                           hyalo-appearance-alpha-elements)
      (set-frame-parameter f 'left-fringe 8)
      (set-frame-parameter f 'right-fringe 8)
      (set-frame-parameter f 'internal-border-width 0))))

(defun hyalo-appearance--update-fringe-alpha (opacity)
  "Update fringe alpha from OPACITY slider value.
Sets ns-alpha-fringe to min(OPACITY + offset, 1.0) in the
`hyalo-appearance-alpha-elements' alist and applies to all frames.
The fringe alpha is derived from the opacity -- not persisted
independently.  The opacity itself lives in UserDefaults on the
Swift side."
  (let* ((fringe-alpha (min (+ opacity hyalo-appearance-fringe-alpha-offset) 1.0))
         (found nil)
         (updated (mapcar (lambda (entry)
                            (if (and (consp entry)
                                     (eq (car entry) 'ns-alpha-fringe))
                                (progn (setq found t)
                                       (cons 'ns-alpha-fringe fringe-alpha))
                              entry))
                          hyalo-appearance-alpha-elements)))
    (unless found
      (setq updated (append updated
                            (list (cons 'ns-alpha-fringe fringe-alpha)))))
    (setq hyalo-appearance-alpha-elements updated)
    (dolist (f (frame-list))
      (when (and (display-graphic-p f) (eq (framep f) 'ns))
        (set-frame-parameter f 'ns-alpha-elements
                             hyalo-appearance-alpha-elements)))
    (redisplay t)))

(defun hyalo-appearance--clear-backgrounds (&optional _theme)
  "Clear face backgrounds that should be transparent.
In TTY mode, also resets the default face background to `unspecified'
so the terminal's native background (and transparency) shows through."
  (unless (display-graphic-p)
    (set-face-background 'default 'unspecified))
  (when (facep 'line-number)
    (set-face-background 'line-number nil))
  (when (facep 'line-number-current-line)
    (set-face-background 'line-number-current-line nil)))

;;; Window Divider Color

(defun hyalo-appearance--apply-divider-color (&optional _theme)
  "Set window divider color to match the current theme.
Uses the `border' palette entry from modus/nano themes, falling back
to a computed subtle color from the default background."
  (when (display-graphic-p)
    (let ((divider-color
           (cond
            ;; modus/nano themes expose colors via modus-themes-with-colors
            ((and (fboundp 'modus-themes-get-color-value)
                  (modus-themes-get-color-value 'border))
             (modus-themes-get-color-value 'border))
            ;; Fallback: compute from background
            (t
             (let ((bg (face-background 'default nil t)))
               (if (eq (frame-parameter nil 'background-mode) 'dark)
                   (color-lighten-name (or bg "#2E3440") 15)
                 (color-darken-name (or bg "#FFFFFF") 10)))))))
      (when divider-color
        (set-face-attribute 'window-divider nil :foreground divider-color)
        (set-face-attribute 'window-divider-first-pixel nil :foreground divider-color)
        (set-face-attribute 'window-divider-last-pixel nil :foreground divider-color)
        (set-face-foreground 'vertical-border divider-color)))))

(defun hyalo-appearance--set-subtle-fringe (&optional _theme)
  "Set fringe face to subtle color matching the theme."
  (when (display-graphic-p)
    (let* ((bg (face-background 'default nil t))
           (fringe-color
            (cond
             ((and (fboundp 'modus-themes-get-color-value)
                   (modus-themes-get-color-value 'bg-dim))
              (modus-themes-get-color-value 'bg-dim))
             (t
              (if (eq (frame-parameter nil 'background-mode) 'dark)
                  (color-lighten-name (or bg "#2E3440") 8)
                (color-darken-name (or bg "#FFFFFF") 3))))))
      (set-face-attribute 'fringe nil
                          :background fringe-color
                          :foreground fringe-color))))

;;; Mode Line as Thin Separator

(defun hyalo-appearance--apply-mode-line (&optional _theme)
  "Style the mode line as a thin separator line (GUI only).
Uses the nano-emacs approach: height 0.1 with foreground matching
background, plus an underline in the border color.  This renders
the mode-line as a 1-2px horizontal rule.
For nano-dark/nano-light, this is already set via custom-faces in the
theme definition.  This function acts as a fallback for other themes.
In TTY mode, resets mode-line background to unspecified so the
terminal background shows through."
  (if (display-graphic-p)
      ;; GUI: nano-emacs thin-line approach
      (let* ((bg (face-background 'default nil t))
             (subtle (or (and (fboundp 'modus-themes-get-color-value)
                              (modus-themes-get-color-value 'border))
                         (face-foreground 'window-divider nil t)
                         "#3f3f46")))
        (set-face-attribute 'mode-line nil
                            :height 0.1
                            :foreground bg
                            :background bg
                            :underline subtle
                            :overline nil
                            :box nil)
        (when (facep 'mode-line-active)
          (set-face-attribute 'mode-line-active nil
                              :height 0.1
                              :foreground bg
                              :background bg
                              :underline subtle
                              :overline nil
                              :box nil))
        (set-face-attribute 'mode-line-inactive nil
                            :height 0.1
                            :foreground bg
                            :background bg
                            :underline subtle
                            :overline nil
                            :inherit nil
                            :box nil))
    ;; TTY: keep default mode-line, just clear background
    (set-face-attribute 'mode-line nil :background 'unspecified)
    (when (facep 'mode-line-active)
      (set-face-attribute 'mode-line-active nil :background 'unspecified))
    (set-face-attribute 'mode-line-inactive nil :background 'unspecified)))

;;; Hook Functions

(defun hyalo-appearance--on-theme-load (theme)
  "Handle theme load event for THEME."
  (hyalo-appearance--apply-frame-settings)
  (hyalo-appearance--clear-backgrounds)
  (hyalo-appearance--apply-divider-color)
  (hyalo-appearance--set-subtle-fringe)
  (hyalo-appearance--apply-mode-line)
  (when (hyalo-available-p)
    (hyalo-appearance--apply-background-color)))

;;; Public API

(defun hyalo-appearance-set-dark ()
  "Switch to dark appearance."
  (interactive)
  (hyalo-appearance--apply-window-appearance 'dark))

(defun hyalo-appearance-set-light ()
  "Switch to light appearance."
  (interactive)
  (hyalo-appearance--apply-window-appearance 'light))

(defun hyalo-appearance-set-auto ()
  "Switch to auto (system) appearance."
  (interactive)
  (hyalo-appearance--apply-window-appearance 'auto))

(defun hyalo-appearance-sync ()
  "Synchronize the current theme with the Hyalo appearance system.
Called during init to push theme-derived settings to Swift.
Opacity and material are NOT pushed — those persist in UserDefaults
and are managed exclusively by the Swift appearance panel."
  (hyalo-appearance--apply-frame-settings)
  (hyalo-appearance--apply-background-color)
  (hyalo-appearance--clear-backgrounds)
  (hyalo-appearance--apply-divider-color)
  (hyalo-appearance--set-subtle-fringe)
  (hyalo-appearance--apply-mode-line)
  ;; Push the effective NSWindow appearance (light/dark chrome) to
  ;; match the loaded theme.  The initial load-theme fires
  ;; enable-theme-functions before the Hyalo module is loaded, so
  ;; hyalo-theme--on-enable skips the push.  Re-do it now.
  (when (and (fboundp 'hyalo-set-workspace-appearance)
             (fboundp 'hyalo-theme--dark-p))
    (let* ((theme (car custom-enabled-themes))
           (dark (and theme (hyalo-theme--dark-p theme))))
      (hyalo-set-workspace-appearance (if dark "dark" "light"))))
  ;; Apply settings to future frames
  (add-hook 'after-make-frame-functions #'hyalo-appearance--setup-frame))

(defun hyalo-appearance-panel ()
  "Open the appearance settings panel."
  (interactive)
  (when (fboundp 'hyalo-show-appearance-panel)
    (hyalo-show-appearance-panel)))

;;; Hooks

(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'hyalo-appearance--on-theme-load))

(add-hook 'after-init-hook #'hyalo-appearance--apply-divider-color)
(add-hook 'after-init-hook #'hyalo-appearance--set-subtle-fringe)
(add-hook 'after-init-hook #'hyalo-appearance--apply-mode-line)

(provide 'hyalo-appearance)
;;; hyalo-appearance.el ends here
