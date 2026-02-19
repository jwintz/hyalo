;;; hyalo-themes.el --- Theme management for Hyalo -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Theme management for hyalo standalone.
;; Uses modus-based nano-themes (nano-light / nano-dark) via `load-theme'.
;;
;; Handles:
;; - Default light theme: nano-light
;; - Default dark theme: nano-dark
;; - System appearance changes load the appropriate default theme
;; - Loading a dark theme switches window appearance to dark
;; - Loading a light theme switches window appearance to light
;; - Terminal palette synchronization with Swift
;;
;; The old nano-base-colors / nano-faces / nano-theme variable system
;; is replaced by standard Emacs `load-theme' + `enable-theme-functions'.

;;; Code:

(require 'cl-lib)
(require 'json)

;; hyalo.el may not be loaded yet (init-appearance loads before init-hyalo).
;; Use fboundp guards for all hyalo module functions.

(defgroup hyalo-theme nil
  "Theme settings for hyalo."
  :group 'hyalo
  :prefix "hyalo-theme-")

(defcustom hyalo-theme-light 'nano-light
  "Theme to use for light appearance."
  :type 'symbol
  :group 'hyalo-theme)

(defcustom hyalo-theme-dark 'nano-dark
  "Theme to use for dark appearance."
  :type 'symbol
  :group 'hyalo-theme)

(defvar hyalo-theme--loading nil
  "Non-nil while `hyalo-theme-sync' is loading a theme.
Prevents re-entrant calls from NS appearance change events.")

(defvar hyalo-theme--user-theme-p nil
  "Non-nil when the user explicitly loaded a non-default theme.
Set by `enable-theme-functions' when the loaded theme is not one of
the configured defaults.  Prevents `hyalo-theme-sync' from overriding
the user's choice on subsequent NS appearance change events.")

;;; System Appearance Detection (TTY)

(defun hyalo-theme--system-appearance-tty ()
  "Detect macOS system appearance from terminal.
Uses `defaults read` to check AppleInterfaceStyle.
Returns 'light or 'dark."
  (condition-case nil
      (if (and (eq system-type 'darwin)
               (executable-find "defaults"))
          (let ((output (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "defaults" nil t nil
                                          "read" "-g" "AppleInterfaceStyle")))))
            (if (string-match-p "Dark" output)
                'dark
              'light))
        'dark)
    (error 'dark)))

;;; Theme Variant Detection

(defun hyalo-theme--dark-p (theme)
  "Return non-nil if THEME is a dark theme.
Checks modus-themes metadata first, then falls back to frame background mode."
  (cond
   ;; modus/nano themes declare their type
   ((memq theme (bound-and-true-p modus-themes--dark-themes)) t)
   ((memq theme (bound-and-true-p nano-themes-dark-themes)) t)
   ;; ef-themes encode variant in name
   ((and (symbolp theme)
         (string-match-p "-dark\\'" (symbol-name theme))) t)
   ;; modus/nano light lists
   ((memq theme (bound-and-true-p modus-themes--light-themes)) nil)
   ((memq theme (bound-and-true-p nano-themes-light-themes)) nil)
   ((and (symbolp theme)
         (string-match-p "-light\\'" (symbol-name theme))) nil)
   ;; Fallback: check frame background-mode after theme is loaded
   (t (eq (frame-parameter nil 'background-mode) 'dark))))

;;; Appearance Sync on Theme Load

(defun hyalo-theme--on-enable (theme)
  "Handle theme enable event for THEME.
Pushes the correct appearance (light/dark) to the Swift window.
Detects when the user loads a non-default theme and prevents
`hyalo-theme-sync' from overriding it.
Terminal transparency is handled by iota-theme-transparent-mode
which removes backgrounds from all faces automatically."
  (when (and (not hyalo-theme--loading)
             (not (memq theme '(use-package))) ; ignore non-visual themes
             (custom-theme-p theme))
    ;; Track whether this is a user-chosen non-default theme
    (setq hyalo-theme--user-theme-p
          (not (memq theme (list hyalo-theme-light hyalo-theme-dark))))
    ;; TTY: terminal transparency handled by iota-theme-transparent-mode
    ;; which removes backgrounds from all faces automatically
    (when (and (fboundp 'hyalo-available-p) (hyalo-available-p))
      (let ((dark (hyalo-theme--dark-p theme)))
        (when (fboundp 'hyalo-set-workspace-appearance)
          (hyalo-set-workspace-appearance (if dark "dark" "light")))
        ;; Push theme name to appearance panel
        (when (fboundp 'hyalo-set-current-theme-name)
          (hyalo-set-current-theme-name (symbol-name theme)))
        ;; Sync background color with new theme (opacity preserved from UserDefaults)
        (unless (featurep 'hyalo-appearance)
          (when (fboundp 'hyalo-set-background-color)
            (let* ((bg (or (face-background 'default nil 'default) "#1e1e1e"))
                   (hex (if (string-prefix-p "#" bg) bg
                          (apply #'format "#%02x%02x%02x"
                                 (mapcar (lambda (c) (/ c 256))
                                         (color-values bg))))))
              (hyalo-set-background-color hex))))
        ;; Send terminal palette
        (hyalo-theme-send-palette)
        ;; Send color theme for SwiftUI views
        (hyalo-theme-send-color-theme)))))

;;; System Appearance Change Handler

(defun hyalo-theme-sync (appearance)
  "Sync theme with system APPEARANCE.
APPEARANCE is `light' or `dark'.
In GUI mode, called by `ns-system-appearance-change-functions'.
In TTY mode, called during setup based on `defaults read' detection.
Loads the configured default theme for that appearance.
Skips if the user has explicitly loaded a non-default theme, or if
a theme load is already in progress.
Background transparency in TTY mode is handled by iota-theme-transparent-mode."
  (let ((theme (pcase appearance
                 ('light hyalo-theme-light)
                 ('dark hyalo-theme-dark)
                 (_ nil))))
    (when (and theme
               (not hyalo-theme--loading)
               (not hyalo-theme--user-theme-p))
      (unwind-protect
          (let ((ns-system-appearance-change-functions nil))
            (setq hyalo-theme--loading t)
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme theme t)
            ;; hyalo-theme--on-enable is skipped because hyalo-theme--loading
            ;; is t.  Perform its work here: push appearance, theme name,
            ;; terminal palette, color theme, and divider colors to Swift.
            (when (and (fboundp 'hyalo-available-p) (hyalo-available-p))
              (let ((dark (hyalo-theme--dark-p theme)))
                (when (fboundp 'hyalo-set-workspace-appearance)
                  (hyalo-set-workspace-appearance (if dark "dark" "light")))
                (when (fboundp 'hyalo-set-current-theme-name)
                  (hyalo-set-current-theme-name (symbol-name theme))))
              (hyalo-theme-send-palette)
              (hyalo-theme-send-color-theme))
            ;; Apply divider, fringe, mode-line, and background colors.
            ;; enable-theme-functions fires hyalo-appearance--on-theme-load
            ;; from load-theme above, but belt-and-suspenders: ensure
            ;; appearance is consistent even if hook ordering changes.
            (when (fboundp 'hyalo-appearance--apply-divider-color)
              (hyalo-appearance--apply-divider-color)
              (hyalo-appearance--apply-background-color))
            ;; Force immediate redisplay so the Emacs view reflects
            ;; the new theme even when it is not the key window
            ;; (e.g. after an appearance panel click).
            ;; load-theme sets FRAME_GARBAGED via recompute_basic_faces;
            ;; redisplay clears the flag and recomputes the display,
            ;; then hyalo-force-redisplay flushes the IOSurface to VRAM.
            (redisplay t)
            (when (fboundp 'hyalo-force-redisplay)
              (hyalo-force-redisplay)))
        (setq hyalo-theme--loading nil)))))

;;; Setup

(defun hyalo-theme-setup ()
  "Setup Hyalo theme hooks and apply initial theme.
Hooks `ns-system-appearance-change-functions' for automatic light/dark switching.
Hooks `enable-theme-functions' for appearance sync on any theme load.
Loads the initial theme matching the current system appearance."

  ;; Hook: sync appearance when any theme is loaded
  (when (boundp 'enable-theme-functions)
    (add-hook 'enable-theme-functions #'hyalo-theme--on-enable))

  ;; Hook: system appearance changes
  (when (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions #'hyalo-theme-sync))

  ;; Load initial theme for current system appearance
  (let ((current (cond
                  ;; GUI: use ns-system-appearance
                  ((and (boundp 'ns-system-appearance) ns-system-appearance)
                   ns-system-appearance)
                  ;; GUI: fallback to hyalo-get-system-appearance
                  ((fboundp 'hyalo-get-system-appearance)
                   (intern (hyalo-get-system-appearance)))
                  ;; TTY: detect via defaults command
                  ((not initial-window-system)
                   (hyalo-theme--system-appearance-tty))
                  ;; Fallback
                  (t 'dark))))
    (hyalo-theme-sync current)))

;;; Terminal Palette

(defun hyalo-theme--color-to-hex (color)
  "Convert Emacs COLOR name or spec to a hex string like \"#RRGGBB\"."
  (when color
    (condition-case nil
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (c) (round (* c 255)))
                       (color-name-to-rgb color)))
      (error nil))))

(defun hyalo-theme-send-palette ()
  "Send the current theme palette to the inspector terminal.
Derives ANSI colors from theme face colors."
  (when (fboundp 'hyalo-set-terminal-palette)
    (let* ((fg (or (hyalo-theme--color-to-hex (face-foreground 'default nil t))
                   "#ffffff"))
           (bg (or (hyalo-theme--color-to-hex (face-background 'default nil t))
                   "#1e1e1e"))
           (cursor-color (or (hyalo-theme--color-to-hex
                              (face-foreground 'font-lock-keyword-face nil t))
                             "#A68AF9"))
           ;; Normal ANSI colors (0-7)
           (normal (vector
                    bg                              ; 0: black
                    (or (hyalo-theme--color-to-hex
                         (face-foreground 'error nil t))
                        "#f38ba8")                  ; 1: red
                    (or (hyalo-theme--color-to-hex
                         (face-foreground 'success nil t))
                        "#a6e3a1")                  ; 2: green
                    (or (hyalo-theme--color-to-hex
                         (face-foreground 'warning nil t))
                        "#f9e2af")                  ; 3: yellow
                    (or (hyalo-theme--color-to-hex
                         (face-foreground 'link nil t))
                        "#89b4fa")                  ; 4: blue
                    (or (hyalo-theme--color-to-hex
                         (face-foreground 'font-lock-keyword-face nil t))
                        "#A68AF9")                  ; 5: magenta
                    (or (hyalo-theme--color-to-hex
                         (face-foreground 'font-lock-constant-face nil t))
                        "#22D3EE")                  ; 6: cyan
                    fg))                            ; 7: white
           ;; Bright ANSI colors (8-15)
           (bright (vector
                    (or (hyalo-theme--color-to-hex
                         (face-foreground 'shadow nil t))
                        "#808080")                  ; 8: bright black
                    "#ff0000"                       ; 9: bright red
                    "#4ADE80"                       ; 10: bright green
                    "#FDE047"                       ; 11: bright yellow
                    "#60A5FA"                       ; 12: bright blue
                    (or (hyalo-theme--color-to-hex
                         (face-foreground 'font-lock-string-face nil t))
                        "#dcd3f8")                  ; 13: bright magenta
                    "#22D3EE"                       ; 14: bright cyan
                    "#ffffff"))                     ; 15: bright white
           (ansi (vconcat normal bright))
           (palette (json-encode
                     `((foreground . ,fg)
                       (background . ,bg)
                       (cursor . ,cursor-color)
                       (ansi . ,ansi)))))
      (hyalo-set-terminal-palette palette))))

;;; Color Theme (SwiftUI semantic colors)

(defun hyalo-theme--modus-color (name)
  "Get hex color for modus palette NAME, or nil if unavailable."
  (when (fboundp 'modus-themes-get-color-value)
    (hyalo-theme--color-to-hex (modus-themes-get-color-value name))))

(defun hyalo-theme-send-color-theme ()
  "Send the current theme's semantic colors to Swift for the active variant.
Extracts colors from Emacs faces and modus palette entries.
The variant (light/dark) is determined from the current theme."
  (when (fboundp 'hyalo-set-color-theme)
    (let* ((dark (eq (frame-parameter nil 'background-mode) 'dark))
           (variant (if dark "dark" "light"))
           (bg (or (hyalo-theme--color-to-hex (face-background 'default nil t))
                   (if dark "#1e1e1e" "#ffffff")))
           (bg-dim (or (hyalo-theme--modus-color 'bg-dim)
                       (if dark "#27272a" "#fafafa")))
           (fg (or (hyalo-theme--color-to-hex (face-foreground 'default nil t))
                   (if dark "#f4f4f5" "#18181b")))
           (fg-dim (or (hyalo-theme--color-to-hex (face-foreground 'shadow nil t))
                       (if dark "#71717a" "#a1a1aa")))
           (accent (or (hyalo-theme--color-to-hex
                        (face-foreground 'font-lock-keyword-face nil t))
                       "#A58AF9"))
           (accent-secondary (or (hyalo-theme--color-to-hex
                                  (face-foreground 'font-lock-type-face nil t))
                                 (if dark "#dcd3f8" "#321685")))
           (err (or (hyalo-theme--color-to-hex (face-foreground 'error nil t))
                    (if dark "#f38ba8" "#D32F2F")))
           (warn (or (hyalo-theme--color-to-hex (face-foreground 'warning nil t))
                     (if dark "#f9e2af" "#F57F17")))
           (ok (or (hyalo-theme--color-to-hex (face-foreground 'success nil t))
                   (if dark "#a6e3a1" "#2E7D32")))
           (lnk (or (hyalo-theme--color-to-hex (face-foreground 'link nil t))
                     (if dark "#89b4fa" "#7c3aed")))
           (str (or (hyalo-theme--color-to-hex
                     (face-foreground 'font-lock-string-face nil t))
                    (if dark "#ede8fc" "#240e67")))
           (cmt (or (hyalo-theme--color-to-hex
                     (face-foreground 'font-lock-comment-face nil t))
                    (if dark "#71717a" "#a1a1aa")))
           (cst (or (hyalo-theme--color-to-hex
                     (face-foreground 'font-lock-constant-face nil t))
                    (if dark "#c4b5fd" "#5E35B1")))
           (bdr (or (hyalo-theme--modus-color 'border)
                    (if dark "#3f3f46" "#d4d4d8")))
           (sel (or (hyalo-theme--modus-color 'bg-hl-line)
                    (if dark "#655594" "#c5beda")))
           (json (json-encode
                  `((background . ,bg)
                    (backgroundDim . ,bg-dim)
                    (foreground . ,fg)
                    (foregroundDim . ,fg-dim)
                    (accent . ,accent)
                    (accentSecondary . ,accent-secondary)
                    (error . ,err)
                    (warning . ,warn)
                    (success . ,ok)
                    (link . ,lnk)
                    (string . ,str)
                    (comment . ,cmt)
                    (constant . ,cst)
                    (border . ,bdr)
                    (selection . ,sel)))))
      (hyalo-set-color-theme variant json))))

(provide 'hyalo-themes)
;;; hyalo-themes.el ends here
