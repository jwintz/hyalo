;;; nano-dark-theme.el --- N Λ N O dark theme (Lithos/Zinc+Violet dichromatic) -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-theme
;; Keywords: faces, theme, accessibility

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Dichromatic dark theme based on the Lithos design system:
;; - Neutral scale: Tailwind Zinc (macOS-like grays)
;; - Accent: Violet (#A58AF9 primary) — the only chromatic hue
;; - Emphasis through weight (bold) and shade, not color variety
;;
;; Diagnostic/diff colors (red, green, yellow) are retained for
;; semantic necessity but kept muted. All syntax highlighting uses
;; only zinc grays and violet shades.
;;
;; Palette reference (vscodet dark):
;;   bg: #1e1e1e  fg: #ffffff  accent: #A68AF9
;;   secondary: #dcd3f8  strings: #ede8fc  comments: #808080

;;; Code:

(require 'nano-themes)

;; Tailwind Zinc scale:
;; 50:#fafafa 100:#f4f4f5 200:#e4e4e7 300:#d4d4d8 400:#a1a1aa
;; 500:#71717a 600:#52525b 700:#3f3f46 800:#27272a 900:#18181b 950:#09090b
;;
;; Violet scale (Lithos):
;; 50:#f5f3ff 100:#ede9fe 200:#ddd6fe 300:#c4b5fd 400:#A58AF9
;; 500:#8b5cf6 600:#7c3aed 700:#6d28d9 800:#5b21b6 900:#4c1d95 950:#2e1065

(defconst nano-dark-palette-partial
  '(;; Core surfaces — Zinc dark end
    (cursor "#A58AF9")                ; violet-400 — accent cursor
    (bg-main "#1e1e1e")               ; vscodet editor background
    (bg-dim "#27272a")                ; zinc-800
    (bg-alt "#3f3f46")                ; zinc-700
    (fg-main "#f4f4f5")              ; zinc-100
    (fg-dim "#71717a")                ; zinc-500 — comments, secondary
    (fg-alt "#a1a1aa")                ; zinc-400 — tertiary
    (bg-active "#52525b")             ; zinc-600
    (bg-inactive "#18181b")           ; zinc-900
    (border "#3f3f46")                ; zinc-700

    ;; Dichromatic: violet is the ONLY chromatic hue for syntax
    ;; Cyan slots → violet shades (secondary/tertiary code elements)
    (cyan "#c4b5fd")                  ; violet-300
    (cyan-warmer "#dcd3f8")           ; vscodet secondary (types/keywords)
    (cyan-cooler "#A58AF9")           ; violet-400 accent
    (cyan-faint "#8b5cf6")            ; violet-500

    ;; Red — semantic only (errors, invalid, diff deleted)
    (red "#f38ba8")                   ; muted rose (from vscodet)
    (red-warmer "#f38ba8")
    (red-cooler "#e06c84")
    (red-faint "#c9637a")

    ;; Green — semantic only (diff inserted, success)
    (green "#a6e3a1")                 ; muted green (from vscodet)
    (green-warmer "#a6e3a1")
    (green-cooler "#8fd694")
    (green-faint "#7ac97f")

    ;; Yellow — semantic only (warnings, modified)
    (yellow "#f9e2af")                ; muted amber (from vscodet)
    (yellow-warmer "#f9e2af")
    (yellow-cooler "#e8d49e")
    (yellow-faint "#d5c68d")

    ;; Blue slots → violet shades (dichromatic: no separate blue hue)
    (blue "#A58AF9")                  ; violet-400 primary accent
    (blue-warmer "#c4b5fd")           ; violet-300
    (blue-cooler "#8b5cf6")           ; violet-500
    (blue-faint "#7c3aed")            ; violet-600

    ;; Magenta slots → violet shades (all one hue family)
    (magenta "#dcd3f8")               ; vscodet secondary
    (magenta-warmer "#ede8fc")        ; vscodet strings (lightest violet)
    (magenta-cooler "#c4b5fd")        ; violet-300
    (magenta-faint "#A58AF9")         ; violet-400

    ;; Line highlight and completion — vscodet accent-background
    (bg-hl-line "#655594")            ; vscodet dark accent-background
    (bg-completion "#655594")         ; unified with hl-line for vertico

    ;; Mode line — thin separator line, not a bar
    (bg-mode-line-active "#27272a")   ; zinc-800 — matches bg-dim
    (fg-mode-line-active "#a1a1aa")   ; zinc-400
    (border-mode-line-active "#3f3f46") ; zinc-700
    (bg-mode-line-inactive "#1e1e1e") ; same as bg-main
    (fg-mode-line-inactive "#52525b") ; zinc-600
    (border-mode-line-inactive "#27272a"))) ; zinc-800

(defconst nano-dark-palette-mappings-partial
  '((err red)
    (warning yellow)
    (info green)

    ;; Dichromatic syntax: fg-main for plain code, violet shades for emphasis
    (fg-link cyan-cooler)             ; violet-400 links
    (fg-link-visited magenta-faint)   ; violet-400 visited
    (name cyan-warmer)                ; secondary violet — identifiers
    (keybind cyan-cooler)             ; violet-400
    (identifier fg-dim)               ; zinc-500 — subdued
    (fg-prompt fg-main)

    ;; Code faces — dichromatic treatment:
    ;; bold+fg-main for structural keywords/operators
    ;; violet shades for types/keywords/decorators
    ;; fg-main for variables, fg-dim for comments
    (builtin magenta)                 ; secondary violet
    (comment fg-dim)                  ; zinc-500
    (constant cyan)                   ; violet-300
    (fnname fg-main)                  ; plain white — functions are structure
    (keyword magenta)                 ; secondary violet (bold via modus)
    (preprocessor magenta-faint)      ; violet-400
    (docstring fg-dim)                ; zinc-500
    (string magenta-warmer)           ; lightest violet (#ede8fc)
    (type cyan-warmer)                ; secondary violet (#dcd3f8)
    (variable fg-alt)                 ; zinc-400

    (accent-0 blue)                   ; violet-400
    (accent-1 magenta)                ; secondary violet
    (accent-2 cyan)                   ; violet-300
    (accent-3 green)                  ; kept for diff/status

    (date-common cyan-cooler)
    (date-deadline red)
    (date-deadline-subtle red-faint)
    (date-event fg-alt)
    (date-holiday magenta)
    (date-now fg-main)
    (date-range fg-alt)
    (date-scheduled yellow)
    (date-scheduled-subtle yellow-faint)
    (date-weekday fg-main)
    (date-weekend red-faint)

    (fg-prose-code magenta-faint)
    (prose-done green)
    (fg-prose-macro cyan-cooler)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table fg-alt)
    (prose-table-formula err)
    (prose-tag magenta-faint)
    (prose-todo red)
    (fg-prose-verbatim cyan)

    (mail-cite-0 cyan-cooler)
    (mail-cite-1 magenta)
    (mail-cite-2 cyan)
    (mail-cite-3 fg-alt)
    (mail-part magenta-faint)
    (mail-recipient cyan-warmer)
    (mail-subject magenta-warmer)
    (mail-other fg-alt)

    (bg-search-static bg-dim)
    (bg-search-current bg-alt)
    (bg-search-lazy bg-dim)
    (bg-search-replace bg-alt)

    (bg-search-rx-group-0 bg-cyan-subtle)
    (bg-search-rx-group-1 bg-blue-subtle)
    (bg-search-rx-group-2 bg-magenta-subtle)
    (bg-search-rx-group-3 bg-green-subtle)

    ;; Headings — dichromatic: violet shades + zinc, no semantic colors
    (fg-heading-1 fg-main)            ; bold white
    (fg-heading-2 magenta)            ; secondary violet (#dcd3f8)
    (fg-heading-3 cyan)               ; violet-300 (#c4b5fd)
    (fg-heading-4 fg-alt)             ; zinc-400 (#a1a1aa)
    (fg-heading-5 magenta-faint)      ; violet-400 (#A58AF9)
    (fg-heading-6 fg-dim)             ; zinc-500 (#71717a)

    (rainbow-0 cyan-cooler)
    (rainbow-1 magenta)
    (rainbow-2 cyan)
    (rainbow-3 magenta-warmer)
    (rainbow-4 cyan-warmer)
    (rainbow-5 magenta-faint)
    (rainbow-6 fg-alt)
    (rainbow-7 fg-dim)
    (rainbow-8 magenta-cooler)))

(defcustom nano-dark-palette-overrides nil
  "Overrides for `nano-dark-palette'.

Mirror the elements of the aforementioned palette, overriding their
value.

For overrides that are shared across all of the N Λ N O themes,
refer to `nano-themes-common-palette-overrides'.

To preview the palette entries, use `nano-themes-preview-colors' or
`nano-themes-preview-colors-current' (read the documentation for
further details)."
  :group 'nano-themes
  :package-version '(nano-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(nano-themes) Palette overrides"))

(defconst nano-dark-palette
  (modus-themes-generate-palette
   nano-dark-palette-partial
   'cool
   nil
   nano-dark-palette-mappings-partial)
  "Generated palette for the N Λ N O dark theme.")

;; Mode-line as thin separator in GUI: height 0.1, fg=bg (invisible text),
;; underline in bg-dim color, no box.  In TTY: default mode-line with underline.
(defvar nano-dark-custom-faces
  '(`(mode-line
      ((((type graphic))
        :height 0.1
        :foreground ,bg-main
        :background ,bg-main
        :underline ,bg-dim
        :overline nil
        :box nil
        :inherit nil)
       (t :underline ,border-mode-line-active :box nil)))
    `(mode-line-active
      ((((type graphic))
        :height 0.1
        :foreground ,bg-main
        :background ,bg-main
        :underline ,bg-dim
        :overline nil
        :box nil
        :inherit nil)
       (t :underline ,border-mode-line-active :box nil)))
    `(mode-line-inactive
      ((((type graphic))
        :height 0.1
        :foreground ,bg-main
        :background ,bg-main
        :underline ,bg-dim
        :overline nil
        :box nil
        :inherit nil)
       (t :underline ,border-mode-line-inactive :box nil)))
    ;; Window dividers — match border color (zinc-700)
    `(window-divider ((t :foreground ,border)))
    `(window-divider-first-pixel ((t :foreground ,border)))
    `(window-divider-last-pixel ((t :foreground ,border)))
    `(vertical-border ((t :foreground ,border))))
  "Custom face overrides for the N Λ N O dark theme.")

(modus-themes-theme
 'nano-dark
 'nano-themes
 "N Λ N O dark theme — Lithos dichromatic (Zinc + Violet)."
 'dark
 'nano-dark-palette
 nil
 'nano-dark-palette-overrides
 'nano-dark-custom-faces)

;;; nano-dark-theme.el ends here
