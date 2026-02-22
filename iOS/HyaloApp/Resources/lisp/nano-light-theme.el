;;; nano-light-theme.el --- N Λ N O light theme (Lithos/Zinc+Violet dichromatic) -*- lexical-binding:t -*-

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
;; Dichromatic light theme based on the Lithos design system:
;; - Neutral scale: Tailwind Zinc (macOS-like grays)
;; - Accent: Violet — the only chromatic hue
;; - Emphasis through weight (bold) and shade, not color variety
;;
;; Diagnostic/diff colors (red, green, yellow) are retained for
;; semantic necessity but kept muted. All syntax highlighting uses
;; only zinc grays and violet shades.
;;
;; Palette reference (vscodet light):
;;   bg: #ffffff  fg: #000000  accent: #A68AF9
;;   secondary: #321685  strings: #240e67  comments: #808080

;;; Code:

(require 'nano-themes)

;; Tailwind Zinc scale:
;; 50:#fafafa 100:#f4f4f5 200:#e4e4e7 300:#d4d4d8 400:#a1a1aa
;; 500:#71717a 600:#52525b 700:#3f3f46 800:#27272a 900:#18181b 950:#09090b
;;
;; Violet scale (Lithos):
;; 50:#f5f3ff 100:#ede9fe 200:#ddd6fe 300:#c4b5fd 400:#A58AF9
;; 500:#8b5cf6 600:#7c3aed 700:#6d28d9 800:#5b21b6 900:#4c1d95 950:#2e1065

(defconst nano-light-palette-partial
  '(;; Core surfaces — Zinc light end
    (cursor "#A58AF9")                ; violet-400 — accent cursor
    (bg-main "#ffffff")               ; pure white
    (bg-dim "#fafafa")                ; zinc-50
    (bg-alt "#f4f4f5")               ; zinc-100
    (fg-main "#18181b")               ; zinc-900
    (fg-dim "#a1a1aa")                ; zinc-400 — comments, secondary
    (fg-alt "#71717a")                ; zinc-500 — tertiary
    (bg-active "#e4e4e7")             ; zinc-200
    (bg-inactive "#fafafa")           ; zinc-50
    (border "#d4d4d8")                ; zinc-300

    ;; Dichromatic: violet is the ONLY chromatic hue for syntax
    ;; Cyan slots → deep violet shades
    (cyan "#5E35B1")                  ; deep violet (from vscodet light)
    (cyan-warmer "#321685")           ; vscodet secondary (types/keywords)
    (cyan-cooler "#4c1d95")           ; violet-900
    (cyan-faint "#7c3aed")            ; violet-600

    ;; Red — semantic only (errors, invalid, diff deleted)
    (red "#D32F2F")
    (red-warmer "#E53935")
    (red-cooler "#C62828")
    (red-faint "#EF9A9A")

    ;; Green — semantic only (diff inserted, success)
    (green "#2E7D32")
    (green-warmer "#388E3C")
    (green-cooler "#1B5E20")
    (green-faint "#A5D6A7")

    ;; Yellow — semantic only (warnings, modified)
    (yellow "#F57F17")
    (yellow-warmer "#FF8F00")
    (yellow-cooler "#E65100")
    (yellow-faint "#FFE082")

    ;; Blue slots → violet shades (dichromatic: no separate blue hue)
    (blue "#7c3aed")                  ; violet-600
    (blue-warmer "#5b21b6")           ; violet-800
    (blue-cooler "#4c1d95")           ; violet-900
    (blue-faint "#8b5cf6")            ; violet-500

    ;; Magenta slots → violet shades (all one hue family)
    (magenta "#321685")               ; vscodet secondary — deep violet
    (magenta-warmer "#240e67")        ; vscodet strings — deepest violet
    (magenta-cooler "#5E35B1")        ; deep violet
    (magenta-faint "#7c3aed")         ; violet-600

    ;; Line highlight and completion — vscodet accent-background
    (bg-hl-line "#c5beda")            ; vscodet light accent-background
    (bg-completion "#c5beda")         ; unified with hl-line for vertico

    ;; Mode line — thin separator line, not a bar
    (bg-mode-line-active "#f4f4f5")   ; zinc-100 — barely visible
    (fg-mode-line-active "#52525b")   ; zinc-600
    (border-mode-line-active "#d4d4d8") ; zinc-300
    (bg-mode-line-inactive "#ffffff") ; same as bg-main
    (fg-mode-line-inactive "#a1a1aa") ; zinc-400
    (border-mode-line-inactive "#e4e4e7"))) ; zinc-200

(defconst nano-light-palette-mappings-partial
  '((err red-warmer)
    (warning yellow-warmer)
    (info green)

    ;; Dichromatic syntax: fg-main for plain code, violet shades for emphasis
    (fg-link cyan-faint)              ; violet-600 links
    (fg-link-visited magenta-faint)   ; violet-600 visited
    (name cyan-warmer)                ; deep violet — identifiers
    (keybind cyan-cooler)             ; violet-900
    (identifier fg-alt)               ; zinc-500 — subdued
    (fg-prompt fg-main)

    ;; Code faces — dichromatic treatment:
    ;; bold+fg-main for structural keywords/operators
    ;; violet shades for types/keywords/decorators
    ;; fg-main for variables, fg-dim for comments
    (builtin magenta)                 ; deep violet
    (comment fg-dim)                  ; zinc-400
    (constant cyan)                   ; deep violet
    (fnname fg-main)                  ; plain black — functions are structure
    (keyword magenta)                 ; deep violet (bold via modus)
    (preprocessor magenta-faint)      ; violet-600
    (docstring fg-dim)                ; zinc-400
    (string magenta-warmer)           ; deepest violet (#240e67)
    (type cyan-warmer)                ; vscodet secondary (#321685)
    (variable fg-alt)                 ; zinc-500

    (accent-0 blue)                   ; violet-600
    (accent-1 magenta)                ; deep violet
    (accent-2 cyan)                   ; deep violet
    (accent-3 green)                  ; kept for diff/status

    (date-common cyan)
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

    (mail-cite-0 cyan)
    (mail-cite-1 magenta)
    (mail-cite-2 cyan-warmer)
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
    (fg-heading-1 fg-main)            ; bold near-black
    (fg-heading-2 magenta)            ; secondary violet (#321685)
    (fg-heading-3 cyan)               ; deep violet (#5E35B1)
    (fg-heading-4 fg-alt)             ; zinc-500 (#71717a)
    (fg-heading-5 magenta-faint)      ; violet-400 (#A58AF9)
    (fg-heading-6 fg-dim)             ; zinc-400 (#a1a1aa)

    (rainbow-0 cyan)
    (rainbow-1 magenta)
    (rainbow-2 cyan-warmer)
    (rainbow-3 magenta-warmer)
    (rainbow-4 cyan-cooler)
    (rainbow-5 magenta-faint)
    (rainbow-6 fg-alt)
    (rainbow-7 fg-dim)
    (rainbow-8 magenta-cooler)))

(defcustom nano-light-palette-overrides nil
  "Overrides for `nano-light-palette'.

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

(defconst nano-light-palette
  (modus-themes-generate-palette
   nano-light-palette-partial
   'cool
   nil
   nano-light-palette-mappings-partial)
  "Generated palette for the N Λ N O light theme.")

;; Mode-line as thin separator in GUI: height 0.1, fg=bg (invisible text),
;; underline in border color, no box.  In TTY: default mode-line with underline.
;; NOTE: bg-dim (#fafafa) is nearly invisible on bg-main (#ffffff) in light
;; themes — use border (#d4d4d8 zinc-300) for a visible separator.
(defvar nano-light-custom-faces
  '(`(mode-line
      ((((type graphic))
        :height 0.1
        :foreground ,bg-main
        :background ,bg-main
        :underline ,border
        :overline nil
        :box nil
        :inherit nil)
       (t :underline ,border-mode-line-active :box nil)))
    `(mode-line-active
      ((((type graphic))
        :height 0.1
        :foreground ,bg-main
        :background ,bg-main
        :underline ,border
        :overline nil
        :box nil
        :inherit nil)
       (t :underline ,border-mode-line-active :box nil)))
    `(mode-line-inactive
      ((((type graphic))
        :height 0.1
        :foreground ,bg-main
        :background ,bg-main
        :underline ,border
        :overline nil
        :box nil
        :inherit nil)
       (t :underline ,border-mode-line-inactive :box nil)))
    ;; Window dividers — match border color (zinc-300)
    `(window-divider ((t :foreground ,border)))
    `(window-divider-first-pixel ((t :foreground ,border)))
    `(window-divider-last-pixel ((t :foreground ,border)))
    `(vertical-border ((t :foreground ,border))))
  "Custom face overrides for the N Λ N O light theme.")

(modus-themes-theme
 'nano-light
 'nano-themes
 "N Λ N O light theme — Lithos dichromatic (Zinc + Violet)."
 'light
 'nano-light-palette
 nil
 'nano-light-palette-overrides
 'nano-light-custom-faces)

;;; nano-light-theme.el ends here
