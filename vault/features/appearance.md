---
title: Appearance Overview
description: Liquid Glass design, SF fonts, theme stack, and per-element transparency
navigation:
  icon: i-lucide-sparkles
order: 1
tags:
  - features
  - appearance
  - liquid-glass
  - macos
  - themes
  - swiftui
---

Hyalo targets **macOS 26 Tahoe** and adopts the Liquid Glass design system throughout. Every panel and chrome element uses native materials; the Emacs content area uses a configurable color tint overlay.

## Liquid Glass

Panels (Navigator, Inspector, UtilityArea) and toolbar elements use macOS 26 `.glassEffect()`. The Inspector Appearance tab lets you choose the vibrancy material and opacity independently for the Emacs content area.

Key rules from `AGENTS.md`:
- `workspace.backgroundColor` tint overlay applies to editor content area, navigator, and inspector panels for a uniform look
- Status bar, tab bar, and utility area use system-adaptive colors
- Split view layer backgrounds use `NSColor.windowBackgroundColor.cgColor`
- Do not force window appearance — let the system handle dark/light mode

## Fonts

| Face | Family | Size |
|------|--------|------|
| Default (monospace) | SF Mono | 11pt |
| Variable pitch | Recursive Mono Casual Static | 11pt |

## Theme Stack

Hyalo ships a curated theme stack. Themes are applied via `init-appearance.el`:

| Theme | Package | Notes |
|-------|---------|-------|
| `modus-operandi` / `modus-vivendi` | `modus-themes` | High-contrast, accessibility-first |
| `nano-light` / `nano-dark` | `nano-themes` (vendored) | Clean minimal aesthetic |
| `ef-*` | `ef-themes` | Colourful accessible themes |

## iota-dimmer

`iota-dimmer-mode` dims inactive windows by overlaying a semi-transparent face, making the active buffer visually prominent without obscuring content.

## Per-element Transparency

`hyalo-appearance-alpha-elements` controls per-layer alpha:

```elisp
(defcustom hyalo-appearance-alpha-elements
  '((ns-alpha-default . 0.0)
    (ns-alpha-glyphs  . 0.0)
    (ns-alpha-fringe  . 0.10))
  ...)
```

The fringe alpha is overridden at runtime by the Inspector opacity slider.

## See Also

- [[init/appearance|init-appearance.el]] — Emacs-side theme and font configuration
- [[lisp/hyalo-appearance|hyalo-appearance.el]] — syncs active theme colors to Swift
- [[swift/appearance|Swift Appearance]] — Liquid Glass materials and `HyaloColorTheme`
