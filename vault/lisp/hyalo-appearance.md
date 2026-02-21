---
title: hyalo-appearance
description: System appearance detection and theme color synchronization to Swift
navigation:
  icon: i-lucide-sun-moon
order: 2
tags:
  - lisp
  - module
  - appearance
---

> Appearance management: system light/dark detection, frame transparency (`alpha-background`), background color sync to Swift (theme-derived), window divider color, and fringe color. Opacity and vibrancy are managed exclusively by Swift and persisted in UserDefaults.

## Overview

`hyalo-appearance.el` monitors macOS system appearance changes and active Emacs themes, then pushes the derived color set to Swift so that SwiftUI panels and overlays stay in sync with the editor. Frame-level transparency is controlled via `alpha-background`; opacity and vibrancy knobs live entirely on the Swift side.

## Functions

### `hyalo-appearance-sync`
**()** → nil

Reads the current theme's background, foreground, fringe, and window-divider colors, packages them as a property list, and pushes them to Swift over the `appearance` channel.

### `hyalo-appearance-setup`
**()** → nil

Installs hooks on `ns-system-appearance-change-functions`, `enable-theme-functions`, and `disable-theme-functions` so that `hyalo-appearance-sync` is called whenever the active appearance changes.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-appearance-alpha-elements` | `((ns-alpha-default . 0.0) (ns-alpha-glyphs . 0.0) (ns-alpha-fringe . 0.10))` | Alist mapping NS alpha frame parameters to their target opacity values |

## Hooks Used

| Hook | Purpose |
|------|---------|
| `ns-system-appearance-change-functions` | Re-sync colors when macOS switches between light and dark mode |
| `enable-theme-functions` | Re-sync colors when a new theme is enabled |
| `disable-theme-functions` | Re-sync colors when the active theme is disabled |

## See Also

- [[init/appearance|init-appearance.el]] — configures themes and fonts that this module syncs
- [[swift/appearance|Swift Appearance]] — receives color payloads via the appearance channel
- [[features/appearance|Appearance feature overview]]
