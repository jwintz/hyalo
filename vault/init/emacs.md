---
title: init-emacs
description: Built-in Emacs settings — startup, cursor, persistence, and macOS tweaks
navigation:
  icon: i-lucide-settings
order: 3
tags:
  - init
  - emacs-lisp
  - configuration
  - emacs
  - macos
  - builtin
---

`init-emacs` configures built-in Emacs behaviour with no external packages. It covers startup suppression, cursor style, persistence modes, fringe width, and macOS-specific knobs.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `emacs` | built-in | Umbrella `use-package` form for built-in settings |

## Configuration Highlights

### Startup

```elisp
(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-screen t)
  (initial-buffer-choice t))   ; open *scratch* on launch
```

### Cursor

```elisp
  (cursor-type '(hbar . 2))
  (cursor-in-non-selected-windows nil)
```

### Persistence

```elisp
  :config
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
```

### Fringe & macOS

```elisp
  (fringe-mode 8)
  (ns-use-proxy-icon nil)
  (ns-use-mwheel-momentum t)
```
