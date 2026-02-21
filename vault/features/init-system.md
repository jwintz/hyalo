---
title: Init System
description: 14 focused init modules loaded in sequence via use-package
navigation:
  icon: i-lucide-layers
order: 3
tags:
  - features
  - init
  - emacs-lisp
  - configuration
  - use-package
---

Hyalo's init system is split into **14 focused modules**, each responsible for a single concern. The entry point `init.el` loads them in sequence after `init-bootstrap` sets up package management.

## Design Principles

- Each module is a single `.el` file in `init/`
- All package configuration uses `use-package` with `:ensure t`
- Keybindings use `general` with a `C-c` leader prefix
- No module depends on another module (only on packages)
- `init-hyalo` is always last among non-optional modules — it loads the Swift dynamic module after all Emacs configuration is in place

## Load Order

```
init.el
├── init-bootstrap   — Package archives, GC, exec-path-from-shell
├── init-core        — diminish, general, which-key
├── init-emacs       — Built-in Emacs settings
├── init-appearance  — Fonts, themes, iota-dimmer
├── init-editing     — god-mode, windmove, avy, expand-region
├── init-completion  — Vertico, Consult, Corfu, Cape
├── init-modes       — Language major modes
├── init-tools       — project, magit, eglot, flymake
├── init-help        — helpful, elisp-refs
├── init-header      — header2 file headers
├── init-markdown    — markdown-mode, denote
├── init-hyalo       — Swift module load, channels, keybindings
├── init-agents      — copilot (AI completions)
└── init-tengwar     — Tengwar rendering (optional)
```

See [[init/index|Init Modules]] for full documentation of each module.
