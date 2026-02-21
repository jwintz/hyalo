---
title: Init System
description: Hyalo's modular Emacs init system — 14 focused modules loaded in sequence
navigation:
  icon: i-lucide-layers
  order: 0
order: 0
tags:
  - init
  - emacs-lisp
  - configuration
  - use-package
  - overview
---

Hyalo's init system is split into 14 focused modules, each responsible for a single concern. Modules are loaded in sequence by `init.el` via `init-bootstrap`, which sets up package management and the execution environment before other modules run.

## Load Order

1. `init-bootstrap` — Package archives, GC tuning, exec-path-from-shell
2. `init-core` — diminish, general, which-key
3. `init-emacs` — Built-in Emacs settings (startup, cursor, recentf, saveplace)
4. `init-appearance` — Fonts, themes (modus, nano, ef, iota-dimmer)
5. `init-editing` — Editing packages (god-mode, windmove, outline)
6. `init-completion` — Vertico, Consult, Marginalia, Orderless
7. `init-modes` — Language major modes (json, swift, yaml, …)
8. `init-tools` — Dev tools (project, magit, eglot, flymake)
9. `init-help` — Help system (helpful, elisp-refs)
10. `init-header` — File header management (header2)
11. `init-markdown` — Markdown and knowledge management
12. `init-hyalo` — macOS integration, module load, channels, keybindings
13. `init-agents` — AI agents (copilot)
14. `init-tengwar` — Tengwar script rendering (optional)

## Module Reference

![[init-modules.base]]

## See Also

- [[features/init-system|Init System feature overview]]
- [[lisp/hyalo|hyalo.el]] — Lisp loader loaded by `init-hyalo`
- [[lisp/hyalo-channels|hyalo-channels.el]] — channels opened on startup
- [[lisp/index|Lisp Side]] — companion Lisp module reference
