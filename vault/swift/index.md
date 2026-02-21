---
title: Swift Side
description: Hyalo's native macOS Swift layer — NavigationSplitView, Liquid Glass, channels
navigation:
  icon: i-lucide-layers-3
  order: 0
order: 0
tags:
  - swift
  - overview
---

The Swift side of Hyalo is a macOS 26 dynamic module (`.dylib`) loaded directly into the Emacs process. It provides:

- A native `NavigationSplitView` layout with Navigator, Editor, Inspector, and Utility panels
- Liquid Glass design — macOS 26 Tahoe native materials
- Bidirectional async channels connecting Swift views to Emacs Lisp
- SF Symbols and SF Fonts throughout

The module is built with `swift build` and loaded by `hyalo.el` via `module-load`.

## Architecture

```
Emacs Process
└── Hyalo.dylib (Swift dynamic module)
    ├── Core/          — Module entry, workspace state, environment
    ├── Window/        — NSWindowController + NavigationSplitView host
    ├── Editor/        — Emacs view embedding + tab bar
    ├── Navigator/     — Left sidebar (buffers, files, SCM, search)
    ├── Inspector/     — Right sidebar (file info, history, appearance)
    ├── StatusBar/     — Bottom status bar
    ├── UtilityArea/   — Bottom panel (terminal, diagnostics)
    ├── Toolbar/       — NSToolbar (environment pill, branch picker, packages)
    ├── CommandPalette/ — Cmd+P palette and Cmd+O open-quickly
    ├── Appearance/    — Liquid Glass effects, vibrancy, color themes
    ├── Settings/      — App settings
    └── Shared/        — Reusable panel/section components
```

## Module Reference

![[swift-modules.base]]

## See Also

- [[features/swift-side|Swift Side feature overview]]
- [[lisp/hyalo|hyalo.el]] — loads this module into Emacs
- [[lisp/hyalo-channels|hyalo-channels.el]] — opens channels Swift listens on
- [[lisp/index|Lisp Side]] — companion Lisp module reference
