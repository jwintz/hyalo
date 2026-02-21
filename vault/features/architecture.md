---
title: Architecture
description: How Swift and Emacs Lisp communicate — the dynamic module and channel architecture
navigation:
  icon: i-lucide-git-branch
order: 6
tags:
  - features
  - architecture
  - channels
  - dynamic-module
  - swift
  - emacs-lisp
---

Hyalo's architecture is built around a single principle: **Swift runs inside Emacs**. The `.dylib` is loaded into the Emacs process, not launched as a separate process.

## Process Model

```
macOS Process: Emacs
├── Emacs C core  (NS/AppKit event loop)
├── Emacs Lisp    (init system, hyalo-*.el)
└── Hyalo.dylib   (Swift dynamic module)
    └── SwiftUI   (views, channels, toolbar)
```

There is no XPC, no IPC, no socket. Swift and Emacs share memory in the same process.

## Channel Architecture

Channels are **unidirectional pipes** opened by Emacs Lisp and written by Swift (or vice versa):

```
Swift → Emacs:  Swift writes JSON to pipe fd → Emacs reads via process filter
Emacs → Swift:  Emacs calls Swift-exported C functions via module-load ABI
```

`wakeEmacs()` posts an `NSApplicationDefined` event to force `[NSApp run]` to return, so Emacs reads the pipe immediately after Swift writes.

## Data Flow Example: Tab Switch

```
User clicks tab in EditorTabBarView
  → EditorTabBarView calls selectTab(id)
  → wakeEmacs() posted
  → Swift writes JSON to editor-tab pipe: {"action":"select","id":"..."}
  → Emacs process filter fires in hyalo-status.el
  → Emacs switches buffer via tab-line-switch-to-buffer
  → window-buffer-change-functions hook fires
  → hyalo-sync--push reads new state
  → Swift setSelectedTab(id) called via module ABI
  → EditorTabViewModel updates selectedTabID
```

## Module ABI

Swift functions exposed to Emacs are registered in `Module.swift` via `EmacsSwiftModule`:

```swift
module.defun("hyalo-navigator-update-buffers") { ... }
module.defun("hyalo-set-project-root") { (root: String) in ... }
```

Emacs calls them as regular Lisp functions:

```elisp
(hyalo-navigator-update-buffers)
(hyalo-set-project-root "/path/to/project")
```

## See Also

- [[swift/core|Swift Core]] — `HyaloModule` entry point where functions are registered
- [[lisp/hyalo-channels|hyalo-channels.el]] — channel management on the Lisp side
- [[features/lisp-side|Lisp Side]] — full overview of hook-driven data push
- [[features/swift-side|Swift Side]] — full overview of the dynamic module layout
