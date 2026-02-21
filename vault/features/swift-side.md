---
title: Swift Side
description: NavigationSplitView, Liquid Glass panels, NSToolbar, and the dynamic module architecture
navigation:
  icon: i-lucide-layers-3
order: 5
tags:
  - features
  - swift
  - swiftui
  - macos
  - dynamic-module
  - architecture
---

The Swift side is a macOS dynamic module (`.dylib`) loaded directly into the Emacs process at startup. It adds a native macOS 26 UI layer without replacing Emacs.

## Dynamic Module

The module is compiled with `swift build` and loaded by `hyalo.el` via `module-load`. Because it runs inside the Emacs process, Swift code can:
- Wrap the original Emacs `NSView` in a SwiftUI hosting view
- Call Emacs C functions via `dlsym` (`ns_force_redisplay`, `ns_wake_emacs`)
- Open pipe-based async channels that Emacs reads as process I/O

## Layout

```
NavigationSplitView
├── Sidebar  →  NavigatorAreaView
│               ├── Buffers (BufferListView)
│               ├── Files (ProjectNavigatorView)
│               ├── Source Control (SourceControlNavigatorView)
│               └── Find (FindNavigatorView)
├── Content  →  MainContentView
│               ├── EditorTabBarView
│               └── EmacsView (original NSView)
└── Detail   →  InspectorAreaView
                ├── File (FileInspectorView)
                ├── History (HistoryInspectorView)
                ├── Appearance (InspectorAppearanceView)
                └── Terminal (InspectorTerminalView)
```

## Key Design Decisions

- **Emacs is single source of truth** — Swift views never mutate local state on user interaction; they send a message to Emacs and wait for the echo
- **`wakeEmacs()`** is called on every user action so Emacs reads the pipe immediately
- **`@Observable` view models** — no `ObservableObject`, no `@Published`; uses macOS 17+ `@Observable`
- **`@Bindable` injection** — view models injected via `@Bindable`, not `@EnvironmentObject`

See [[swift/index|Swift Modules]] for full documentation of each module.
