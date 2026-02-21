---
title: Features
description: An overview of Hyalo's core features
navigation:
  icon: i-lucide-star
  order: 0
order: 0
tags:
  - features
  - overview
  - hyalo
---

Hyalo is a macOS IDE shell around Emacs, delivered as a Swift dynamic module (`.dylib`) loaded directly into the Emacs process. It adds a native macOS UI layer without replacing Emacs — every panel, toolbar item, and inspector view communicates with Emacs Lisp via async channels.

## Feature Areas

- [[features/appearance|Appearance]] — Liquid Glass, themes, transparency, SF Mono
- [[features/native-compilation|Native Compilation AOT]] — Ahead-of-time compilation for fast startup
- [[features/init-system|Init System]] — 14 focused modules loaded in sequence
- [[features/lisp-side|Lisp Side]] — Hook-driven data push, channels, and module architecture
- [[features/swift-side|Swift Side]] — NavigationSplitView, panels, toolbar, command palette
- [[features/architecture|Architecture]] — How Swift and Emacs communicate
