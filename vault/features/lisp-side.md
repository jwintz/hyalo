---
title: Lisp Side
description: Hook-driven data push, bidirectional channels, and the Emacs Lisp module architecture
navigation:
  icon: i-lucide-parentheses
order: 4
tags:
  - features
  - emacs-lisp
  - channels
  - hooks
  - architecture
---

The Lisp side of Hyalo is the Emacs Lisp layer in `lisp/`. It bridges Emacs internals to the Swift dynamic module via async channels.

## Core Principle: No Polling

All data synchronization uses **hooks and advices**. There are no polling timers for pushing state to Swift. Every push happens in response to an Emacs event:

| Emacs Event | Hook | What gets pushed |
|-------------|------|-----------------|
| Buffer switch | `window-buffer-change-functions` | Tabs, buffer list, status bar |
| Cursor move | `post-command-hook` (debounced) | Cursor position, mode |
| Git subprocess | Debounce timer | Branch name, file status |
| Theme change | `enable-theme-functions` | Background color, face colors |
| Native compile | `native-comp-async-cu-done-functions` | Compilation progress |

## Channel Architecture

`hyalo-channels.el` opens all async channels at startup:

```
navigator-channel    ← file tree, buffer list, project root
editor-tab-channel   ← tab list, selected tab
status-channel       ← cursor, mode, encoding, branch
toolbar-channel      ← activity items, branch picker
command-palette-channel ← command invocation
search-channel       ← project search
appearance-channel   ← theme colors, vibrancy
diagnostics-channel  ← eglot/flymake errors
package-channel      ← upgradable packages
source-control-channel ← git status, staged files
```

## Module Overview

| Module | Role |
|--------|------|
| `hyalo.el` | Core loader, feature detection, logging |
| `hyalo-channels.el` | Open all async channels |
| `hyalo-window.el` | Window setup, loading proxy, post-setup orchestration |
| `hyalo-status.el` | Status bar + sync entry point (`hyalo-sync--push`) |
| `hyalo-navigator.el` | Buffer list + project root push |
| `hyalo-appearance.el` | Theme color sync |
| `hyalo-diagnostics.el` | eglot/flymake/flycheck push |
| `hyalo-compile.el` | Native compilation activity |
| `hyalo-package.el` | Package manager toolbar |
| `hyalo-source-control.el` | Git branch/status |
| `hyalo-lib.el` | Transient hooks, incremental loading |

See [[lisp/index|Lisp Modules]] for full documentation of each file.
