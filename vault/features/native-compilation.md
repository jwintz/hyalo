---
title: Native Compilation AOT
description: Ahead-of-time native compilation of Emacs Lisp for fast subsequent startups
navigation:
  icon: i-lucide-zap
order: 2
tags:
  - features
  - native-compilation
  - performance
  - emacs-lisp
  - aot
---

Hyalo enables **ahead-of-time (AOT) native compilation** of Emacs Lisp. On first launch, Emacs compiles all packages and init files to native code asynchronously. On subsequent launches, the native `.eln` cache is used directly — resulting in near-instant startup.

## How it works

Emacs 30.1+ ships with `libgccjit` support. When `native-comp-jit-compilation` is enabled, Emacs compiles `.elc` bytecode to native machine code via GCC's JIT backend.

AOT compilation works differently: packages are compiled at install time (or on first load) and cached in `~/.emacs.d/eln-cache/`. Subsequent loads skip all compilation overhead.

## Activity Viewer

While native compilation is in progress, Hyalo shows a **circular progress indicator** in the toolbar activity pill via `hyalo-compile.el`:

- Detects pending files via `comp-files-queue` and active jobs via `comp-async-compilations`
- Updates on `native-comp-async-cu-done-functions` (per file) and `native-comp-async-all-done-hook` (batch complete)
- Auto-dismisses 8 seconds after the queue drains

```elisp
;; Activity shown while compiling:
;; "Native compiling N files..."
```

## Configuration

Native compilation is enabled in `init-bootstrap.el`. No special configuration is needed — if the Emacs binary supports it, it is active.

To check:

```elisp
(native-comp-available-p) ; => t if enabled
```
