---
title: hyalo
description: Core loader for the Hyalo dynamic module
navigation:
  icon: i-lucide-zap
order: 1
tags:
  - lisp
  - module
  - core
---

> Core loader for the Hyalo dynamic module. Handles building the Swift dylib when needed, loading the module via `module-load`, and feature detection. Window setup and channels are managed in separate files.

## Overview

`hyalo.el` is the entry point for the Lisp side of Hyalo. It locates and loads the compiled Swift dynamic module, optionally triggering an automatic build if the dylib is absent or stale. All other Hyalo modules depend on the feature provided here.

## Functions

### `hyalo-available-p`
**()** → boolean

Returns non-nil if the Hyalo dynamic module has been successfully loaded and its feature is present in `features`.

### `hyalo-load-module`
**()** → nil

Locates the compiled `.dylib` and calls `module-load` to register the Swift-side native functions. Signals an error if the module cannot be found and `hyalo-auto-build` is nil.

### `hyalo-auto-build`
**()** → nil

Invokes the Swift build system to compile the dynamic module from source. Called automatically on startup when `hyalo-auto-build` is non-nil and no prebuilt dylib is present.

### `hyalo-trace`
**(fmt &rest args)** → nil

Emits a TRACE-level log message, formatted via `format`.

### `hyalo-debug`
**(fmt &rest args)** → nil

Emits a DEBUG-level log message.

### `hyalo-info`
**(fmt &rest args)** → nil

Emits an INFO-level log message.

### `hyalo-warn`
**(fmt &rest args)** → nil

Emits a WARN-level log message.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-auto-build` | `t` | When non-nil, automatically build the Swift dylib if it is missing at startup |
| `hyalo-elog` | `nil` | Logger handle; set to a non-nil value once the `elog` package is loaded |

## See Also

- [[init/hyalo|init-hyalo.el]] — calls `(require 'hyalo)` to load this module
- [[lisp/hyalo-channels|hyalo-channels.el]] — opens channels after this module loads
- [[swift/core|Swift Core]] — the dynamic module entry point registered here
