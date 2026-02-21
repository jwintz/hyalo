---
title: hyalo-lib
description: Utility library providing transient hooks, first-use hooks, and incremental idle loading
navigation:
  icon: i-lucide-library
order: 9
tags:
  - lisp
  - module
  - core
---

> Utility library: transient hooks, first-use hooks, incremental idle loading. Provides infrastructure used across all other Hyalo modules to defer work, avoid redundant listeners, and spread startup cost across idle time.

## Overview

`hyalo-lib.el` supplies the shared primitives that the rest of the Hyalo Lisp layer is built on. The transient hook macro enables self-removing one-shot callbacks; the incremental loader spreads feature `require` calls across idle timers so that startup latency is minimized.

## Macros

### `hyalo-add-transient-hook`
**(hook-or-function &rest forms)** → nil

Attaches `forms` as a lambda to `hook-or-function`. The lambda executes `forms` once and then immediately removes itself from the hook, ensuring the callback fires exactly one time regardless of how many times the hook is subsequently run.

## Functions

### `hyalo-load-incrementally`
**(features)** → nil

Schedules each symbol in `features` to be `require`d on successive idle timers. This spreads the I/O and compilation cost of loading optional packages across Emacs's idle periods rather than blocking startup.
