---
title: hyalo-compile
description: Native compilation progress tracking with Swift activity viewer integration
navigation:
  icon: i-lucide-cpu
order: 4
tags:
  - lisp
  - module
  - compilation
---

> Tracks async native compilation activity and pushes status to the Swift activity viewer. Shows a circular progress indicator and descriptive label while compilation is active, then auto-removes the activity entry after a brief delay.

## Overview

`hyalo-compile.el` monitors Emacs's native asynchronous compiler by inspecting `comp-files-queue` (pending units) and `comp-async-compilations` (active processes). It derives a progress fraction and pushes it to the Swift activity viewer channel; on completion the activity is removed after an 8-second grace period.

## Functions

### `hyalo-compile-setup`
**()** → nil

Installs advice and hooks required to observe native compilation lifecycle events and drive `hyalo-compile--update-activity`.

### `hyalo-compile--update-activity`
**()** → nil

Computes current pending and active compilation counts, calculates progress, and pushes a status payload — including a circular progress value and a localized label — to the Swift activity channel. Schedules auto-removal when the queue is empty.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-compile--active` | `nil` | Non-nil while native compilation is in progress |
| `hyalo-compile--total` | `0` | Total number of compilation units started in the current session |
| `hyalo-compile--done` | `0` | Number of compilation units that have completed |
| `hyalo-compile--activity-id` | `"native-compilation"` | Stable identifier used to update or remove the Swift activity entry |

## Hooks Used

| Hook | Purpose |
|------|---------|
| `native-comp-async-cu-done-functions` | Increment `hyalo-compile--done` and refresh activity status after each unit finishes |
| `native-comp-async-all-done-hook` | Trigger the post-completion grace period and schedule activity removal |

## See Also

- [[swift/toolbar|Swift Toolbar]] — `ActivityManager` / `BuildStatusView` display the progress pill driven by this module
- [[features/native-compilation|Native Compilation feature overview]]
