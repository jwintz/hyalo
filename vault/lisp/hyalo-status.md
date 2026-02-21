---
title: hyalo-status
description: Status bar hook-driven updates for cursor position, mode, encoding, and project root
navigation:
  icon: i-lucide-activity
order: 16
tags:
  - lisp
  - module
  - statusbar
---

> Status bar updates via hooks. Pushes cursor position, major mode, file encoding, and project root to the Swift status bar. Buffer-switch hooks perform zero subprocess work; git calls are deferred behind debounce timers.

## Overview

`hyalo-status.el` is the central synchronization hub for the Swift status bar. It installs hooks on window and selection changes, debounces expensive operations (git branch lookup, navigator refresh), and exposes `hyalo-sync--push` as the single entry point that coordinates all downstream push modules.

## Functions

### `hyalo-status-setup`
**()** → nil

Adds `hyalo-status--on-buffer-change` to `window-buffer-change-functions` and `window-selection-change-functions`, and adds `hyalo-sync--push` to `post-command-hook` for cursor-position updates.

### `hyalo-sync--push`
**()** → nil

Main synchronization entry point. Immediately pushes cursor position, mode name, and encoding; schedules `hyalo-status--branch-timer` and `hyalo-status--navigator-timer` for deferred git and navigator updates.

### `hyalo-status--on-buffer-change`
**(&rest _)** → nil

Hook callback for `window-buffer-change-functions`. Calls `hyalo-sync--push` and cancels any stale debounce timers before scheduling fresh ones.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-status--cursor-timer` | `nil` | Debounce timer for cursor-position pushes on `post-command-hook` |
| `hyalo-status--branch-timer` | `nil` | Debounce timer that defers the git-branch subprocess call after a buffer switch |
| `hyalo-status--navigator-timer` | `nil` | Debounce timer that defers the navigator buffer-list push after a buffer switch |

## Hooks Used

| Hook | Purpose |
|------|---------|
| `window-buffer-change-functions` | Trigger full sync when the buffer shown in any window changes |
| `window-selection-change-functions` | Trigger full sync when window focus moves |
| `post-command-hook` | Push cursor position after every command |

## See Also

- [[swift/statusbar|Swift StatusBar]] — renders the values pushed by this module
- [[lisp/hyalo-navigator|hyalo-navigator.el]] — called from `hyalo-sync--push` for buffer list updates
