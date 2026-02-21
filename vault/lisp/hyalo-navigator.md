---
title: hyalo-navigator
description: Buffer list and project root relay to the Swift navigator panel
navigation:
  icon: i-lucide-panel-left
order: 12
tags:
  - lisp
  - module
  - navigator
---

> Pushes buffer list and project root to the Swift navigator. The file tree is built entirely in Swift via `ProjectNavigator` — Emacs only provides the root path. Buffer list updates are driven by hook, not polling.

## Overview

`hyalo-navigator.el` keeps the Swift navigator panel up to date by pushing the current buffer list (excluding internal buffers) and the active project root whenever a window's buffer changes. File-system traversal and tree rendering are owned entirely by the Swift `ProjectNavigatorView`.

## Functions

### `hyalo-navigator-refresh`
**()** → nil

Entry point called by `hyalo-sync--push` (in `hyalo-status.el`) to refresh both the buffer list and the project root in a single update cycle.

### `hyalo-navigator--update-buffers`
**()** → nil

Collects the list of live buffers whose names do not start with a space, serializes their names and associated file paths, and pushes the result to Swift over the `navigator` channel.

### `hyalo-navigator--push-project-root`
**()** → nil

Resolves the project root for the current buffer using `project.el` or `projectile`, then pushes the absolute path to Swift so `ProjectNavigator` can (re-)scan the directory tree.

## Hooks Used

| Hook | Purpose |
|------|---------|
| `window-buffer-change-functions` | Triggers refresh via `hyalo-sync--push` in `hyalo-status.el` whenever the buffer displayed in any window changes |

## See Also

- [[swift/navigator|Swift Navigator]] — receives buffer list and project root pushes
- [[lisp/hyalo-status|hyalo-status.el]] — calls `hyalo-navigator-refresh` from `hyalo-sync--push`
