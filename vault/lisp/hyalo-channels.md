---
title: hyalo-channels
description: Bidirectional async channel management between Swift and Emacs Lisp
navigation:
  icon: i-lucide-cable
order: 3
tags:
  - lisp
  - module
  - channels
---

> Manages bidirectional async channels between Swift and Emacs Lisp. Each named channel maps to a SwiftUI view or service on the Swift side and a push handler on the Lisp side.

## Overview

`hyalo-channels.el` is responsible for opening every named async channel that Hyalo uses for communication. Channels are opened once at startup via `hyalo-channels-setup`; individual modules then write to their respective channels without needing to manage lifecycle themselves. The `hyalo-source-control` dependency is loaded optionally.

## Functions

### `hyalo-channels-setup`
**()** → nil

Opens all application channels by calling the module-level open function for each named channel: `navigator`, `editor-tab`, `status`, `toolbar`, `command-palette`, `search`, `appearance`, `diagnostics`, `package`, and `source-control`. Requires `hyalo-source-control` when available.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-channels--initialized` | `nil` | Non-nil after `hyalo-channels-setup` has completed successfully; guards against double-initialization |

## See Also

- [[lisp/hyalo|hyalo.el]] — loads the dynamic module before channels can be opened
- [[init/hyalo|init-hyalo.el]] — calls `hyalo-channels-setup` on `window-setup-hook`
- [[swift/core|Swift Core]] — Swift side that listens on these channels
