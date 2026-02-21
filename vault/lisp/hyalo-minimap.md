---
title: hyalo-minimap
description: Buffer text snapshot relay to the Swift minimap overlay
navigation:
  icon: i-lucide-map
order: 11
tags:
  - lisp
  - module
  - editor
---

> Minimap integration — pushes buffer text snapshots to the Swift minimap overlay. The minimap is rendered entirely in SwiftUI; Emacs provides the raw text and viewport position on change.

## Overview

`hyalo-minimap.el` captures a lightweight representation of the current buffer's content and the visible region, then forwards this data to Swift. Swift renders the minimap as a scaled-down overlay beside the editor frame, keeping rendering concerns separate from Emacs's redisplay engine.

## Functions

### `hyalo-minimap-setup`
**()** → nil

Installs buffer-change and scroll hooks so that `hyalo-minimap--push` is called whenever the displayed content or viewport changes.

### `hyalo-minimap--push`
**()** → nil

Serializes the current buffer's text, the visible line range, and the total line count into a payload and sends it to Swift over the minimap channel.
