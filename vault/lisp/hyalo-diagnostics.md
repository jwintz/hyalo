---
title: hyalo-diagnostics
description: Eglot/Flymake/Flycheck diagnostic relay to the Swift Utility Area
navigation:
  icon: i-lucide-triangle-alert
order: 5
tags:
  - lisp
  - module
  - diagnostics
---

> Pushes eglot, Flymake, and Flycheck diagnostics to the Swift UtilityArea panel. Diagnostic data is serialized as a JSON array and sent over the `diagnostics` channel whenever the buffer's error set changes.

## Overview

`hyalo-diagnostics.el` collects diagnostic entries from whichever checking backend is active for the current buffer, normalizes them into a uniform schema, and pushes the resulting array to Swift. The Swift side renders the list in the Utility Area with severity icons, source labels, and navigable line references.

## Functions

### `hyalo-diagnostics-setup`
**()** → nil

Adds `hyalo-diagnostics--on-buffer-change` to `flymake-diagnostic-functions` and installs a hook on `eglot-managed-mode-hook` so that diagnostic push is wired up for every LSP-managed buffer.

### `hyalo-diagnostics--push`
**()** → nil

Collects current diagnostics from the active backend, serializes them to a JSON array of objects conforming to the data format below, and writes the payload to the `diagnostics` channel.

### `hyalo-diagnostics--on-buffer-change`
**(&rest _)** → nil

Callback invoked by Flymake or Eglot notification hooks. Schedules a debounced call to `hyalo-diagnostics--push` to avoid redundant pushes during rapid edits.

## Hooks Used

| Hook | Purpose |
|------|---------|
| `flymake-diagnostic-functions` | Integrate with Flymake's diagnostic reporting pipeline |
| `eglot-managed-mode-hook` | Wire up LSP diagnostic notifications when Eglot activates for a buffer |

## Data Format

Each entry in the JSON array has the following shape:

```json
{
  "file":     "/absolute/path/to/file.swift",
  "line":     42,
  "col":      7,
  "severity": "error",
  "message":  "Use of undeclared identifier 'foo'",
  "source":   "eglot"
}
```

## See Also

- [[swift/utilityarea|Swift UtilityArea]] — renders the diagnostics list in the Diagnostics tab
