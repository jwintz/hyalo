---
title: iota-dimmer
description: Inactive buffer dimming via semi-transparent face overlay
navigation:
  icon: i-lucide-contrast
order: 20
tags:
  - lisp
  - module
  - appearance
---

> Inactive buffer dimming. Dims non-selected windows by overlaying a semi-transparent face, making the active window visually prominent. Integrates with the iota theme family's layered transparency model.

## Overview

`iota-dimmer.el` applies a configurable dimming overlay to every window that is not currently selected. When the active window changes, overlays are redistributed so the focused editor is always at full brightness. The dimming fraction is adjustable to complement the overall transparency level set by Swift.

## Functions

### `iota-dimmer-mode`
**(&optional arg)** → nil

Global minor mode that enables or disables inactive-window dimming. When enabled, installs hooks on `window-selection-change-functions` to update overlays after every focus change.

### `iota-dimmer--dim-buffers`
**()** → nil

Iterates over all live windows and applies the dimming face overlay to every window except the selected one. Existing overlays are removed and reapplied to handle split-window configurations correctly.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `iota-dimmer-fraction` | — | Float in the range 0.0–1.0 controlling the opacity of the dimming overlay; higher values produce a more pronounced dim effect |
