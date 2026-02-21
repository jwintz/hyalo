---
title: hyalo-themes
description: Theme registry exposing nano-theme and iota-theme variants to Swift
navigation:
  icon: i-lucide-swatch-book
order: 18
tags:
  - lisp
  - module
  - appearance
---

> Theme registry: nano-themes wrappers and iota-theme variants. Provides the available theme list to the Swift appearance inspector, and applies themes by name from both Lisp and Swift.

## Overview

`hyalo-themes.el` maintains a catalogue of bundled themes (nano-themes light/dark variants and iota-theme transparent variants) and exposes functions to list and apply them. The Swift appearance inspector panel reads the theme list over the `appearance` channel and can invoke `hyalo-themes--apply` via a channel command.

## Functions

### `hyalo-themes-setup`
**()** → nil

Registers all bundled themes and pushes the initial theme list to Swift over the `appearance` channel so the inspector panel is populated at startup.

### `hyalo-themes--list`
**()** → list

Returns an alist of `(id . display-name)` pairs for all registered themes, suitable for serialization to Swift.

### `hyalo-themes--apply`
**(theme-id)** → nil

Disables the currently active theme, then loads and enables the theme identified by `theme-id`. Triggers `hyalo-appearance-sync` after the switch to propagate new colors to Swift.
