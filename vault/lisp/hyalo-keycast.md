---
title: hyalo-keycast
description: Keycast integration that broadcasts pressed keys to the Swift toolbar KeycastView
navigation:
  icon: i-lucide-keyboard
order: 8
tags:
  - lisp
  - module
  - toolbar
---

> Keycast integration — broadcasts pressed keys and the associated command name to the Swift toolbar `KeycastView`. Uses an advice on `keycast--update` to intercept display data without reimplementing keycast's capture logic.

## Overview

`hyalo-keycast.el` piggybacks on the `keycast` package by advising its internal update function. Each time `keycast` would render a key+command pair, Hyalo intercepts the data and pushes it to Swift instead, where it is displayed in a transient toolbar overlay. No additional key-capture infrastructure is needed.

## Functions

### `hyalo-keycast-setup`
**()** → nil

Adds the advice on `keycast--update` and enables `hyalo-keycast-mode` so the toolbar channel receives key events.

### `hyalo-keycast-mode`
**(&optional arg)** → nil

Minor mode toggle that enables or disables the keycast relay to Swift. When disabled, the advice is removed and the Swift `KeycastView` is cleared.
