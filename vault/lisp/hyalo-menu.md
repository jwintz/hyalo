---
title: hyalo-menu
description: macOS menu bar customization for Hyalo-specific Application menu items
navigation:
  icon: i-lucide-menu
order: 10
tags:
  - lisp
  - module
  - macos
---

> macOS menu bar customization. Sets up Hyalo-specific Application menu items that complement the standard Emacs menu, providing IDE-oriented actions accessible from the system menu bar.

## Overview

`hyalo-menu.el` modifies the macOS Application menu to expose Hyalo-level commands — such as triggering module reloads, opening settings, and invoking Swift-side panels — alongside the standard Emacs menu entries.

## Functions

### `hyalo-menu-setup`
**()** → nil

Configures the macOS Application menu by adding Hyalo-specific `NSMenuItem` entries via the dynamic module's menu API. Should be called after the Hyalo module has been loaded and the frame is live.

## See Also

- [[swift/toolbar|Swift Toolbar]] — toolbar items that complement these menu entries
