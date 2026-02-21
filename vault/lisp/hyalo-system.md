---
title: hyalo-system
description: macOS system integration for notifications, pasteboard, and Services menu
navigation:
  icon: i-lucide-monitor-cog
order: 17
tags:
  - lisp
  - module
  - macos
---

> macOS system integration: notifications, pasteboard, and Services menu. Bridges Emacs operations to native macOS system services via the dynamic module.

## Overview

`hyalo-system.el` exposes Emacs-level APIs over macOS system facilities provided by the Swift dynamic module. Notifications are delivered through `NSUserNotificationCenter`, pasteboard access goes through `NSPasteboard`, and Services menu registration allows other macOS apps to push content into Emacs.

## Functions

### `hyalo-system-setup`
**()** → nil

Registers Hyalo as a Services provider and configures notification and pasteboard callbacks through the dynamic module.

### `hyalo-system--notify`
**(title body &optional subtitle)** → nil

Posts a macOS user notification with the given `title`, `body`, and optional `subtitle` via the Swift notification bridge.
