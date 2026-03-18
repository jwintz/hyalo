---
title: hyalo-ios
description: Platform-specific Lisp initialization and bootstrapping for iOS/iPadOS
navigation:
  icon: i-lucide-smartphone
order: 15
tags:
  - lisp
  - module
  - ios
  - bootstrap
---

> Handles the platform-specific initialization and bootstrapping sequence for Hyalo on iOS and iPadOS.

## Overview

`hyalo-ios.el` is responsible for setting up the Emacs environment within the native iOS application bundle. It configures paths, loads the correct channel transport, and manages the initial startup hooks to ensure the Lisp environment is ready for the SwiftUI host to take control.

## Functions

### `hyalo-ios-bootstrap`
**()** → nil

The entry point for the iOS-specific bootstrap sequence. It is called early in the `init.el` cycle when `(eq system-type 'ios)` is true.

**Responsibilities:**
- Configures `load-path` to include bundle-relative directories.
- Sets up temporary directory and cache paths within the app's sandboxed environment.
- Loads `hyalo-channels-ios` for the direct C-interop transport.

### `hyalo-ios-setup`
**()** → nil

Called after the bootstrap sequence to initialize the Hyalo UI on iOS. It mirrors the standard `hyalo-setup` but targets the `UIKit` event loop and the embedded Emacs rendering surface.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-ios--is-bootstrapped` | `nil` | Non-nil if the iOS-specific bootstrap has completed. |
| `hyalo-ios--bundle-root` | `nil` | Stores the absolute path to the application bundle root. |

## See Also

- [[lisp/hyalo-channels|hyalo-channels.el]] — unified channel management
- [[features/architecture|Architecture Overview]] — iOS-specific process and event loop details
- [[init/hyalo|init-hyalo.el]] — the primary init module that delegates to `hyalo-ios`
