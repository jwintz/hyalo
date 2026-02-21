---
title: hyalo-splash
description: SVG splash screen displayed while Emacs initializes
navigation:
  icon: i-lucide-monitor-play
order: 15
tags:
  - lisp
  - module
  - startup
---

> SVG splash screen shown while Emacs initializes. Provides `hyalo-loading-done` to dismiss the loading proxy window that Swift shows before the Emacs frame is ready.

## Overview

`hyalo-splash.el` renders a branded SVG splash screen in a temporary buffer that is displayed inside the Swift proxy loading window during Emacs startup. Once initialization is complete, `hyalo-loading-done` signals Swift to dissolve the proxy and reveal the live Emacs frame.

## Functions

### `hyalo-splash-show`
**()** → nil

Creates a buffer containing the Hyalo SVG splash illustration and displays it in the current frame. Called early in the startup sequence, before packages and user configuration are fully loaded.

### `hyalo-loading-done`
**()** → nil

Signals the Swift side that Emacs initialization is complete. Swift dismisses the loading proxy window and transitions to the live editor layout. This function is also re-exported by `hyalo-window.el` for use from `window-setup-hook`.
