---
title: hyalo-window
description: NSSplitViewController-based window layout and SwiftUI hosting view setup
navigation:
  icon: i-lucide-layout-dashboard
order: 19
tags:
  - lisp
  - module
  - window
---

> Interface to the `NSSplitViewController`-based window layout. Called from `init-hyalo.el` via `window-setup-hook`. Manages the two-phase setup: early decoration before init and full SwiftUI hosting view installation after init.

## Overview

`hyalo-window.el` orchestrates window initialization in two phases. The early phase decorates the Emacs frame and shows the loading proxy window before user configuration runs. The late phase, triggered by `window-setup-hook`, installs the SwiftUI hosting view into the split layout and opens all channels via `hyalo-channels-setup`.

## Functions

### `hyalo-window--early-setup`
**()** → nil

Decorates the nascent Emacs frame (title bar style, toolbar visibility) and instructs Swift to show the loading proxy window. Called as early as possible during startup, before `window-setup-hook`.

### `hyalo-window-setup`
**()** → nil

Installs the SwiftUI hosting view into the `NSSplitViewController` pane, calls `hyalo-channels-setup` to open all communication channels, and marks `hyalo-window--early-setup-done`. Retries up to `hyalo-window--post-setup-max-retries` times if the frame is not yet fully initialized.

### `hyalo-loading-done`
**()** → nil

Signals Swift to dismiss the loading proxy and reveal the live IDE layout. Re-exported here for convenience; the canonical implementation lives in `hyalo-splash.el`.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-window--post-setup-max-retries` | `20` | Maximum number of retry attempts when `hyalo-window-setup` finds the frame not yet ready |
| `hyalo-window--early-setup-done` | `nil` | Non-nil after `hyalo-window--early-setup` has completed successfully |

## Hooks Used

| Hook | Purpose |
|------|---------|
| `window-setup-hook` | Triggers `hyalo-window-setup` after Emacs finishes building the initial window layout |

## See Also

- [[swift/window|Swift Window]] — `HyaloWindowController` installed by this module's setup
- [[lisp/hyalo-channels|hyalo-channels.el]] — opened from `hyalo-window-setup`
- [[init/hyalo|init-hyalo.el]] — calls `hyalo-window-setup` on `window-setup-hook`
