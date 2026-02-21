---
title: hyalo-package
description: Package management toolbar integration with upgradable-package detection
navigation:
  icon: i-lucide-package
order: 13
tags:
  - lisp
  - module
  - packages
---

> Package management toolbar integration. Detects upgradable packages and pushes their count and metadata to the Swift `PackageManagerView`. `:vc` packages are surfaced in the list but excluded from the badge count.

## Overview

`hyalo-package.el` compares installed package versions in `package-alist` against the latest available versions in `package-archive-contents` using `version-list-<`, collecting packages where an upgrade is available. The result is pushed to Swift as a structured payload that drives a toolbar badge and an expandable package list.

## Functions

### `hyalo-package-setup`
**()** → nil

Installs hooks so that `hyalo-package--push-status` runs after `package-refresh-contents` completes, keeping the toolbar badge current without manual refresh.

### `hyalo-package--push-status`
**()** → nil

Enumerates upgradable packages, separates `:vc`-sourced packages (detected via `(eq (package-desc-kind desc) 'vc)`), and pushes the full list plus a numeric badge count to Swift over the `package` channel.

### `hyalo-package-refresh`
**()** → nil

Triggers `package-refresh-contents` asynchronously, guarded by `hyalo-package--refreshing` to prevent concurrent refreshes.

### `hyalo-package-upgrade-all`
**()** → nil

Upgrades all packages with available updates. Sets `hyalo-package--upgrading` during the operation and pushes a final status once complete.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-package--refreshing` | `nil` | Non-nil while a `package-refresh-contents` call is in flight; prevents concurrent refreshes |
| `hyalo-package--upgrading` | `nil` | Non-nil while an upgrade batch is running |
