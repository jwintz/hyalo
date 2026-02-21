---
title: hyalo-environment
description: Development environment detection and breadcrumb push to the Swift toolbar
navigation:
  icon: i-lucide-layers-2
order: 6
tags:
  - lisp
  - module
  - toolbar
  - environment
---

> Development environment detection. Detects active toolchains (Pixi, Conda, npm, Bun, Swift, Rust, Python venv, Docker) per project root and pushes user/host and environment state to the Swift toolbar breadcrumb via the `hyalo-environment` channel. All state changes are driven by hooks — no polling.

## Overview

`hyalo-environment.el` detects the development context for the current project and pushes it to Swift so the toolbar breadcrumb stays current. Detection runs when the buffer, file, or project changes. Results are cached and compared against the previous push, so Swift only receives a message when the state actually changes.

User/host info (`username`, `hostname`) is pushed separately via `hyalo-update-user-host`. The environment list is pushed via `hyalo-update-environments`, encoded as a JSON array of environment descriptors.

## Functions

### `hyalo-environment-setup`
**()** → nil

Installs hooks for `find-file-hook`, `window-buffer-change-functions`, `after-save-hook`, and `project-switch-project-hook`. Schedules an initial push via an idle timer so Emacs is fully settled before the first detection runs.

### `hyalo-environment-teardown`
**()** → nil

Removes all hooks installed by `hyalo-environment-setup` and cancels any pending debounce timer.

### `hyalo-environment-refresh`
**()** → nil

Interactive command. Clears all caches (user/host, environment list, toolchain versions) and immediately pushes fresh state to Swift. Useful for debugging or after environment changes outside of Emacs.

### `hyalo-environment-diagnostics`
**()** → nil

Interactive command. Opens a `*Hyalo Environment Diagnostics*` buffer reporting the current project root, buffer directory, detected environments, and their paths.

### `hyalo-environment--push`
**()** → nil

Public entry point called from `hyalo-status--push`. Skips transient, minibuffer, and internal buffers, then debounces the actual detection and push via a 0.1-second idle timer.

### `hyalo-environment--detect-all`
**()** → list | nil

Runs all per-toolchain detectors in priority order and returns a list of environment alists. Returns `nil` when no project root is found, which preserves the previously pushed state rather than clearing the breadcrumb.

## Detectors

| Detector | Trigger |
|----------|---------|
| `hyalo-environment--detect-pixi` | `.pixi/` directory or `PIXI_ENVIRONMENT_NAME` env var |
| `hyalo-environment--detect-conda` | `CONDA_DEFAULT_ENV` env var or `environment.yml` |
| `hyalo-environment--detect-bun` | `bun.lockb` or `bun.lock` |
| `hyalo-environment--detect-npm` | `package.json` or `node_modules/` |
| `hyalo-environment--detect-swift` | `Package.swift`, `.swift-version`, or `Sources/` with `.swift` files |
| `hyalo-environment--detect-rust` | `Cargo.toml` or `rust-toolchain.toml` |
| `hyalo-environment--detect-python-venv` | `.venv/` or `venv/` directory |
| `hyalo-environment--detect-docker` | `Dockerfile` or `docker-compose.yml` |

Each detector returns an alist with keys `type`, `name`, `icon`, `isActive`, and `path`, or `nil` if not detected.

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `hyalo-environment--last-user-host` | `nil` | Cached user/host alist; push is skipped when unchanged |
| `hyalo-environment--last-environments` | `nil` | Cached environment list; push is skipped when unchanged |
| `hyalo-environment--last-had-project` | `nil` | Tracks project presence across calls to detect project-loss transitions |
| `hyalo-environment--push-idle-timer` | `nil` | Pending debounce timer; cancelled and replaced on each call |
| `hyalo-environment--node-version-cache` | `nil` | Cached output of `node --version` |
| `hyalo-environment--swift-version-cache` | `nil` | Cached output of `swift --version` |

## Hooks Used

| Hook | Purpose |
|------|---------|
| `find-file-hook` | Push when a new file is opened |
| `window-buffer-change-functions` | Push when the visible buffer changes |
| `after-save-hook` | Push after a save (toolchain files may have been written) |
| `project-switch-project-hook` | Push when the active project changes |

## Channel Callbacks

Swift can invoke these functions via the `hyalo-environment` channel:

| Function | Purpose |
|----------|---------|
| `hyalo-environment--switch` | Activate the named environment type |
| `hyalo-environment--open-terminal` | Open the utility area terminal at the project root |
| `hyalo-environment--copy-ssh-command` | Copy an SSH command for the current host to the clipboard |

## See Also

- [[swift/core|Swift Core]] — registers `hyalo-update-user-host`, `hyalo-update-environments`, and `hyalo-setup-environment-channel`
- [[swift/toolbar|Swift Toolbar]] — renders the breadcrumb driven by this module
- [[lisp/hyalo-status|hyalo-status.el]] — calls `hyalo-environment--push` from `hyalo-sync--push`
- [[lisp/hyalo-channels|hyalo-channels.el]] — opens the `hyalo-environment` channel at startup
