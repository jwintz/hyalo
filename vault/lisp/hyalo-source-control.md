---
title: hyalo-source-control
description: Git branch, status, and log relay to the Swift SourceControlNavigatorView
navigation:
  icon: i-lucide-git-branch
order: 14
tags:
  - lisp
  - module
  - git
---

> SCM data: git branch, status, and log — pushed to the Swift `SourceControlNavigatorView`. Git subprocess calls are debounced to avoid thrashing during rapid file saves or index updates.

## Overview

`hyalo-source-control.el` runs lightweight `git` subprocesses to collect branch name, working-tree status, and recent log entries, then pushes the results to Swift over the `source-control` channel. A debounce timer coalesces rapid hook firings into a single subprocess invocation.

## Functions

### `hyalo-source-control-setup`
**()** → nil

Installs hooks on `after-save-hook` and `magit-post-refresh-hook` (when available) to trigger debounced pushes whenever the repository state may have changed.

### `hyalo-source-control--push-branch`
**()** → nil

Resolves the current git branch name via a `git rev-parse --abbrev-ref HEAD` subprocess and pushes it to Swift.

### `hyalo-source-control--push-status`
**()** → nil

Runs `git status --porcelain` and `git log` to collect working-tree changes and recent commits, serializes the results, and sends the payload to Swift over the `source-control` channel.
