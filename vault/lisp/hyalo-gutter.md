---
title: hyalo-gutter
description: Git gutter diff status relay to the Swift gutter overlay
navigation:
  icon: i-lucide-git-commit-horizontal
order: 7
tags:
  - lisp
  - module
  - git
---

> Git gutter integration — diff-hl / git-gutter status pushed to the Swift gutter overlay. Provides per-line change classification (added, modified, deleted) without duplicating gutter rendering inside Emacs.

## Overview

`hyalo-gutter.el` reads the hunk data produced by `diff-hl` or `git-gutter` for the current buffer and forwards it to Swift, which renders the gutter overlay alongside the Emacs frame. This keeps the visual gutter managed by SwiftUI while the diff computation remains in Emacs.

## Functions

### `hyalo-gutter-setup`
**()** → nil

Installs hooks on the active gutter backend (`diff-hl` or `git-gutter`) so that `hyalo-gutter--push` is called whenever hunk data is refreshed.

### `hyalo-gutter--push`
**()** → nil

Collects the current buffer's hunk list, serializes it to a structured payload, and sends it to Swift over the gutter channel.
