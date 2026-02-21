---
title: iota-faces
description: Extended face definitions for the iota theme family
navigation:
  icon: i-lucide-paintbrush
order: 21
tags:
  - lisp
  - module
  - appearance
---

> Extended face definitions for the iota theme family. Defines additional faces used by `iota-theme-transparent.el` and `nano-themes.el` variants, providing a consistent visual language across all iota-derived color schemes.

## Overview

`iota-faces.el` declares the supplementary faces that the iota theme system layers on top of Emacs's built-in face set. These faces cover UI regions — fringe gradients, inactive modeline variants, overlay tints, and window-divider styles — that standard themes do not address. Both `iota-theme-transparent.el` and the nano-themes wrappers in `hyalo-themes.el` depend on these definitions.

## Faces Defined

Faces in this module follow the `iota-` prefix convention and are grouped by UI region:

- **Fringe** — `iota-fringe`, `iota-fringe-inactive`
- **Mode line** — `iota-modeline-inactive`, `iota-modeline-highlight`
- **Window divider** — `iota-window-divider`, `iota-window-divider-inactive`
- **Overlay tints** — `iota-overlay-dim`, `iota-overlay-highlight`
- **Header line** — `iota-header-line`, `iota-header-line-inactive`
