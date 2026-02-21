---
title: init-bootstrap
description: Package archives, GC tuning, and shell PATH synchronisation
navigation:
  icon: i-lucide-rocket
order: 1
tags:
  - init
  - emacs-lisp
  - configuration
  - startup
  - package-management
---

`init-bootstrap` is the first module loaded. It configures package archives, raises the GC threshold for a faster startup, and synchronises the shell `PATH` into Emacs before any other module runs.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `exec-path-from-shell` | MELPA | Sync `PATH` (and other env vars) from `.zshrc`/`.zshenv` |

## Configuration Highlights

### Package archives

```elisp
(setq package-archives
      '(("melpa"    . "https://melpa.org/packages/")
        ("gnu"      . "https://elpa.gnu.org/packages/")
        ("nongnu"   . "https://elpa.nongnu.org/packages/")))
```

### GC tuning

```elisp
;; Raise threshold during init to reduce GC pauses; restored afterwards.
(setq gc-cons-threshold most-positive-fixnum)
```

### Shell PATH sync

```elisp
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
```

### Bootstrap guard

If `.local/` or `.local/elpa/` is missing, the bootstrap installs all declared packages before the rest of the init sequence proceeds.
