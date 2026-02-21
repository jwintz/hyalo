---
title: init-header
description: Automatic file header creation and maintenance via header2
navigation:
  icon: i-lucide-file-text
order: 10
tags:
  - init
  - emacs-lisp
  - configuration
  - file-headers
  - header2
---

`init-header` loads the vendored `header2` library and hooks it into the save and major-mode lifecycle so that file headers are created on new files and kept up-to-date on every save.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `header2` | `lisp/` (vendored) | Programmatic file header creation and auto-update |

## Configuration Highlights

### Loading the vendored library

```elisp
(use-package header2
  :ensure nil
  :load-path "lisp/")
```

### Hooks

```elisp
  :hook ((write-file-functions . auto-update-file-header)
         (emacs-lisp-mode      . auto-make-header))
```

### Editor keybindings (under `C-c e`)

```elisp
(leader-def
  "em" #'make-header
  "ec" #'make-box-comment
  "ed" #'make-divider)
```
