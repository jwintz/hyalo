---
title: init-help
description: Enhanced help buffers and elisp symbol reference lookup
navigation:
  icon: i-lucide-info
order: 9
tags:
  - init
  - emacs-lisp
  - configuration
  - help
  - helpful
  - elisp-refs
---

`init-help` replaces Emacs's default help commands with richer, more navigable alternatives and adds cross-reference search for elisp symbols.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `helpful` | MELPA | Enhanced `*Help*` buffers with source, examples, and references |
| `elisp-refs` | MELPA | Find all references to any elisp function, variable, or macro |

## Configuration Highlights

### helpful — rebind built-in help keys

```elisp
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))
```

### elisp-refs

```elisp
(use-package elisp-refs
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))
```
