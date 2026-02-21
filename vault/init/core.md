---
title: init-core
description: Foundational keybinding infrastructure, minor-mode suppression, and discovery
navigation:
  icon: i-lucide-cpu
order: 2
tags:
  - init
  - emacs-lisp
  - configuration
  - keybindings
  - which-key
  - general
---

`init-core` establishes the keybinding layer used by all subsequent modules and suppresses noisy minor-mode lighters from the mode line.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `diminish` | MELPA | Suppress minor-mode lighters in the mode line |
| `general` | MELPA | Structured keybinding definitions with a `leader-def` prefix |
| `which-key` | MELPA | Pop-up displaying available keybinding completions |

## Configuration Highlights

### Leader prefix

```elisp
(general-create-definer leader-def
  :prefix "C-c")
```

### Top-level prefix groups

```elisp
(leader-def
  "b" '(:ignore t :which-key "buffer")
  "e" '(:ignore t :which-key "editor")
  "f" '(:ignore t :which-key "file")
  "h" '(:ignore t :which-key "help"))
```

### which-key

```elisp
(use-package which-key
  :diminish
  :config
  (which-key-mode))
```
