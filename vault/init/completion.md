---
title: init-completion
description: Minibuffer and in-buffer completion stack — Vertico, Consult, Corfu, and friends
navigation:
  icon: i-lucide-search
order: 6
tags:
  - init
  - emacs-lisp
  - configuration
  - completion
  - vertico
  - consult
  - corfu
  - cape
---

`init-completion` builds a composable completion stack: a vertical minibuffer UI, rich annotations, fuzzy matching, powerful search commands, contextual actions, and inline code completion.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `vertico` | GNU ELPA | Vertical completion UI for the minibuffer |
| `marginalia` | GNU ELPA | Annotations (type, docstring) in minibuffer candidates |
| `orderless` | GNU ELPA | Space-separated, any-order completion style |
| `consult` | GNU ELPA | Enhanced search and navigation commands |
| `embark` | MELPA | Contextual actions on minibuffer candidates |
| `embark-consult` | MELPA | Embark + Consult integration |
| `corfu` | GNU ELPA | In-buffer completion popup |
| `cape` | GNU ELPA | Completion-at-point extensions (file, dabbrev, …) |

## Configuration Highlights

### vertico

```elisp
(use-package vertico
  :init
  (vertico-mode))
```

### orderless

```elisp
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
```

### consult

```elisp
(use-package consult
  :bind (("C-s"   . consult-line)
         ("C-c f" . consult-find)
         ("C-c r" . consult-recent-file)))
```

### corfu

```elisp
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))
```

### cape

```elisp
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))
```
