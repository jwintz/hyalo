---
title: init-editing
description: Modal editing, window navigation, code folding, and semantic selection
navigation:
  icon: i-lucide-square-pen
order: 5
tags:
  - init
  - emacs-lisp
  - configuration
  - editing
  - god-mode
  - avy
  - windmove
---

`init-editing` adds editing ergonomics on top of the built-in experience: a modal layer via god-mode, directional window movement, outline folding, and fast cursor jumping.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `display-line-numbers` | built-in | Line numbers with stable width |
| `god-mode` | MELPA | Modal editing — command mode without modifier keys |
| `windmove` | built-in | Navigate windows with Shift+arrow keys |
| `outline-minor-mode` | built-in | Code folding via outline headings |
| `avy` | MELPA | Jump to any visible text with a few keystrokes |
| `expand-region` | MELPA | Incrementally expand the selection by semantic units |

## Configuration Highlights

### Line numbers

```elisp
(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-width 2)
  (display-line-numbers-widen t)
  (display-line-numbers-grow-only t))
```

### god-mode

```elisp
(use-package god-mode
  :bind (("C-x C-1" . god-mode-all)
         ("C-x C-2" . god-local-mode)
         ("C-x C-3" . god-mode)))
```

### windmove

```elisp
(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings))  ; Shift+arrow
```

### avy

```elisp
(use-package avy
  :bind ("M-j" . avy-goto-char-timer))
```

### expand-region

```elisp
(use-package expand-region
  :bind ("C-=" . er/expand-region))
```
