---
title: init-appearance
description: Fonts, themes, inactive-buffer dimming, and icon sets
navigation:
  icon: i-lucide-palette
order: 4
tags:
  - init
  - emacs-lisp
  - configuration
  - themes
  - fonts
  - appearance
  - modus-themes
---

`init-appearance` configures the visual layer: typeface selection, theme families, inactive-buffer dimming, and nerd-icon integration across Dired, ibuffer, and the minibuffer.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `modus-themes` | GNU ELPA | Accessible light/dark theme pair |
| `nano-themes` | MELPA | nano-light / nano-dark minimal themes |
| `ef-themes` | GNU ELPA | Colourful, well-contrast theme collection |
| `iota-dimmer` | MELPA | Dims inactive buffers to reduce visual noise |
| `lin` | GNU ELPA | Highlight current line in list-style buffers |
| `nerd-icons` | MELPA | Nerd Font icon library |
| `nerd-icons-dired` | MELPA | Icons in Dired buffers |
| `nerd-icons-ibuffer` | MELPA | Icons in ibuffer |
| `nerd-icons-completion` | MELPA | Icons in minibuffer completion |

## Configuration Highlights

### Fonts

```elisp
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 110)   ; 11 pt

(set-face-attribute 'variable-pitch nil
                    :family "Recursive Mono Casual"
                    :height 110)
```

### iota-dimmer

```elisp
(use-package iota-dimmer
  :diminish
  :config
  (iota-dimmer-mode 1))
```

### lin

```elisp
(use-package lin
  :config
  (lin-global-mode 1))
```

### nerd-icons

```elisp
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
```

## See Also

- [[lisp/hyalo-appearance|hyalo-appearance.el]] — syncs active theme colors to Swift
- [[swift/appearance|Swift Appearance]] — Liquid Glass materials, color theme mapping
- [[features/appearance|Appearance feature overview]]
