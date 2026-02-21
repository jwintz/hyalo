---
title: init-tengwar
description: Optional Tengwar script rendering via overlays (requires hyalo-tengwar)
navigation:
  icon: i-lucide-type
order: 14
tags:
  - init
  - emacs-lisp
  - configuration
  - tengwar
  - optional
  - typography
---

`init-tengwar` provides optional Tengwar script rendering using font overlays. The module is silently skipped at startup if the `hyalo-tengwar` dynamic module is not installed, so it never blocks the rest of the init sequence.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `hyalo-tengwar` | external (`git clone`) | Tengwar rendering engine as a dynamic module |

## Configuration Highlights

### Graceful optional load

```elisp
(when (locate-library "hyalo-tengwar")
  (require 'hyalo-tengwar))
```

### Configurable module path

```elisp
(defcustom hyalo-tengwar-path nil
  "Path to the hyalo-tengwar dynamic module.
When nil, Emacs's standard `load-path` is searched."
  :type '(choice (const nil) file)
  :group 'hyalo)
```

### Font requirements

Tengwar rendering requires one or more Tengwar-compatible fonts installed in `~/Library/Fonts/`:

- **Tengwar Annatar** — the primary rendering font
- **Free Tengwar** fonts — fallback alternatives

### Installation

```sh
git clone https://github.com/jwintz/hyalo-tengwar
```

Follow the repository's build instructions to compile the dynamic module, then set `hyalo-tengwar-path` to the resulting `.so` / `.dylib` file if it is not on `load-path`.
