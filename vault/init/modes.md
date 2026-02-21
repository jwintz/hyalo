---
title: init-modes
description: Language major modes for structured data and application code
navigation:
  icon: i-lucide-code
order: 7
tags:
  - init
  - emacs-lisp
  - configuration
  - major-modes
  - languages
  - tree-sitter
---

`init-modes` registers major modes for the file types Hyalo commonly edits. Tree-sitter grammars are intentionally disabled in favour of the simpler standard major modes.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `json-mode` | MELPA | JSON editing and syntax highlighting |
| `yaml-mode` | MELPA | YAML editing and syntax highlighting |
| `swift-mode` | MELPA | Swift source editing |
| `markdown-mode` | MELPA | Markdown editing (also used by `init-markdown`) |
| `toml-mode` | MELPA | TOML configuration file editing |

## Configuration Highlights

### json-mode

```elisp
(use-package json-mode
  :mode "\\.json\\'")
```

### yaml-mode

```elisp
(use-package yaml-mode
  :mode ("\\.ya?ml\\'"))
```

### swift-mode

```elisp
(use-package swift-mode
  :mode "\\.swift\\'")
```

### toml-mode

```elisp
(use-package toml-mode
  :mode "\\.toml\\'")
```

> **Note:** Tree-sitter is disabled. All modes use standard font-lock and indentation engines.
