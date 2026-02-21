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
| `toml-mode` | MELPA | TOML configuration file editing |
| `swift-mode` | MELPA | Swift source editing |
| `typescript-mode` | MELPA | TypeScript source editing |
| `git-modes` | GNU ELPA | Git file support (gitignore, gitconfig, gitattributes) |

## Configuration Highlights

### json-mode

```elisp
(use-package json-mode
  :mode "\\.json\\'")
```

### toml-mode

```elisp
(use-package toml-mode
  :mode "\\.toml\\'")
```

### swift-mode

```elisp
(use-package swift-mode
  :mode "\\.swift\\'")
```

### typescript-mode

```elisp
(use-package typescript-mode
  :mode "\\.ts\\'")
```

### git-modes

Provides `gitignore-mode`, `gitconfig-mode`, and `gitattributes-mode` with proper comment syntax (`#`) for header2.el compatibility.

```elisp
(use-package git-modes
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/attributes\\'" . gitattributes-mode)
         ("/\\.gitattributes\\'" . gitattributes-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/\\.gitconfig\\'" . gitconfig-mode)))
```

> **Note:** Tree-sitter is disabled. All modes use standard font-lock and indentation engines.
