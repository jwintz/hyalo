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

`init-modes` registers major modes for the file types Hyalo commonly edits. It enables `treesit-auto` for selected programming and data formats while keeping Markdown on plain `markdown-mode`.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `treesit-auto` | MELPA | Prompt-driven tree-sitter grammar install and major-mode remapping |
| `json-mode` | MELPA | JSON editing and syntax highlighting |
| `toml-mode` | MELPA | TOML configuration file editing |
| `swift-mode` | MELPA | Swift source editing |
| `typescript-mode` | MELPA | TypeScript source editing |
| `yaml-mode` | MELPA | YAML editing and syntax highlighting |
| `git-modes` | GNU ELPA | Git file support (gitignore, gitconfig, gitattributes) |

## Configuration Highlights

### treesit-auto

```elisp
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt
        treesit-auto-langs (delq 'markdown (copy-sequence treesit-auto-langs)))
  (global-treesit-auto-mode))
```

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

### yaml-mode

```elisp
(use-package yaml-mode
  :mode (("Procfile\\'" . yaml-mode)
         ("\\.ya?ml\\'" . yaml-mode)))
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

> **Note:** Markdown is explicitly excluded from `treesit-auto`, so `.md` files stay on `markdown-mode` even when a Markdown grammar is installed.
