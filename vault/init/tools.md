---
title: init-tools
description: Project management, Git porcelain, LSP, and syntax checking
navigation:
  icon: i-lucide-wrench
order: 8
tags:
  - init
  - emacs-lisp
  - configuration
  - magit
  - eglot
  - lsp
  - flymake
  - project
---

`init-tools` wires up the development toolchain: project navigation, Git integration with fringe diff indicators, an LSP client, and both built-in and external syntax checkers.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `project` | built-in | Project root detection and per-project commands |
| `magit` | MELPA | Full-featured Git porcelain |
| `transient` | MELPA | Transient command menus (magit guard) |
| `diff-hl` | GNU ELPA | VCS diff indicators in the fringe |
| `eglot` | built-in (30.1+) | LSP client |
| `flymake` | built-in | On-the-fly syntax checking |
| `flycheck` | MELPA | Extended syntax checking with many checkers |

## Configuration Highlights

### magit

```elisp
(use-package magit
  :bind ("C-x g" . magit-status))
```

### diff-hl

```elisp
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
```

### eglot

```elisp
(use-package eglot
  :ensure nil
  :hook ((swift-mode . eglot-ensure)
         (python-mode . eglot-ensure)))
```

### transient history guard

Emacs crashes mid-save can leave `transient-history-file` with a truncated sexp. On the next load, transient reads it as a non-list value and every subsequent alist operation signals `wrong-type-argument`. Three defences are applied:

```elisp
(with-eval-after-load 'transient
  ;; 1. Validate immediately after load
  (unless (proper-list-p transient-history)
    (setq transient-history nil))

  ;; 2. Guard transient-save-history at exit
  (advice-add 'transient-save-history :around
              (lambda (fn &rest args)
                (condition-case err
                    (progn
                      (unless (proper-list-p transient-history)
                        (setq transient-history nil))
                      ;; Drop malformed (SYMBOL . LIST) entries
                      (setq transient-history
                            (cl-remove-if-not
                             (lambda (e) (and (consp e) (symbolp (car e))
                                             (proper-list-p (cdr e))))
                             transient-history))
                      (apply fn args))
                  (error
                   (setq transient-history nil)
                   (ignore-errors (apply fn args))))))

  ;; 3. Runtime guard on transient-setup
  (advice-add 'transient-setup :around
              (lambda (fn &rest args)
                (unless (proper-list-p transient-history)
                  (setq transient-history nil))
                (apply fn args))))
```

### flymake

```elisp
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))
```
