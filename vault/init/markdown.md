---
title: init-markdown
description: Markdown editing, structured note-taking, and Obsidian vault integration
navigation:
  icon: i-lucide-file-code
order: 11
tags:
  - init
  - emacs-lisp
  - configuration
  - markdown
  - denote
  - obsidian
  - notes
---

`init-markdown` enhances Markdown editing and wires up two complementary note-taking systems: `denote` for structured plain-text notes and `obsidian` for interoperability with Obsidian vaults.

## Packages

| Package | Source | Purpose |
|---------|--------|---------|
| `markdown-mode` | MELPA | Full Markdown editing with preview and font-lock |
| `denote` | GNU ELPA | Note-taking with structured, date-stamped filenames |
| `obsidian` | MELPA | Obsidian vault integration (wikilinks, backlinks) |

## Configuration Highlights

### markdown-mode — theme-aware pre-block background

```elisp
(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  ;; Sync pre-block background to the active theme's block face.
  (set-face-attribute 'markdown-pre-face nil
                      :inherit 'org-block))
```

### denote

```elisp
(use-package denote
  :custom
  (denote-directory (expand-file-name "~/notes/"))
  (denote-known-keywords '("project" "journal" "reference")))
```

### obsidian

```elisp
(use-package obsidian
  :custom
  (obsidian-directory (expand-file-name "~/Syntropment/hyalo/vault/"))
  :config
  (obsidian-specify-path obsidian-directory)
  (global-obsidian-mode 1))
```
