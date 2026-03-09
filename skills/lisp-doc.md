# Lisp Module Documentation Template

A lisp module doc page lives under `vault/3.lisp/N.module-name.md`. It follows Apple documentation style — concise, structured, factual.

## Frontmatter

```yaml
---
title: module-name          # Without .el
description: One-line description of what the module does
navigation:
  icon: i-lucide-<icon>
order: N
---
```

## Page Structure

```markdown
> Commentary from the file's `;;; Commentary:` section (as a blockquote).

## Overview

1–3 sentences explaining the module's architectural role and key responsibilities.

## Functions

### `function-name`
`(arg1 arg2 &optional arg3)` → return-type

Docstring-based description. Mention any important side effects or hooks.

```elisp
;; Example usage
(hyalo-example-function "arg1" 42)
```

### `another-function`
...

## Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `var-name` | `nil` | What it tracks |

## Hooks Used

| Hook | Handler | Purpose |
|------|---------|---------|
| `window-buffer-change-functions` | `hyalo-sync--push` | Trigger on buffer switch |

## Custom Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `hyalo-auto-build` | `boolean` | `t` | Auto-build module if missing |
```

## Rules

- Use `elisp` as the fenced code block language
- Do not document private functions (prefixed `--`) unless they are architecturally significant
- Keep the Variables table short — only document vars that a user or developer might configure or depend on
- Do not invent behavior — base all descriptions on the actual source file
- If the module has no public functions, write a brief Overview and omit the Functions section
