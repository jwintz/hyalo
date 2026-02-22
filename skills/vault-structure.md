# Hyalo Vault Structure

## Overview

The Hyalo vault lives at `vault/` in the repository root. It is an Obsidian-flavored markdown vault rendered by [lithos](https://github.com/jwintz/lithos) (Nuxt 4 / Docus v5) and published at `jwintz.github.io/hyalo`.

## Directory Layout

```
vault/
├── nuxt.config.ts       # site name only: { site: { name: 'Hyalo' } }
├── index.md             # Landing page (navigation: false)
├── logo.svg             # Hyalo logo (copy of lisp/hyalo-splash.svg)
├── logo-blink.svg       # Blink variant (same file for now)
├── Assets/              # Images referenced as /_raw/Assets/...
│   └── Hyalo-Landing-N.png
├── 1.features/          # Feature overview pages
│   ├── index.md         # Features overview
│   ├── 1.appearance.md
│   ├── 2.native-compilation.md
│   ├── 3.init-system.md
│   ├── 4.lisp-side.md
│   ├── 5.swift-side.md
│   └── 6.architecture.md
├── 2.init/              # Init module documentation
│   ├── index.md         # Init system overview
│   └── 1.bootstrap.md … 14.tengwar.md
├── 3.lisp/              # Lisp module documentation (Apple doc style)
│   ├── index.md
│   └── 1.hyalo.md … 20.iota-faces.md
├── 4.swift/             # Swift module documentation (Apple doc style)
│   ├── index.md
│   └── 1.core.md … 12.shared.md
└── Changelog.md         # Uses :changelog-versions component
```

## Numbering Convention

Folders and files are prefixed with `N.` to control sidebar order. Lower numbers appear first. The `index.md` in each folder has `order: 0` in frontmatter.

## Frontmatter Schema

Every content page uses:

```yaml
---
title: Human Readable Title
description: One-line description for SEO and sidebar tooltips
navigation:
  icon: i-lucide-<icon-name>   # Lucide icon identifier
order: N                        # Integer, controls sort within folder
---
```

Landing page (`index.md`) uses `navigation: false`.

## Assets

Images go in `vault/Assets/`. Reference them in markdown as:

```markdown
![Alt text](/_raw/Assets/Hyalo-Landing-1.png)
```

The `/_raw/` prefix serves files directly from the vault root in dev mode; the deploy workflow copies them to the output directory.

## Wikilinks

Use Obsidian wikilinks for internal navigation:

```markdown
[[2.init|Init Modules]]
[[3.lisp/1.hyalo|hyalo.el]]
```

## Changelog

`Changelog.md` uses the lithos `:changelog-versions` component:

```markdown
:changelog-versions{:versions='[{"title":"Unreleased","description":"..."},{"title":"v0.1.0","date":"YYYY-MM-DD","description":"...","to":"https://github.com/jwintz/hyalo/releases/tag/v0.1.0"}]'}
```

## Key Constraints

- No references to CodeEdit or Xcode
- No references to lithos internals (it's a dependency, not content)
- All code examples use `elisp` or `swift` language identifiers
- Keep descriptions factual and concise — Apple documentation style
