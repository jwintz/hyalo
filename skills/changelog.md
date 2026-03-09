# Hyalo Changelog Skill

## Overview

`vault/Changelog.md` is the documentation site's changelog page. It mirrors `CHANGELOG.md` at the repo root and uses the lithos `:changelog-versions` component for a rendered version history.

## Page Structure

```markdown
---
title: Changelog
description: Release history and notable changes to Hyalo
navigation:
  icon: i-lucide-history
order: 99
---

All notable changes to Hyalo are documented here. The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

:changelog-versions{:versions='[...]'}

---

## Unreleased

### Added
...

## v0.1.0 - YYYY-MM-DD

### Added
...
```

## The `:changelog-versions` Component

The JSON array controls the rendered version navigation UI:

```json
[
  {
    "title": "Unreleased",
    "description": "Preparing v0.1.0"
  },
  {
    "title": "v0.1.0",
    "date": "2026-03-01",
    "description": "Initial release",
    "to": "https://github.com/jwintz/hyalo/releases/tag/v0.1.0"
  }
]
```

Fields:
- `title` — version string shown in the UI (required)
- `date` — ISO date string (omit for Unreleased)
- `description` — short release summary
- `to` — URL to the GitHub release (omit for Unreleased)

## Preparing a Release

1. In `CHANGELOG.md` (repo root): move `[Unreleased]` entries to `## [v0.1.0] - YYYY-MM-DD`
2. In `vault/Changelog.md`:
   - Add the new version to the `:changelog-versions` JSON array (prepend after Unreleased)
   - Move `## Unreleased` content to `## v0.1.0 - YYYY-MM-DD`
   - Reset `## Unreleased` to empty

## Categories

Use only these standard categories (imperative mood):

- **Added** — new features
- **Changed** — changes to existing functionality
- **Deprecated** — soon-to-be removed features
- **Removed** — removed features
- **Fixed** — bug fixes
- **Security** — security fixes

## Sync Rule

`vault/Changelog.md` and `CHANGELOG.md` must always be in sync. When one is updated, update the other.
