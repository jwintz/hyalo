---
title: Changelog
description: Release history and notable changes to Hyalo
navigation:
  icon: i-lucide-history
  order: 99
order: 99
tags:
  - changelog
  - releases
  - hyalo
---

All notable changes to Hyalo are documented here. The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

:changelog-versions{:versions='[{"title":"Unreleased","description":"Initial revision"}]'}

---

## Unreleased

### Fixed

- Fix Emacs initialization error: `use-package` failed to parse `modus-themes` because `:ensure` requires a symbol or boolean, not a lisp expression. Replaced `:ensure (not (eq window-system 'ios))` with `:ensure t` and `:if (not (eq window-system 'ios))`.

### Added

- Initial revision: native macOS Emacs with SwiftUI chrome, NavigationSplitView layout, bidirectional Swift/Emacs Lisp channels, and integrated package/activity/terminal UI.
- Add `git-modes` package to `init-modes.el` for `.gitignore`, `.gitattributes`, and `.gitconfig` file support with proper comment syntax.

### Changed

- Update README.md init files table to include all 14 modules with accurate descriptions.
- Update README.md Lisp files table: rename `hyalo-theme.el` to `hyalo-themes.el`, add missing modules (activities, compile, diagnostics, environment, gutter, keycast, lib, menu, minimap, package, splash, system).
- Update vault `init/modes.md` documentation to include `git-modes` package.

### Fixed

- Make HyaloShared view models and UI types public for HyaloMac access.
