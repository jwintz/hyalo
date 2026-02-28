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

### Changed

- Complete NavigatorViewModel migration: remove deprecated properties, route all updates to focused ViewModels
- Migrate `SourceControlNavigatorView` from legacy `NavigatorViewModel` to `SourceControlViewModel`
- Move `groupedResults` from `FindNavigatorView` to `SearchViewModel`
- Fix `BufferListView` property wrapper from `@State` to `@Bindable`
- Split `Module.swift` (2,403 → 216 lines) into 9 domain-specific extension files
- Replace hardcoded colors across 8 files with semantic alternatives
- Remove 8 unused stored properties from `HyaloWorkspaceState`
- Delete dead-code duplicate `InstantPopoverModifier.swift` from HyaloMac
- Remove unnecessary `import SwiftUI` from `ChannelBridge.swift`

### Added

- Add `DateFormatting` utility with cached formatters for ISO 8601 date parsing and relative formatting
- Add accessibility labels to editor tab bar, command palette, project navigator, source control, buffer list
- Add `@Environment`-based dependency injection for navigator and inspector subsystems (EnvironmentKeys, parent injection, child fallback pattern)
- Add Liquid Glass `.glassEffect()` to panel tab bar headers
- Add `HyaloSharedTests` test target with 17 tests (DateFormatting + FuzzyMatcher)

### Fixed

- Remove duplicate `relativeDate(from:)` implementations (now shared via `DateFormatting`)
- Remove duplicate `import SwiftUI` in `InspectorAppearanceView`
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
