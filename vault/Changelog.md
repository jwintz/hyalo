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

### Added

- Add Shackle configuration to display popup buffers at bottom with sensible defaults for magit, help, compilation, and diagnostics
- Add iTermColors parser to load terminal color schemes from .itermcolors files
- Add appearance-aware terminal palette with automatic light/dark switching
- Add terminal color extensions with `applyPalette()` method for SwiftTerm
- Add `init-lang-yaml.el` with YAML mode setup and tree-sitter registration

### Changed

- Update `TerminalPalette` to search for nano themes in multiple locations: `~/.config/hyalo/`, `~/Library/Application Support/hyalo/`, and current working directory
- Update terminal views to use `@Bindable TerminalPalette` for automatic theme updates
- Update appearance callbacks to refresh `TerminalPalette` on appearance changes
- Update editor tab labels to show exact Emacs buffer names without display-side rewriting
- Update `init-modes.el` and docs to include YAML support and keep Markdown on plain `markdown-mode`
- Remove stale comments and dead config stubs from touched init files
- Audit and simplify the full Emacs Lisp configuration surface by removing duplicate setup, dead stubs, and stale commentary while preserving behavior
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
- Fix SwiftTerm terminal not loading theme by default: `TerminalPalette` now auto-loads nano themes from .itermcolors files on initialization, with embedded fallback colors
- Fix `hyalo-set-terminal-palette` Emacs function to use new `updateCurrentScheme()` API
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
