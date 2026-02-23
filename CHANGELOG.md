# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Fixed

- System appearance change (dark/light mode switch) no longer triggers color theme re-render. `HyaloColorTheme.active` was calling `platformIsDarkMode()` — an external read invisible to SwiftUI's `@Observable` tracking graph — so no views ever re-evaluated on system appearance change. Fix: introduce a stored `isDark: Bool` property on `HyaloColorTheme` (tracked by `@Observable`) updated by `HyaloManager`'s `DistributedNotificationCenter` observer via `refreshAppearance()`. Route `HyaloWorkspaceState.isDarkMode` through `colorTheme.isDark` for the same reason.

### Added

- Initial revision: native macOS Emacs with SwiftUI chrome, NavigationSplitView layout, bidirectional Swift/Emacs Lisp channels, and integrated package/activity/terminal UI.
- Add `git-modes` package to `init-modes.el` for `.gitignore`, `.gitattributes`, and `.gitconfig` file support with proper comment syntax.

### Changed

- Update README.md init files table to include all 14 modules with accurate descriptions.
- Update README.md Lisp files table: rename `hyalo-theme.el` to `hyalo-themes.el`, add missing modules (activities, compile, diagnostics, environment, gutter, keycast, lib, menu, minimap, package, splash, system).
- Update vault `init/modes.md` documentation to include `git-modes` package.

### Changed

- Add `#if canImport(UIKit)` guards to all HyaloiOS source files so `swift build` succeeds on macOS without `--target`
- Simplify `iOS/build.sh` to use `/tmp/hyalo-ios-build` as derivedData path, add `--resources-only` flag
- Update `.gitignore` to ignore generated `iOS/HyaloApp.xcodeproj/` and `iOS/HyaloApp/Resources/`
- Update README.md with platform architecture, iOS build commands, and iOS lisp files

### Removed

- Remove stale `iOS/build-ios/` DerivedData directory

### Fixed

- Fix Phase 1 type migration by moving misplaced types from HyaloMac to HyaloShared (HyaloDesign, CommandItem, OpenQuicklyItem, HyaloContentUnavailableView, GeometrySizeTracker, EffectView, InstantPopoverModifier, DropdownItemStyle, TerminalPalette)
- Add `#if os(macOS)` platform guards for macOS-specific types (EffectView, InstantPopoverModifier, UtilityAreaTerminalView, UtilityAreaTerminalHolder, InspectorAppearanceView)
- Make HyaloShared view models and UI types public for HyaloMac access
- Fix iOS simulator crash: add `embed: true` to HyaloKit package dependency in `project.yml` so XcodeGen generates "Embed Frameworks" build phase
- Fix `swift build` failing on macOS when building all targets (HyaloKit imported UIKit unconditionally)
