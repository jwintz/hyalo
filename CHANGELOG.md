# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Add iOS initialization support to init files:
  - `init-bootstrap.el`: iOS-specific path setup with `HYALO_BUNDLE_PATH` environment variable, bundled package directory configuration, native compilation disabled
  - `init.el`: Skip dynamic module support check on iOS (modules are statically linked)
  - `early-init.el`: Skip dylib loading on iOS while preserving load-path setup
  - `init-hyalo.el` already has full iOS support with `(eq window-system 'ios)` checks and `hyalo-ios` module loading

### Fixed

- Navigator and inspector chrome areas (top tab bar, navigator bottom filter bar) used `.background(.bar)` — the system bar material — which rendered on top of and ignored the workspace background tint applied by the parent via `.background { Color(...).ignoresSafeArea() }`. Replace `.background(.bar)` with `.background(.clear)` in `HyaloPanelView` (top tab bar) and in `ProjectNavigatorView` / `BufferListView` (bottom filter bar) for both `Hyalo` and `HyaloShared` targets. The `Divider()` elements already provide visual separation; the workspace tint now extends uniformly through all panel chrome areas.
- Keycast placeholder icon left padding larger than top/bottom: `ControlGroup` on macOS 26 provides its own horizontal content insets; the additional `.padding(.horizontal, 6)` on both branches of `Hyalo/KeycastView` was stacking on top, creating wider horizontal insets than vertical. Remove the explicit horizontal padding from the macOS target entirely and let `ControlGroup` own all insets. `HyaloShared` version (which owns its own capsule background) retains its padding.
- `BuildStatusView` (segment 3 of the environment pill) lacked trailing inset: segments 1 and 2 apply `.padding(.horizontal, 6)` internally giving 11pt from the capsule edge (5pt outer + 6pt inner), while `BuildStatusView` had no inner padding. Add `.padding(.trailing, 6)` to align the right edge of "Building Hyalo module..." with the capsule boundary at the same depth as other segments.
- Keycast toolbar pill placeholder icon has excess horizontal padding due to redundant `frame(minWidth:minHeight:)` on the `Image`. Remove the frame constraint and reduce horizontal padding from 8 to 6 on both content and placeholder branches to match the environment pill's `.padding(.horizontal, 6)` design.
- Keycast toolbar pill has no left padding and wrong overflow priority. `ToolbarSpacer(.fixed)` between a flexible spacer and a custom-capsule item produces no visible gap on macOS 26; replaced with `.padding(.leading, 8)` inside `_KeycastToolbarContent`. `toolbarItemVisibilityPriority` was silently failing because `_ToolbarPriorityAnchor` called `isDescendant(of: item.view)` (wrong direction — walks down); fixed to walk UP the superview chain with `===` identity. Added `Coordinator.applied` flag and retry in `updateNSView` for robustness. Keycast now has `.low` (-1000) priority: it overflows before package manager (`.high` = 1000) and inspector (`.user` = 2000).
- Inspector and package manager toolbar pills sometimes shrink under space pressure. Root cause: wrapping single-action buttons in `ControlGroup` bridges them to `NSToolbarItemGroup` at the AppKit layer, which applies compression and collapse behaviour when horizontal space is constrained. Fix: remove `ControlGroup` from the inspector toggle, package manager, and keycast toolbar items. Single-action items now use `.buttonStyle(.bordered)` for Liquid Glass appearance. Keycast owns its own capsule background (`.background(.regularMaterial, in: Capsule())`). A new `ToolbarVisibilityPriorityModifier` (`NSViewRepresentable` anchor) walks to the enclosing `NSToolbarItem` on appear and sets `visibilityPriority`: `.user` (2000) for the inspector toggle so NSToolbar never auto-overflows it, `.high` (1000) for the package manager so it overflows before the inspector.
- System appearance change (dark/light mode switch) no longer triggers color theme re-render. `HyaloColorTheme.active` was calling `platformIsDarkMode()` — an external read invisible to SwiftUI's `@Observable` tracking graph — so no views ever re-evaluated on system appearance change. Fix: introduce a stored `isDark: Bool` property on `HyaloColorTheme` (tracked by `@Observable`) updated by `HyaloManager`'s `DistributedNotificationCenter` observer via `refreshAppearance()`. Route `HyaloWorkspaceState.isDarkMode` through `colorTheme.isDark` for the same reason.

### Added

- `hyalo-doctor.el`: `M-x hyalo-doctor` checks Swift version, macOS version, Xcode version, Emacs module support, required fonts (SF Mono, Recursive Mono Casual Static, Symbols Nerd Font Mono), required tools (rg, git, node), and optional tools/paths (typescript-language-server, iOS feedstock, hyalo-tengwar)

- `hyalo-build` and `hyalo-rebuild-and-reload` now open `*hyalo-build*` in `compilation-mode` in addition to their current behavior (Swift activity viewer tracking unchanged). For the async path (`hyalo-async-build`), a new `hyalo-build` channel (`hyalo-setup-build-channel`) fires three Emacs callbacks per build: `hyalo-channels--handle-build-start` (prepares the buffer), `hyalo-channels--handle-build-log-line` (streams each line with auto-scroll), `hyalo-channels--handle-build-finish` (appends a completion marker). For the sync fallback paths the buffer is prepared before `call-process-shell-command` runs and `compilation-mode` is activated after. Swift compiler output (`file:line:col: error:`) is parsed by `compilation-mode` enabling `next-error` / `M-g n` navigation to build errors.
- Initial revision: native macOS Emacs with SwiftUI chrome, NavigationSplitView layout, bidirectional Swift/Emacs Lisp channels, and integrated package/activity/terminal UI.
- Add `git-modes` package to `init-modes.el` for `.gitignore`, `.gitattributes`, and `.gitconfig` file support with proper comment syntax.
- Add command name-to-ID mapping functions (`hyalo-ios-command-name-to-id`, `hyalo-ios-command-id-to-name`) to `hyalo-ios.el` for translating between string command names and integer command IDs
- Update `hyalo-ios-dispatch` function to accept either string command names (e.g., "eval") or integer command IDs, enabling simpler test syntax like `(hyalo-ios-dispatch "eval" "(+ 1 2)")`

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
