# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Add god-mode toolbar pill (`GodModeView.swift`, `GodModeState.swift`) showing current editing state (control, literal, meta, control-meta, digit argument, universal argument) with per-state tint colors and icons
- Add god-mode Emacs integration (`hyalo-god-mode.el`) syncing Swift toolbar visibility and state directly from `god-local-mode`
- Add debug logging for the god-mode Emacs-to-Swift bridge on both the Lisp and Swift sides
- Add Shackle configuration (`lisp/hyalo-shackle.el`) to display popup buffers at bottom with sensible defaults for magit, help, compilation, and diagnostics
- Add iTermColors parser (`ITermColorsParser.swift`) to load terminal color schemes from .itermcolors files
- Add appearance-aware terminal palette (`TerminalPalette.swift`) with automatic light/dark switching
- Add terminal color extensions (`TerminalColorExtensions.swift`) with `applyPalette()` method for SwiftTerm
- Add comprehensive OSLog logging to terminal theming system for debugging theme loading and application
- Add `init-lang-yaml.el` with YAML mode setup and tree-sitter registration

### Removed

- Remove user/host and development environment segments from the environment pill; the pill now only appears when a build or activity is active
- Remove `EnvironmentBreadcrumbModel.swift`, `UserHostDropDownView.swift`, `EnvironmentDropDownView.swift`
- Remove `hyalo-environment.el` and all environment detection/push infrastructure
- Remove environment channel setup from `hyalo-channels.el`, `hyalo-window.el`, and `Module+StatusBar.swift`

### Changed

- Center the active buffer label horizontally in `EditorTabBarView` detail tab without affecting pill width or geometry
- Update god-mode toolbar state detection to follow transient key sequences from the god-mode command path instead of only settled post-command state
- Update the environment pill build segment to dismiss when no build activity remains active instead of pinning the last finished build inline
- Update `TerminalPalette` to search for nano themes in multiple locations: `~/.config/hyalo/`, `~/Library/Application Support/hyalo/`, and current working directory
- Update `hyalo-window--post-setup` to load `hyalo-shackle` module
- Update `InspectorTerminalView`, `UtilityAreaTerminalView` (macOS), and `UtilityAreaTerminalViewiOS` to use `@Bindable TerminalPalette` for automatic theme updates
- Update `HyaloManager` and `InspectorAppearanceCallbacks` to refresh `TerminalPalette` on appearance changes
- Update editor tab labels to show exact Emacs buffer names without display-side rewriting
- Update `init-modes.el` and docs to include YAML support and exclude Markdown from `treesit-auto`
- Update touched init files to remove stale comments and dead config stubs
- Audit and simplify the full Emacs Lisp configuration surface by removing duplicate setup, dead stubs, and stale commentary while preserving behavior

### Fixed

- Fix Swift minibuffer overlay clipping on the left edge by removing hard `minWidth: 680` and adding horizontal padding to keep the overlay within window bounds
- Suppress nested major-mode hooks during native Markdown code-block fontification to avoid extra Markdown buffer-open work
- Defer synchronous VC state refresh off the interactive file-open path so visiting tracked files does not block on immediate VC bookkeeping
- Stop polling buffer modified-state changes from a global `post-command-hook`; track real modified-flag transitions instead
- Disable automatic `global-treesit-auto-mode` activation while investigating the recent file-open latency regression
- Stop Hyalo status and environment sync hooks from rebuilding/pushing UI state during Emacs shutdown
- Guard `hyalo-minibuffer` behind a graphical display and fall back to the native minibuffer in TTY sessions
- Hide the shared Liquid Glass toolbar background for custom trailing items so keycast and package manager render as standalone pills without nested capsules
- Let the keycast toolbar pill use system-default height and give the package toolbar item its own Liquid Glass capsule pill
- Fix SwiftTerm terminal not loading theme by default: `TerminalPalette` now auto-loads nano themes from .itermcolors files on initialization, with embedded fallback colors
- Fix `hyalo-set-terminal-palette` Emacs function to use new `updateCurrentScheme()` API instead of directly setting read-only properties

### Changed

- Complete NavigatorViewModel migration: remove 15 deprecated properties (`bufferFilter`, `bufferList`, `selectedBuffer`, `activeBuffer`, `searchQuery`, `searchResults`, `findStatus`, `searchResultCount`, `searchFileCount`, `changedFiles`, `commitHistory`), route all NavigatorManager updates exclusively to focused ViewModels (`BufferListViewModel`, `SearchViewModel`, `SourceControlViewModel`)
- Migrate `SourceControlNavigatorView` from legacy `NavigatorViewModel` to `SourceControlViewModel` as data source
- Move `groupedResults` computed property from `FindNavigatorView` to `SearchViewModel` to keep collection transforms out of view bodies
- Change `BufferListView` from `@State` to `@Bindable` for singleton ViewModel reference (correct ownership semantics)
- Split `Module.swift` (2,403 lines) into 9 domain-specific files: `EmacsFFI.swift`, `Module+WindowSetup.swift`, `Module+Navigator.swift`, `Module+Panels.swift`, `Module+Editor.swift`, `Module+StatusBar.swift`, `Module+Appearance.swift`, `Module+Build.swift`; `Init()` reduced to 10-line dispatcher
- Replace 10+ hardcoded colors (`Color.white`, `Color.black`, `.gray`) across 8 files with semantic alternatives (`Color.primary`, `.secondary`, `Color(platformColor: .separator)`)
- Remove 8 unused stored properties from `HyaloWorkspaceState` (reduced from 25 to 17)
- Delete dead-code duplicate `InstantPopoverModifier.swift` from HyaloMac (shared version is superset)
- Remove unnecessary `import SwiftUI` from `ChannelBridge.swift`

### Added

- Add `DateFormatting` utility (`Sources/HyaloShared/Shared/DateFormatting.swift`) with cached `ISO8601DateFormatter` and `RelativeDateTimeFormatter` instances, replacing inline allocations in `HistoryInspectorView`, `SourceControlNavigatorView`, and `ToolbarManager`
- Add accessibility labels to `EditorTabBarView`, `CommandPaletteView`, `ProjectNavigatorView`, `SourceControlNavigatorView`, `BufferListView`
- Add `@Environment`-based dependency injection for navigator and inspector subsystems: `SourceControlViewModelKey`, `NavigatorManagerKey`, `InspectorManagerKey` in `EnvironmentKeys.swift`; parent views inject sub-VMs; child views use `@Environment` with singleton fallback
- Add Liquid Glass `.glassEffect(.regular, in: .rect)` to `WorkspacePanelTabBar` top layout for navigator/inspector tab bar headers
- Add `HyaloSharedTests` test target with 17 tests: `DateFormattingTests` (7) and `FuzzyMatcherTests` (10)

### Fixed

- Remove duplicate `relativeDate(from:)` implementations from `HistoryInspectorView` and `SourceControlNavigatorView` (now uses shared `DateFormatting.relativeDate(from:)`)
- Remove duplicate `import SwiftUI` in `InspectorAppearanceView.swift` iOS section

### Fixed

- Fix appearance mode switching from inspector panel not updating SwiftUI views. `colorTheme.isDark` was only refreshed on system appearance changes (via `platformIsDarkMode()` which reads `NSApp.effectiveAppearance`), but never updated when the user manually picks Light/Dark in the inspector. Now the `onAppearanceModeChanged` callback sets `colorTheme.isDark` directly based on the resolved mode ("light" → false, "dark" → true, "auto" → `refreshAppearance()`).

- Fix Emacs initialization error: `use-package` failed to parse `modus-themes` because `:ensure` requires a symbol or boolean, not a lisp expression. Replaced `:ensure (not (eq window-system 'ios))` with `:ensure t` and `:if (not (eq window-system 'ios))`.

- Fix iPadOS themes not applying: `modus-themes` was disabled on iOS via `:if` guard, but `nano-themes` and `hyalo-themes` depend on it. Remove the `:if` guard, use `:ensure` conditional (ELPA on macOS, bundled `etc/themes/` on iOS), and add `etc/themes/` to `load-path` on iOS so `(require 'modus-themes)` resolves
- Fix iPadOS minibuffer completion not available: all completion packages (vertico, orderless, etc.) were disabled on iOS. Add built-in `fido-vertical-mode` for iOS which provides vertical minibuffer completion without external packages

### Changed

- Migrate `GlassEffectContainer` from `.background(.ultraThinMaterial).clipShape()` to `.glassEffect(in:)` (Liquid Glass API, macOS 26). Remove redundant title header tint overlay; glass handles its own surface rendering.
- Remove orphaned `Sources/Hyalo/` directory (72 Swift files with no corresponding Package.swift target; dead code shadowing `Sources/HyaloMac/`)

- Fix inspector appearance panel not shown on iPadOS: `InspectorTab.appearance.body` is a computed property on an enum — `@Environment` cannot be a stored property. Add a private `AppearanceTabContent` helper struct (iOS-only, guarded with `#if !os(macOS)`) that reads `@Environment(HyaloWorkspaceState.self)` and renders `InspectorAppearanceView(workspace:)`.
- Fix iPadOS toolbar layout: sidebar toggle and branch picker were bundled in an `HStack` inside a single `ToolbarItem(placement: .topBarLeading)`. Split into two separate `ToolbarItem` entries so each receives its own independent tap target and layout slot.
### Fixed

- Fix SwiftUI shell never appearing on iPadOS: feedstock `ios_connect_frame_to_window` replaced the SwiftUI hosting controller with `EmacsViewController`, destroying the shell hierarchy. Add weak `ios_has_swiftui_host()` callback in feedstock; Swift overrides via `@_cdecl` to return `true`, causing `ios_connect_frame_to_window` to skip rootVC replacement. Move frame resize notification into `EmacsView.layoutSubviews` so it works without `EmacsViewController`. Make `EmacsContainerViewiOS.layoutSubviews` synchronous and defer `becomeFirstResponder` to next run-loop iteration.
- Add `EmacsLifecycle.markRunning()` called from `bridgeSetMainEmacsView`: `ios_emacs_init` never returns while Emacs is alive, so `lifecycle.state` never reached `.running` via the thread exit handler. `markRunning()` is the correct trigger, driven by `ios_set_main_emacs_view` from the feedstock when the Emacs UIView is ready. Thread exit handler now only handles the failure (non-zero return) case.
- Remove `@Published` from `emacsView` in `HyaloiOSModule`: `@Published` is an `ObservableObject` annotation and has no effect inside an `@Observable` class; property is now a plain stored var observed through the macro's synthesized access
- Replace `ObservableObject` with a plain `final class` for `UtilityAreaTerminalHolder` in the iOS stub (`HyaloShared/UtilityArea/UtilityAreaTerminalView.swift`): the stub is unused on iOS and `ObservableObject` is not appropriate here
- Fix missing closing braces in `EmacsViewsiOS.swift`: `updateUIView` body and `EmacsUIViewRepresentable` struct were unterminated, causing a compile error
- Fix iPadOS toolbar not rendering in iPad simulator: toolbar items placed on a `NavigationStack` nested inside `NavigationSplitView` detail closure do not propagate to the container navigation bar on iPad. Remove unnecessary `NavigationStack` wrapper, move `.toolbar {}` and `.navigationTitle()` onto the detail content view directly, add `.toolbarVisibility(.visible, for: .navigationBar)` to force bar visibility, and constrain `BranchPickerView` width to prevent layout overflow from its internal `.frame(maxWidth: .infinity)`.
- Add explicit sidebar toggle button (`sidebar.left` icon) to iPadOS toolbar leading edge: `NavigationSplitView`'s automatic toggle is displaced when custom items occupy `.topBarLeading`. Place the toggle in an `HStack` alongside the branch picker for consistent navigator show/hide.
- Remove mock data (`loadMockData()`) from iPadOS module: Emacs boots and reaches `.running` state, so fake buffers, tabs, and status bar values are misleading. Views now show real defaults (empty navigator, no tabs, zero status) until the channel bridge (Phase 7) wires Emacs data push.
- Fix Emacs face rendering on iPadOS (two root causes): (1) `ios_set_foreground_color` and `ios_set_background_color` in feedstock `ios/iosfns.m` set the UIColor on the `ios_output` struct but never wrote `FRAME_FOREGROUND_PIXEL(f)` / `FRAME_BACKGROUND_PIXEL(f)`, so `init_frame_faces` in xfaces.c realized the default face with `fg=0x0 bg=0x0` (both black). Add the missing assignments. (2) `default-frame-alist` in `init-appearance.el` set `(alpha-background . 0.0)` unconditionally; on iOS this caused xfaces.c to encode transparent background pixels. Guard `alpha-background` and `ns-alpha-elements` behind `(when (eq window-system 'ns) ...)` and clamp `f->alpha_background = 1.0` in `ios/iosfns.m` frame creation.

### Fixed

- Fix iPadOS startup crash "No font backend available" by adding `syms_of_macfont()` call to `syms_of_iosterm()` in feedstock, adding `HAVE_IOS` declaration for `syms_of_macfont` in `font.h`, and rebuilding libemacs.a with all patches applied via the pixi build workflow

### Changed

- Update `iOS/project.yml` to reference `emacs-build-ios-sim/src/libemacs.a` (properly patched build directory) instead of `emacs/src/libemacs.a` (unpatched submodule)

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
