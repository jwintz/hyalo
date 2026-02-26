# Issue I1: Toolbar Visibility in iOS Simulator

## Problem
The toolbar was not visible in the iOS Simulator. The HyaloiOSNavigationLayout had a `.toolbar` modifier applied to `NavigationSplitView`, but on iPad, this doesn't automatically show a navigation bar like it does on macOS.

## Root Cause
1. **iOS App Entry Point**: The `iOS/HyaloApp/HyaloApp.swift` was using a placeholder `ContentView` instead of the actual `HyaloRootView` from the HyaloiOS module. This meant the actual HyaloiOS implementation was never being used.

2. **NavigationSplitView Toolbar Behavior**: On iPad/iOS, `NavigationSplitView` does not automatically show a navigation bar with toolbars like macOS does. The toolbar modifier on `NavigationSplitView` requires the detail content to be wrapped in a `NavigationStack` for the toolbar to appear.

## Fix Applied

### 1. Updated iOS App Entry Point
Changed `iOS/HyaloApp/HyaloApp.swift` to use `HyaloRootView`:
```swift
import SwiftUI
import UIKit
import HyaloKit

struct ContentView: View {
    var body: some View {
        HyaloRootView()
    }
}
```

### 2. Wrapped Detail Content in NavigationStack
Modified `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift` to wrap the detail view content in a `NavigationStack` with the toolbar configuration:

```swift
NavigationSplitView(columnVisibility: $columnVisibility) {
    NavigatorAreaView(workspace: workspace)
        .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
} detail: {
    NavigationStack {
        DetailView(...)
            .navigationTitle("")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .topBarLeading) { ... }
                ToolbarItem(placement: .principal) { ... }
                ToolbarItemGroup(placement: .topBarTrailing) { ... }
            }
    }
    .sheet(...)
}
```

### 3. Extracted DetailView
Created a separate `DetailView` struct to keep the code organized and ensure proper view hierarchy.

## Verification
- Built and launched app in iOS Simulator (iPad Pro 13-inch)
- Screenshot saved to `.sisyphus/evidence/issue-i1-toolbar.png`
- Toolbar now displays with:
  - Branch picker (top-left)
  - Environment pill (center)
  - Action buttons: Open Quickly, Command Palette, Inspector Toggle, Utility Area Toggle (top-right)

## Date Fixed
2026-02-25

---

# Issue I2: Duplicate DetailView Definition

## Problem
iOS build failed with "invalid redeclaration of 'DetailView'" error. Two `DetailView` structs existed:
1. `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift` (lines 140-178, marked `private`)
2. `Sources/HyaloiOS/Window/DetailView.swift` (lines 9-56, public by default)

## Root Cause
During development, `DetailView` was extracted to a separate file but the original definition in `HyaloiOSNavigationLayout.swift` was not removed, causing a duplicate symbol error.

## Fix Applied
Removed the duplicate `private struct DetailView` from `HyaloiOSNavigationLayout.swift` (lines 138-179). Kept the standalone `DetailView.swift` file as the single source of truth.

## Verification
- iOS build command: `xcodebuild -project iOS/HyaloApp.xcodeproj -scheme HyaloApp -destination 'platform=iOS Simulator,name=iPad Pro 13-inch (M5)' -derivedDataPath /tmp/hyalo-ios-build build`
- Build succeeded with no errors

## Date Fixed
2026-02-25

---

# Issue I3: Scope Cleanup for DetailView Fix

## Problem
Previous fix for duplicate DetailView introduced unrelated changes to multiple files (boulder.json, plans, HyaloApp.swift, init-tools.el, UtilityAreaTerminalView.swift) and created junk files (AUDIT.md, elgrep-data.el, evidence files).

## Fix Applied
Reverted all unrelated changes to HEAD and removed junk files. Kept only the minimal fix:
- `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift` - duplicate DetailView removed
- `Sources/HyaloiOS/Window/DetailView.swift` - kept as single source
- `.sisyphus/notepads/ipados-port/issues.md` - this note

## Verification
- `git diff --stat` shows only the three files above
- iOS build passes with no duplicate symbol errors

## Date Fixed
2026-02-25

---

# Issue I2: Emacs text rendering incomplete on iOS

## Problem
Emacs text rendering was incomplete on iOS. The `ns-alpha-elements` frame parameter was being set to non-nil values (e.g., `ns-alpha-default`, `ns-alpha-glyphs`) which caused text rendering issues on iOS.

## Root Cause
The `hyalo-appearance.el` module sets `ns-alpha-elements` for macOS to enable transparency effects. However, on iOS, this parameter must be set to `nil` (default) for correct text rendering. The existing code did not have iOS-specific guards, so the macOS settings were being applied to iOS as well.

## Fix Applied

### 1. Added iOS-specific appearance setup in `hyalo-ios.el`
Created `hyalo-ios-setup-appearance()` function that sets `ns-alpha-elements` to `nil` for iOS:
```elisp
(defun hyalo-ios-setup-appearance ()
  "Setup appearance for iOS.
On iOS, ns-alpha-elements must be set to nil/default for correct text rendering."
  (hyalo-log 'window "iOS appearance setup")
  (when (display-graphic-p)
    (dolist (f (frame-list))
      (set-frame-parameter f 'ns-alpha-elements nil))))
```

This function is called in:
- `hyalo-window--early-setup()` - during early window initialization
- `hyalo-window-setup()` - during full window setup
- `hyalo-ios-init()` - during iOS bridge initialization

### 2. Added iOS guards in `hyalo-appearance.el`
Updated three functions to skip setting `ns-alpha-elements` on iOS:
- `hyalo-appearance--apply-frame-settings()` - added `(not (eq window-system 'ios))` guard
- `hyalo-appearance--setup-frame()` - added `(not (eq window-system 'ios))` guard
- `hyalo-appearance--update-fringe-alpha()` - added `(not (eq window-system 'ios))` guard

These guards ensure that macOS-specific transparency settings are not applied to iOS frames.

## Verification
- iOS build command: `xcodebuild -project iOS/HyaloApp.xcodeproj -scheme HyaloApp -destination 'platform=iOS Simulator,name=iPad Pro 13-inch (M5)' -derivedDataPath /tmp/hyalo-ios-build build`
- Build succeeded with no errors
- No changes to macOS behavior (guards only affect iOS)

## Date Fixed
2026-02-25

---

# Issue I3: Emacs Buffer Interaction Testing

## Test Date
2026-02-25

## Test Environment
- iOS Simulator: iPad Pro 13-inch (M5)
- Device UDID: 65B71F4A-5614-485E-A5EF-EEAD2D35D1E9
- App Build: /tmp/hyalo-ios-build/Build/Products/Debug-iphonesimulator/Hyalo.app

## Test Scope
Buffer interaction testing covering:
1. Buffer switching via channel bridge
2. Window splitting functionality
3. Text editing capabilities

## Test Results Summary

### Status: INFRASTRUCTURE VERIFIED - Manual Testing Required

All core infrastructure components are operational:
- Emacs boots successfully from source (pdmp fingerprint mismatch expected)
- Window system initializes (ios_term_init, ios_create_frame)
- Text renders to screen (150+ glyph draw calls observed)
- Channel bridge architecture in place

### Detailed Results

#### 1. Buffer Switching
**Infrastructure Verified:**
- App launches without crash
- Emacs initializes completely
- *scratch* buffer displays
- Channel bridge components exist
- Navigator selection flow documented
- Race condition fixes implemented (stale callback filtering)

**Requires Manual Verification:**
- Navigator tap -> buffer switch via reverse channel
- Buffer list updates propagated to Swift UI
- Kill buffer operation

**Evidence:** `.sisyphus/evidence/issue-i3-buffer-switch.txt`

#### 2. Window Splitting
**Infrastructure Verified:**
- Window system initialized
- Frame created: 1022x1372 pixels, 73 cols x 49 lines
- Display info allocated and functional
- Standard Emacs window code operational

**Requires Manual Verification:**
- C-x 2 (split-window-below)
- C-x 3 (split-window-right)
- C-x o (other-window navigation)
- C-x 0/1 (window deletion)

**Evidence:** `.sisyphus/evidence/issue-i3-window-split.txt`

#### 3. Text Editing
**Rendering Verified:**
- Text rendering functional (ios_draw_glyph_string)
- Cursor rendering working (ios_draw_window_cursor)
- Frame clear operations functional
- Font rendering via CoreText (macfont.m iOS patches)
- Multiple face IDs used (0-23 for syntax highlighting)
- Input accessory view created

**Requires Manual Verification:**
- Live keyboard input handling
- Text selection highlighting
- Cut/copy/paste (iOS clipboard integration)
- Undo/redo functionality

**Evidence:** `.sisyphus/evidence/issue-i3-text-edit.txt`

### Technical Findings

#### Process Status
```
Hyalo App: PID 21722 (running stable)
Simulator: Booted and responsive
Bundle: All resources present (lisp/, init/, etc/, bootstrap-emacs.pdmp)
```

#### Frame Parameters
- Screen: 1032x1376 points
- Frame: 1022x1372 pixels
- Scale: 2.0x (Retina)
- Character: 14x28 pixels
- Capacity: 73 columns x 49 lines

#### Rendering Pipeline
- Updates: ios_update_begin/end working
- Glyphs: 150+ draw calls during init
- Cursor: Position tracking functional
- Colors: Black on white (bg=0xffffff, fg=0x0)

### Known Limitations

#### Testing Constraints
1. No automated UI interaction available via simctl
2. No keyboard automation (requires manual input)
3. No exposed channel bridge CLI for automated testing
4. Accessibility dump not available through simctl ui

#### Previous Fixes Applied
- Issue I2: Alpha elements disabled for iOS (ns-alpha-elements set to nil)
- Y-coordinate handling fixed in macfont.m Phase 5.1
- Race condition fixes in navigator selection

### Evidence Files Created

1. `.sisyphus/evidence/issue-i3-initial-state.png` - Screenshot of running app
2. `.sisyphus/evidence/issue-i3-buffer-switch.txt` - Buffer switching test details
3. `.sisyphus/evidence/issue-i3-window-split.txt` - Window splitting test details
4. `.sisyphus/evidence/issue-i3-text-edit.txt` - Text editing test details
5. `.sisyphus/evidence/issue-i3-buffer-interactions.txt` - Summary report

### Recommendations

To complete full verification:
1. Manual UI testing with keyboard input
2. Visual verification of window splits
3. Buffer switching via navigator sidebar taps
4. Text editing operations (type, select, copy, paste, undo)

For future automation:
1. Add XCTest/XCUITest suite
2. Expose channel bridge CLI endpoint
3. Add accessibility identifiers to SwiftUI views
4. Implement mock channel commands for CI/CD

### Conclusion

The Hyalo iOS build demonstrates:
- Successful Emacs bootstrap (140+ files loaded)
- Functional text rendering (150+ glyphs)
- Stable process execution (no crashes)
- Complete channel bridge infrastructure

**Manual testing required** to verify end-to-end user interactions.

---

# Issue I4: Toolbar Wiring Restored

## Problem
Toolbar was missing after redeploy because the iOS app entry point was using a placeholder `ContentView` instead of `HyaloRootView()` from the HyaloiOS module.

## Root Cause
The `iOS/HyaloApp/HyaloApp.swift` file had reverted to using a placeholder `ContentView` with mock UI (black rectangle with "Emacs Frame" text). This bypassed the actual HyaloiOS implementation which includes `HyaloiOSNavigationLayout` with the toolbar configuration.

## Fix Applied
Restored the proper app entry point by:
1. Replacing `ContentView()` with `HyaloRootView()` in the `WindowGroup`
2. Removing the placeholder `ContentView` struct with mock UI
3. Ensuring the SwiftUI view hierarchy uses `HyaloiOSNavigationLayout` via `HyaloRootView`

## Verification
- File modified: `iOS/HyaloApp/HyaloApp.swift` only
- `HyaloRootView()` now used as the root view
- Placeholder UI removed
- Toolbar visibility relies on HyaloRootView -> HyaloiOSNavigationLayout

## Date Fixed
2026-02-25

---

# Issue I5: iOS Toolbar Parity with macOS

## Problem
iOS toolbar was missing the KeycastView and PackageManagerView that exist in the macOS toolbar, breaking feature parity between platforms.

## Root Cause
The iOS `HyaloiOSNavigationLayout` toolbar only included BranchPickerView and EnvironmentPillView from macOS, but omitted KeycastView and PackageManagerView.

## Fix Applied
Modified `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift` to add:

1. **KeycastView** - Shows current key binding and command name in toolbar
   - Uses `ToolbarManager.shared.viewModel` (same as macOS)
   - Added padding for proper spacing

2. **PackageManagerView** - Shows package status with badge count and popover
   - Uses `ToolbarManager.shared.viewModel` (same as macOS)
   - Fixed size to prevent layout issues

## Toolbar Structure (Post-Fix)

```swift
ToolbarItem(placement: .topBarLeading) {
    BranchPickerView(viewModel: ToolbarManager.shared.viewModel)
}
ToolbarItem(placement: .principal) {
    EnvironmentPillView(workspace: workspace)
}
ToolbarItemGroup(placement: .topBarTrailing) {
    KeycastView(viewModel: ToolbarManager.shared.viewModel)
    PackageManagerView(viewModel: ToolbarManager.shared.viewModel)
    // iOS-specific buttons preserved:
    Button { showOpenQuickly = true } ...
    Button { showCommandPalette = true } ...
    Button { workspace.inspectorVisible.toggle() } ...
    Button { workspace.utilityAreaVisible.toggle() } ...
}
```

## Verification
- `swift build --target HyaloKit` succeeds
- No duplicate button blocks
- Uses shared view model and shared views (no mock data)
- Preserves iOS-specific buttons (Open Quickly, Command Palette, Inspector, Utility Area)

## Date Fixed
2026-02-25

---

# Issue I6: iOS App Entry Point Restored

## Problem
iOS app entry point was using placeholder `ContentView` instead of `HyaloRootView()`, preventing the real toolbar from rendering.

## Root Cause
The `iOS/HyaloApp/HyaloApp.swift` file had reverted to using a placeholder `ContentView` with mock UI (black rectangle with "Emacs Frame" text). This bypassed the actual HyaloiOS implementation which includes `HyaloiOSNavigationLayout` with the toolbar configuration.

## Fix Applied
Restored the proper app entry point by:
1. Replacing `ContentView()` with `HyaloRootView()` in the `WindowGroup`
2. Removing the placeholder `ContentView` struct with mock UI
3. Ensuring the SwiftUI view hierarchy uses `HyaloiOSNavigationLayout` via `HyaloRootView`

## Verification
- File modified: `iOS/HyaloApp/HyaloApp.swift` only
- `HyaloRootView()` now used as the root view
- Placeholder UI removed
- Toolbar visibility relies on HyaloRootView -> HyaloiOSNavigationLayout

## Date Fixed
2026-02-25

---

# Issue I6: Restore iOS Toolbar Parity with macOS Shared Views

## Problem
iOS toolbar was missing parity with macOS shared views. Investigation revealed the toolbar implementation already contained the shared views (BranchPickerView, EnvironmentPillView, KeycastView, PackageManagerView) but the app entry point was using a placeholder ContentView instead of HyaloRootView, preventing the toolbar from displaying.

## Root Cause
1. **App Entry Point**: iOS/HyaloApp/HyaloApp.swift contained a placeholder ContentView struct with mock UI (black rectangle with "Emacs Frame" text) and used it in WindowGroup instead of HyaloRootView()
2. **Toolbar Wiring**: The actual toolbar implementation in HyaloiOSNavigationLayout was correct but never rendered because the app entry point bypassed it

## Fix Applied

### 1. Restored HyaloRootView as Entry Point
Modified iOS/HyaloApp/HyaloApp.swift:
- Changed WindowGroup content from ContentView() to HyaloRootView()
- Removed placeholder ContentView struct with mock UI

### 2. Verified Toolbar Structure
Confirmed Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift toolbar contains:
- BranchPickerView (.topBarLeading) - uses ToolbarManager.shared.viewModel
- EnvironmentPillView (.principal) - shows workspace breadcrumb
- KeycastView (.topBarTrailing group) - uses ToolbarManager.shared.viewModel
- PackageManagerView (.topBarTrailing group) - uses ToolbarManager.shared.viewModel
- Open Quickly button - triggers sheet for file search
- Command Palette button - triggers sheet for command execution
- Inspector toggle - shows/hides right sidebar
- Utility Area toggle - shows/hides bottom panel

All views use shared ToolbarManager.shared.viewModel (no mock data).

### 3. Confirmed No Duplicate Toolbars
- Only one .toolbar modifier exists in the iOS codebase (in HyaloiOSNavigationLayout.swift)
- No duplicate toolbar blocks or orphaned toolbar configurations

## Verification
- File modified: iOS/HyaloApp/HyaloApp.swift - restored HyaloRootView entry point
- File verified: Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift - toolbar complete
- Toolbar items present: BranchPickerView, EnvironmentPillView, KeycastView, PackageManagerView, Inspector toggle
- iOS actions preserved: Open Quickly, Command Palette, Utility Area toggle
- Uses shared ToolbarManager.shared.viewModel (no mock data)
- No duplicate toolbar blocks

## Date Fixed
2026-02-25

---

# Issue I7: BranchPickerView pill — system iOS 26 NavBar glass treatment

## Date
2026-02-25

## Status
OPEN

## Problem
After guarding macOS-only APIs with `#if os(macOS)`, the BranchPickerView *still* renders inside a pill capsule on iOS 26. The pill is **system-applied** by the iOS 26 NavigationBar glass system — any `ToolbarItem(.topBarLeading)` containing a `Menu` or interactive content receives automatic pill/capsule glass background. Our fix removed Hyalo-specific hover styling but did not suppress the system chrome.

## Root Cause
iOS 26 liquid glass navbar applies a pill/capsule background to ToolbarItems with interactive content (Menu, Button). This is a SwiftUI system behaviour, not Hyalo code.

## Fix Required
Apply `.buttonStyle(.plain)` and/or `.background(.clear)` on the outer HStack/Menu inside BranchPickerView on iOS, or use `.toolbarBackground(.hidden, for: .navigationBar)` scoped to the item. Investigation needed: does wrapping the BranchPickerView content in a plain button style or using `.controlGroupStyle(.navigation)` suppress the pill?

Candidate fix (to test):
```swift
// In HyaloiOSNavigationLayout.swift, on the BranchPickerView ToolbarItem:
ToolbarItem(placement: .topBarLeading) {
    BranchPickerView(viewModel: ToolbarManager.shared.viewModel)
        .frame(minWidth: 80, maxWidth: 200)
        // iOS 26: suppress automatic pill/glass treatment
}
```
Or alternatively in BranchPickerView.swift, inside the `#else` iOS block, add `.buttonStyle(.plain)` and `.background(.clear)` on the outermost HStack.

## Files
- `Sources/HyaloShared/Toolbar/BranchPickerView.swift`
- `Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift`

---

# Issue I8: Appearance panel does not show current theme name

## Date
2026-02-25

## Status
OPEN

## Problem
The "Theme > Current" section in InspectorAppearanceView shows nothing on iOS. `workspace.currentThemeName` is always empty string on iOS.

## Root Cause
`currentThemeName` is set only via `hyalo-set-current-theme-name`, which is implemented as a macOS `env.defun()` in `HyaloMac/Core/Module.swift:1558`. No `@_cdecl` FFI equivalent exists in `Sources/HyaloiOS/Bridge/ChannelBridge.swift`.

On iOS:
- `hyalo-channels-ios.el` lines 113/159 call `(hyalo-set-current-theme-name (symbol-name theme))` via `(when (fboundp 'hyalo-set-current-theme-name) ...)` — but `hyalo-set-current-theme-name` is just the `hyalo--ios-function` stub, which is a no-op.
- Swift side never receives the theme name → `workspace.currentThemeName` stays `""` → theme section hidden.

## Fix Required
Add a `@_cdecl("hyalo_ios_set_current_theme_name")` function to `ChannelBridge.swift`:
```swift
@_cdecl("hyalo_ios_set_current_theme_name")
func bridgeSetCurrentThemeName(_ nameCString: UnsafePointer<CChar>) {
    let name = String(cString: nameCString)
    DispatchQueue.main.async {
        if #available(iOS 26.0, *) {
            HyaloiOSModule.shared.workspace.currentThemeName = name
        }
    }
}
```
Then expose it to Emacs via `hyalo-ios.el` (the `hyalo--ios-function` stub is already present at line 165-ish — verify name matches).

The `(when (fboundp 'hyalo-set-current-theme-name) ...)` guards in hyalo-channels-ios.el will then find the real C function bound.

## Files
- `Sources/HyaloiOS/Bridge/ChannelBridge.swift` — add @_cdecl
- `lisp/hyalo-ios.el` — verify stub name matches (should already be there)

---

# Issue I9: Emacs view not retina resolution

## Date
2026-02-25

## Status
OPEN

## Problem
Emacs text renders at 1x (non-retina) on iOS 26 iPad simulator. Text appears blurry/pixelated vs native UIKit controls.

## Root Cause (hypothesis)
The feedstock's `EmacsView.layer.contentsScale` is set to `UIScreen.mainScreen.scale` at init time. When the view is reparented from the feedstock's `EmacsViewController` into `EmacsContainerViewiOS` (SwiftUI), the layer `contentsScale` may not be re-applied after reparenting. Specifically:
- `ios_has_swiftui_host()` returns `true` → feedstock skips `EmacsViewController` rootVC setup
- `EmacsContainerViewiOS.layoutSubviews` sets `emacsView.frame = bounds` (correct size)
- But `emacsView.layer.contentsScale` may not match the SwiftUI host's screen scale after reparenting

Alternatively, the simulator may report scale=2.0 but the frame pixel dimensions passed to Emacs are in points not pixels.

## Fix Required
In `EmacsContainerViewiOS.layoutSubviews` (or `didMoveToWindow`), ensure:
```swift
emacsView?.layer.contentsScale = window?.screen.scale ?? UIScreen.main.scale
emacsView?.contentScaleFactor = window?.screen.scale ?? UIScreen.main.scale
```
This guarantees the Emacs rendering surface uses native pixel density after SwiftUI reparenting.

## Files
- `Sources/HyaloiOS/Editor/EmacsViewsiOS.swift`

---

# Issue I10: Minibuffer not visible (M-x)

## Date
2026-02-25

## Status
OPEN

## Problem
Invoking M-x (execute-extended-command) on iOS — the minibuffer/echo area is not visible. It may be occluded by the keyboard accessory bar or positioned outside the visible frame.

## Root Cause (hypothesis)
Under SwiftUI hosting with `ios_has_swiftui_host() = true`:
- Emacs frame fills the SwiftUI container (EmacsContainerViewiOS)
- The feedstock's keyboard-height adjustment is done in `EmacsViewController.keyboardWillShow` by modifying `_bottomConstraint`
- BUT: since `EmacsViewController` layout constraints are **not active** in SwiftUI mode, keyboard appearance does NOT shrink the Emacs frame
- Result: the minibuffer (bottom row of Emacs frame) is hidden under the software keyboard

The keyboard accessory bar (Esc/Ctrl/Alt/Tab/arrows) is rendered by the feedstock as `inputAccessoryView` of EmacsView — it sits above the keyboard. But the minibuffer row of the Emacs text frame may be below the keyboard.

Need to investigate whether `ios_request_frame_resize` is called when keyboard appears in SwiftUI mode.

## Fix Required (investigation needed)
1. Check if `keyboardWillShow:` / `keyboardDidChangeFrame:` is still being called when EmacsView is in SwiftUI hierarchy (it should be, since EmacsView registers for these notifications directly)
2. Check if `ios_request_frame_resize` is called with the keyboard-adjusted height
3. If not: wire keyboard height notification → update `EmacsContainerViewiOS` frame height → trigger Emacs resize

May require feedstock patch to `iosterm.m` or a new Swift-side keyboard observer.

## Files
- `Sources/HyaloiOS/Editor/EmacsViewsiOS.swift`
- `~/Syntropment/hyalo-feedstock-unified/ios/iosterm.m` (potential patch, human commits feedstock)

---

# Issue I11: Channels connectivity audit

## Date
2026-02-25

## Status
OPEN (partial)

## Channels connected
- `hyalo_ios_setup_channels` — wired ✓
- Navigator, editor tabs, status, toolbar, open-quickly, command-list — all have `@_cdecl` FFI ✓
- `hyalo_ios_appearance_set_mode` — wired ✓ (Emacs→Swift direction)
- `DispatchRouter.sendCommand(.appearanceChange)` — wired ✓ (Swift→Emacs direction, fixed ea8613d)

## Missing / Broken
- `hyalo-set-current-theme-name` → no `@_cdecl` → Issue I8
- `hyalo-set-workspace-appearance` → has an Elisp stub in hyalo-channels-ios.el, calls `hyalo-ios-appearance-set-mode` → appears OK
- `ios-system-appearance-change-functions` hook → `Vios_system_appearance` is set by feedstock but hyalo-ios.el does not hook it → R3 (auto mode always dark fallback)

## Not yet audited
- Search channel (`hyalo_ios_search_*`)
- Diagnostics channel
- Package channel
- Source control channel

---

# Issue I11: Auto appearance always dark

## Date
2026-02-25

## Status
FIX IMPLEMENTED — awaiting confirmation

## Problem
The `ios-system-appearance-change-functions` hook (defined in feedstock `iosterm.m`) was never hooked in `hyalo-themes.el`, so iOS theme sync never responded to system dark/light mode changes. The initial theme was always loaded as dark (fallback).

## Root Cause
1. `hyalo-theme-sync` had a re-entrancy guard for `ns-system-appearance-change-functions` but not for `ios-system-appearance-change-functions`
2. `hyalo-theme-setup` hooked `ns-system-appearance-change-functions` but not `ios-system-appearance-change-functions`
3. `hyalo-theme-setup`'s initial theme detection cond had no case for `ios-system-appearance`

## Fix Applied

### Change 1: Re-entrancy guard in `hyalo-theme-sync` (line 148-149)
Added `ios-system-appearance-change-functions nil` to the existing `ns-system-appearance-change-functions nil` guard:
```elisp
(let ((ns-system-appearance-change-functions nil)
      (ios-system-appearance-change-functions nil))
```
This prevents the iOS hook from re-firing while a theme load is in progress.

### Change 2: Hook iOS system appearance changes in `hyalo-theme-setup` (lines 198-200)
Added after the existing `ns-system-appearance-change-functions` hook:
```elisp
  ;; iOS: hook system appearance changes
  (when (boundp 'ios-system-appearance-change-functions)
    (add-hook 'ios-system-appearance-change-functions #'hyalo-theme-sync))
```

### Change 3: Initial theme detection in `hyalo-theme-setup` (lines 210-212)
Added iOS case to the cond before the TTY case:
```elisp
                  ;; iOS: use ios-system-appearance
                  ((and (boundp 'ios-system-appearance) ios-system-appearance)
                   ios-system-appearance)
```

## Why this works
- Feedstock `iosterm.m` declares `ios-system-appearance` (Lisp variable, value: `'dark` or `'light`) and `ios-system-appearance-change-functions` (hook list, called with `'dark` or `'light` arg when system appearance changes)
- The hook calling convention matches `ns-system-appearance-change-functions` — both call their functions with a single `appearance` symbol argument
- `hyalo-theme-sync` already accepts a single `appearance` symbol argument — no signature change needed
- The re-entrancy guard prevents a loop if the hook fires during a theme load
- All changes use `boundp` guards so macOS is unaffected

## Verification
- `swift build --target Hyalo` passes (macOS build unaffected)
- Only `lisp/hyalo-themes.el` was modified

---

# Issue I12: Massive duplication in init files from previous edit

## Date
2026-02-26

## Status
FIX IMPLEMENTED — awaiting confirmation

## Problem
Previous edit left MASSIVE DUPLICATION in three init files:
- `init/init-appearance.el`: Every `use-package` form had duplicate `:ensure t` and `:demand t` lines, and old font `set-face-attribute` calls were left after the new iOS `if` block
- `init/init-completion.el`: Every `use-package` form was duplicated entirely
- `init/init-bootstrap.el`: Mostly correct but verified

## Root Cause
Previous subagent used an unstable model and left duplicate lines everywhere. The INTENT of the changes (iOS guards on third-party packages) was correct, but the EXECUTION was broken — lines were inserted but originals not removed.

## Fix Applied

### 1. Rewrote `init/init-appearance.el` (306 lines)
Removed all duplicate lines:
- Lines 12-13: duplicate comment "Set font preferences before loading themes" → removed
- Lines 31-43: OLD font `set-face-attribute` calls → removed
- Lines 47-52: fontaine duplicate `:ensure t` and `:demand t` → kept only ONE of each
- Lines 124-131: modus-themes duplicate `:ensure` and `:demand t` → kept only the `:ensure (not (eq window-system 'ios))` form
- Lines 136-144: ef-themes duplicate `:ensure t` and `:demand t` → kept only ONE of each
- Lines 214-221: nerd-icons duplicate `:ensure t` and `:demand t` → kept only ONE of each
- Lines 253-260: lin duplicate `:ensure t` → kept only ONE
- Lines 273-276: hide-mode-line duplicate `:ensure t` and stray `)` → fixed
- Lines 278-290: demap duplicate `:ensure t` → kept only ONE
- Lines 294-300: olivetti duplicate `:ensure t` → kept only ONE
- Lines 307-321: mixed-pitch duplicate `:ensure t` → kept only ONE

Correct pattern for each guarded use-package:
```elisp
(use-package PACKAGE-NAME
  :if (not (eq window-system 'ios))
  :ensure t
  :demand t  ;; only if originally had :demand t
  ...)
```

### 2. Rewrote `init/init-completion.el` (97 lines)
Every use-package form was duplicated. Rewrote entire file with each form appearing exactly once with the `:if` guard added:
- vertico, marginalia, orderless, consult, embark, embark-consult, corfu — all now appear once with `:if (not (eq window-system 'ios))` guard

### 3. Verified `init/init-bootstrap.el` (237 lines)
No duplicate lines found. Key changes verified present:
- Line 97: `(unless (eq window-system 'ios)` wrapping MELPA archive
- Line 109-110: `(unless (or (package-installed-p 'use-package) (eq window-system 'ios))`
- Line 127: `(setq use-package-always-ensure (not (eq window-system 'ios)))`
- Line 146: `:if (not (eq window-system 'ios))` on elog
- Line 175: `(unless (eq window-system 'ios)` wrapping transient hook
- Line 193: `(unless (eq window-system 'ios)` wrapping incremental loading

## Verification
- `swift build --target Hyalo` passes
- All three files load successfully in Emacs batch mode
- No syntax errors (balanced parens)
- No duplicate lines in any file
- iOS guards correctly applied to all third-party packages

## Files Modified
- `init/init-appearance.el` — complete rewrite (306 lines, no duplicates)
- `init/init-completion.el` — complete rewrite (97 lines, no duplicates)
- `init/init-bootstrap.el` — verified correct (no changes needed)

