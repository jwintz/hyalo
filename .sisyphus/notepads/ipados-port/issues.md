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
