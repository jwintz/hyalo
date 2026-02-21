---
title: Window
description: NSWindowController hosting the NavigationSplitView root layout
navigation:
  icon: i-lucide-app-window
order: 2
tags:
  - swift
  - module
  - window
---

## Overview

The Window module owns the connection between the Emacs `NSWindow` and the SwiftUI world. `HyaloWindowController` replaces the default Emacs window content with a SwiftUI host view containing `HyaloNavigationLayout`, which defines the top-level `NavigationSplitView` structure.

The toolbar is also owned here — defined via SwiftUI's `.toolbar {}` modifier on the `NavigationSplitView`, while a minimal `NSToolbar` stub is installed on the `NSWindow` for compatibility with Emacs C code that queries toolbar metrics.

## Key Types

### `HyaloWindowController`
_kind: class (`NSWindowController` subclass)_

Hosts the entire Hyalo SwiftUI hierarchy inside the Emacs window. Created once by `HyaloManager.setup(in:)`.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `isSetUp` | `Bool` | Guards against double-initialisation |
| `hostingView` | `NSHostingView<HyaloNavigationLayout>` | SwiftUI root host |

**Key Methods**
- `decorateWindow()` — Configures the `NSWindow`: transparent titlebar, full-size content view, removes traffic-light offset. Does **not** force appearance — system handles dark/light mode.
- `installToolbarStub()` — Installs a bare `NSToolbar` so Emacs C code that measures toolbar height does not crash. Actual items are SwiftUI-managed.
- `teardown()` — Removes hosting view, restores original Emacs content view.

```swift
@available(macOS 26.0, *)
final class HyaloWindowController: NSWindowController {
    private(set) var isSetUp = false

    func setUp(on window: NSWindow, state: HyaloWorkspaceState) {
        guard !isSetUp else { return }
        decorateWindow(window)
        installToolbarStub(on: window)
        let root = HyaloNavigationLayout().environment(\.workspaceState, state)
        hostingView = NSHostingView(rootView: root)
        window.contentView = hostingView
        isSetUp = true
    }

    private func decorateWindow(_ window: NSWindow) {
        window.titlebarAppearsTransparent = true
        window.styleMask.insert(.fullSizeContentView)
        // Do NOT set window.appearance — let the system handle it
    }
}
```

---

### `HyaloNavigationLayout`
_kind: struct (`View`)_

Root SwiftUI view. Declares the three-column `NavigationSplitView` and hosts the SwiftUI toolbar definition.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `workspace` | `HyaloWorkspaceState` | Passed as `@Bindable` |
| `emacsView` | `NSView` | The raw Emacs content view to embed |
| `columnVisibility` | `NavigationSplitViewVisibility` | Sidebar show/hide state, synced with `workspace.navigatorVisible` |

**Key Methods**
- `body` — Returns a two-column `NavigationSplitView` (sidebar + detail). The inspector is hosted via `.inspector(isPresented:)` on `MainContentView`, not as a third split column.
- Toolbar items declared inline in `.toolbar {}`: `BranchPickerView` at `.navigation`, `EnvironmentPillView` at `.principal`, `KeycastView` / `PackageManagerView` / inspector toggle at trailing.

```swift
@available(macOS 26.0, *)
struct HyaloNavigationLayout: View {
    @Bindable var workspace: HyaloWorkspaceState
    let emacsView: NSView

    var body: some View {
        NavigationSplitView(columnVisibility: $columnVisibility) {
            NavigatorAreaView(workspace: workspace)
        } detail: {
            MainContentView(workspace: workspace, emacsView: emacsView, ...)
                .inspector(isPresented: inspectorVisibleBinding) {
                    InspectorAreaView(workspace: workspace)
                }
        }
        .navigationSplitViewStyle(.balanced)
        .toolbar {
            ToolbarItem(placement: .navigation) { BranchPickerView(...) }
            ToolbarItem(placement: .principal) { EnvironmentPillView(workspace: workspace) }
            ToolbarSpacer(.flexible)
            ToolbarItem { ControlGroup { KeycastView(...) } }
            ToolbarItem { ControlGroup { PackageManagerView(...) } }
            ToolbarSpacer(.fixed)
            ToolbarItem { ControlGroup { /* inspector toggle */ } }
        }
    }
}
```

## Design Notes

- `NSWindowController` is used (not `NSViewController`) because Emacs manages windows at the `NSWindow` level.
- The toolbar stub is a zero-item `NSToolbar` with `allowsUserCustomization = false`. Its sole purpose is to satisfy Emacs C queries for `[window toolbar]` — never add items to it.
- `decorateWindow()` must not call `window.appearance = ...`. Forcing appearance breaks system-level dark/light switching. This constraint is documented in `AGENTS.md`.
- Split view layer backgrounds use `NSColor.windowBackgroundColor.cgColor` (system-adaptive). The `workspace.backgroundColor` tint is applied only inside `MainContentView`, never on the split view chrome itself.

## See Also

- [[lisp/hyalo-window|hyalo-window.el]] — installs this window controller via `hyalo-window-setup`
- [[lisp/hyalo-channels|hyalo-channels.el]] — channels opened after the window is set up
- [[swift/core|Swift Core]] — `HyaloManager.setup(in:)` that creates this controller
