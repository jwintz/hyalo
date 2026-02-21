---
title: Editor
description: Emacs NSView embedding, tab bar, and loading proxy
navigation:
  icon: i-lucide-code-xml
order: 3
tags:
  - swift
  - module
  - editor
---

## Overview

The Editor module bridges the Emacs rendering surface into the SwiftUI layout. It wraps the existing Emacs `NSView` as a SwiftUI `NSViewRepresentable`, stacks a tab bar above it, and applies the workspace background tint overlay — making the editor area visually cohesive with the surrounding Liquid Glass chrome.

A `LoadingView` is displayed in place of the editor during initial bootstrap (when the Emacs package directory has not yet been populated).

## Key Types

### `EmacsViews`
_kind: struct (`NSViewRepresentable`)_

Wraps the Emacs-owned `NSView` so it can be composed inside SwiftUI. The coordinator holds a reference to the original content view and re-parents it on `makeNSView`.

**Key Methods**
- `makeNSView(context:)` — Re-parents the Emacs `NSView` into a container; stores the original frame.
- `updateNSView(_:context:)` — No-op by design; Emacs manages its own view updates.
- `dismantleNSView(_:coordinator:)` — Restores the original parent on teardown.

```swift
struct EmacsViews: NSViewRepresentable {
    let emacsView: NSView

    func makeNSView(context: Context) -> NSView {
        let container = NSView()
        container.addSubview(emacsView)
        emacsView.autoresizingMask = [.width, .height]
        return container
    }

    func updateNSView(_ nsView: NSView, context: Context) {}
}
```

---

### `MainContentView`
_kind: struct (`View`)_

The editor column content. Stacks `EditorTabBarView` above `EmacsViews` and applies the `workspace.backgroundColor` tint overlay via a `.overlay` modifier.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `workspace` | `HyaloWorkspaceState` | Injected via `@Environment` |
| `tabViewModel` | `EditorTabViewModel` | Tab state, injected via `@Environment` |

```swift
struct MainContentView: View {
    @Environment(\.workspaceState) private var workspace

    var body: some View {
        VStack(spacing: 0) {
            EditorTabBarView()
            EmacsViews(emacsView: emacsContentView)
                .overlay(
                    workspace.backgroundColor
                        .opacity(workspace.backgroundAlpha)
                        .allowsHitTesting(false)
                )
        }
        .background(Color(nsColor: .windowBackgroundColor))
    }
}
```

> **Rule:** `workspace.backgroundColor` tint applies only here. Navigator, Inspector, and split view chrome must not use it.

---

### `EditorTabBarView`
_kind: struct (`View`)_

Renders the horizontal tab bar above the editor. Each tab displays a file icon, name, and close button. Drives actions through `EditorTabViewModel`.

**Key Methods**
- `selectTab(_:)` — Sends a channel message to Emacs to switch the active buffer; calls `wakeEmacs()`.
- `closeTab(_:)` — Sends a channel message to kill the buffer in Emacs; calls `wakeEmacs()`.

```swift
struct EditorTabBarView: View {
    @Environment(EditorTabViewModel.self) private var tabModel

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 0) {
                ForEach(tabModel.tabs) { tab in
                    TabItemView(tab: tab,
                                isSelected: tab.id == tabModel.selectedTabID,
                                onSelect: { selectTab(tab) },
                                onClose: { closeTab(tab) })
                }
            }
        }
        .frame(height: 36)
    }
}
```

---

### `EditorTabViewModel`
_kind: class (`@Observable`)_

Holds tab state. Emacs is the single source of truth — the view model is updated only via channel pushes from `hyalo-tabs.el`, never by local mutation on click.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `tabs` | `[Tab]` | Ordered list of open tabs |
| `selectedTabID` | `Tab.ID?` | Currently active tab |

```swift
@Observable
final class EditorTabViewModel {
    var tabs: [Tab] = []
    var selectedTabID: Tab.ID?

    struct Tab: Identifiable {
        let id: String        // buffer name
        var displayName: String
        var icon: String      // SF Symbol name
        var isModified: Bool
    }
}
```

> **Important:** Do not mutate `selectedTabID` locally on tab click. Send the action to Emacs and wait for the push-back. This prevents visual flicker and keeps Emacs as the authority on buffer focus.

---

### `LoadingView`
_kind: struct (`View`)_

Placeholder shown while Emacs performs initial package installation. Displayed only when `.local/` or `.local/elpa/` directories are absent at startup.

**Key Methods**
- `isBootstrapNeeded()` — Checks for `.local/` and `.local/elpa/` directories under the Hyalo data directory.

## Design Notes

- `EmacsViews` must never resize or reposition the Emacs `NSView` independently. All frame changes come from Auto Layout / SwiftUI layout passes propagated through the container view.
- The tab bar does not render a native `TabView` — it is a custom `ScrollView` + `HStack` to match the Liquid Glass aesthetic and support close buttons.
- `wakeEmacs()` must be called after every user action that sends a message to Emacs, to ensure the Emacs run loop processes the event promptly.
