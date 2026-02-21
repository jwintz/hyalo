---
title: StatusBar
description: Bottom status bar displaying cursor position, mode, encoding, and branch
navigation:
  icon: i-lucide-panel-bottom
order: 6
tags:
  - swift
  - module
  - statusbar
---

## Overview

The StatusBar module renders a thin bar along the bottom of the editor content area. It reflects live Emacs state — cursor position, major mode, file encoding, project name, and current git branch — all updated via channel pushes from `hyalo-status.el`. No polling.

## Key Types

### `StatusBarView`
_kind: struct (`View`)_

The visual status bar. Laid out as a horizontal `HStack` with fixed-width segments separated by `PanelDivider` instances. Uses a system-adaptive background — it does not inherit the editor tint.

```swift
struct StatusBarView: View {
    @Environment(StatusBarViewModel.self) private var model

    var body: some View {
        HStack(spacing: 0) {
            StatusSegment(label: model.mode)
            PanelDivider(axis: .vertical)
            StatusSegment(label: "\(model.line):\(model.column)")
            PanelDivider(axis: .vertical)
            StatusSegment(label: model.encoding)
            Spacer()
            StatusSegment(label: model.projectName)
            PanelDivider(axis: .vertical)
            StatusSegment(label: model.branch, icon: "arrow.triangle.branch")
        }
        .frame(height: 24)
        .background(Color(nsColor: .windowBackgroundColor))
        .overlay(alignment: .top) { Divider() }
    }
}
```

---

### `StatusBarViewModel`
_kind: class (`@Observable`)_

Observable model holding the current status values. Updated only by `StatusBarManager` in response to channel pushes.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `line` | `Int` | Current cursor line (1-based) |
| `column` | `Int` | Current cursor column (1-based) |
| `mode` | `String` | Active major mode name |
| `encoding` | `String` | File encoding (e.g., `"UTF-8"`) |
| `projectName` | `String` | Project root name (from `project.el`) |
| `branch` | `String` | Current git branch |

```swift
@Observable
final class StatusBarViewModel {
    var line: Int = 1
    var column: Int = 1
    var mode: String = ""
    var encoding: String = "UTF-8"
    var projectName: String = ""
    var branch: String = ""
}
```

---

### `StatusBarManager`
_kind: class (singleton)_

Receives structured status payloads from `hyalo-status.el` via the status channel and applies them to `StatusBarViewModel`.

**Key Methods**
- `handleStatusUpdate(_:)` — Decodes a `StatusPayload` from the channel and writes fields to the view model on the main actor.

```swift
// StatusPayload — mirrors the Emacs-side alist
struct StatusPayload: Decodable {
    let line: Int
    let column: Int
    let mode: String
    let encoding: String
    let project: String
    let branch: String
}
```

## Design Notes

- Status bar background is always system-adaptive. It must not use `workspace.backgroundColor`.
- All updates arrive via the `hyalo-status.el` channel — the status bar never reads Emacs state directly.
- The bar height is fixed at 24 pt. It sits below the `NavigationSplitView` content, outside the split view's managed area.
- `PanelDivider` from the Shared module provides the vertical separators between segments.

## See Also

- [[lisp/hyalo-status|hyalo-status.el]] — pushes all status bar data to this module
