---
title: UtilityArea
description: Bottom panel with terminal and diagnostics tabs
navigation:
  icon: i-lucide-terminal
order: 7
tags:
  - swift
  - module
  - editor
---

## Overview

The UtilityArea module provides a collapsible bottom panel below the editor. It contains two tabs: a full SwiftTerm terminal (the primary interactive shell) and a diagnostics list fed by eglot, flymake, or flycheck via channel pushes.

## Key Types

### `UtilityAreaView`
_kind: struct (`View`)_

Container for the bottom panel. Uses `HyaloPanelView` with tab bar positioned at the top of the panel. Height is user-resizable via a drag handle.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `viewModel` | `UtilityAreaViewModel` | Injected via `@Environment` |

```swift
struct UtilityAreaView: View {
    @Environment(UtilityAreaViewModel.self) private var viewModel

    var body: some View {
        HyaloPanelView(tabs: UtilityAreaTab.allCases,
                       selected: $viewModel.activeTab,
                       tabBarPosition: .top) { tab in
            switch tab {
            case .terminal:    UtilityAreaTerminalView()
            case .diagnostics: DiagnosticsView()
            }
        }
        .frame(minHeight: 120, maxHeight: .infinity)
    }
}
```

---

### `UtilityAreaViewModel`
_kind: class (`@Observable`)_

Holds the active tab selection and the collapsed/expanded state of the utility area.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `activeTab` | `UtilityAreaTab` | Currently selected tab |
| `isCollapsed` | `Bool` | Whether the panel is hidden |
| `height` | `CGFloat` | Current panel height |

---

### `UtilityAreaTab`
_kind: enum (`CaseIterable`, `Identifiable`)_

| Case | SF Symbol |
|------|-----------|
| `.terminal` | `terminal` |
| `.diagnostics` | `exclamationmark.triangle` |

---

### `UtilityAreaTerminalView`
_kind: struct (`View`, `NSViewRepresentable`)_

Embeds a SwiftTerm `LocalProcessTerminalView` as the primary project terminal. Launches the user's default shell in the project root directory.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `process` | `LocalProcess` | SwiftTerm shell process |
| `workingDirectory` | `URL` | Project root; set by `HyaloManager` |

```swift
struct UtilityAreaTerminalView: NSViewRepresentable {
    let workingDirectory: URL

    func makeNSView(context: Context) -> LocalProcessTerminalView {
        let view = LocalProcessTerminalView(frame: .zero)
        view.startProcess(executable: userShell(),
                          arguments: [],
                          environment: nil,
                          execName: nil)
        return view
    }

    func updateNSView(_ nsView: LocalProcessTerminalView, context: Context) {}
}
```

---

### `DiagnosticsView`
_kind: struct (`View`)_

Lists diagnostic items (errors and warnings) from the active LSP session or Emacs checker. Each row shows severity icon, message, file name, and line number. Clicking a row sends a channel message to jump to the location in Emacs.

```swift
struct DiagnosticsView: View {
    @Environment(DiagnosticsViewModel.self) private var diagnostics

    var body: some View {
        List(diagnostics.items) { item in
            DiagnosticRowView(item: item)
                .onTapGesture { jumpToDiagnostic(item) }
        }
        .overlay {
            if diagnostics.items.isEmpty {
                ContentUnavailableView("No issues", systemImage: "checkmark.circle")
            }
        }
    }
}
```

---

### `DiagnosticsViewModel`
_kind: class (`@Observable`)_

Holds the current list of diagnostic items, updated via channel push from `hyalo-diagnostics.el`.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `items` | `[DiagnosticItem]` | Current diagnostics |

**`DiagnosticItem`**
| Field | Type | Description |
|-------|------|-------------|
| `id` | `UUID` | Stable identity |
| `severity` | `DiagnosticSeverity` | `.error`, `.warning`, `.info`, `.hint` |
| `message` | `String` | Diagnostic message |
| `file` | `String` | Source file name |
| `line` | `Int` | Line number |
| `column` | `Int` | Column number |

## Design Notes

- The terminal process is started once when the view first appears; it is not restarted on tab switches. The SwiftTerm view is kept alive in memory while the utility area is open.
- Diagnostics are pushed from Emacs; there is no polling of eglot state from Swift.
- The utility area collapse/expand is animated with a spring animation on the `height` binding.
- `jumpToDiagnostic(_:)` calls `wakeEmacs()` after sending the channel message to ensure Emacs processes the jump immediately.

## See Also

- [[lisp/hyalo-diagnostics|hyalo-diagnostics.el]] — pushes eglot/flymake diagnostic data to the Diagnostics tab
- [[lisp/hyalo-compile|hyalo-compile.el]] — pushes native compilation progress shown as a toolbar activity
