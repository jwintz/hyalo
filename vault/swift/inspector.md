---
title: Inspector
description: Right sidebar — file metadata, git history, appearance settings, and terminal
navigation:
  icon: i-lucide-panel-right-open
order: 5
tags:
  - swift
  - module
  - inspector
---

## Overview

The Inspector module provides the right sidebar of the workspace. Like the Navigator, it uses `HyaloPanelView` for its chrome but applies a system-adaptive background — never the `workspace.backgroundColor` tint. Its four tabs expose contextual information about the active file and provide appearance customisation controls.

## Key Types

### `InspectorAreaView`
_kind: struct (`View`)_

Top-level container for the right sidebar. Uses system-adaptive background (`NSColor.windowBackgroundColor`) per the AGENTS.md rule that inspector and navigator panels must not inherit the editor tint.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `manager` | `InspectorManager` | Injected via `@Environment` |

```swift
struct InspectorAreaView: View {
    @Environment(InspectorManager.self) private var manager

    var body: some View {
        HyaloPanelView(tabs: InspectorTab.allCases,
                       selected: $manager.activeTab) { tab in
            switch tab {
            case .file:       FileInspectorView()
            case .history:    HistoryInspectorView()
            case .appearance: InspectorAppearanceView()
            case .terminal:   InspectorTerminalView()
            }
        }
        // System-adaptive — do NOT apply workspace.backgroundColor here
        .background(Color(nsColor: .windowBackgroundColor))
    }
}
```

---

### `InspectorTab`
_kind: enum (`CaseIterable`, `Identifiable`)_

Defines the four inspector tabs.

| Case | SF Symbol |
|------|-----------|
| `.file` | `doc.text` |
| `.history` | `clock.arrow.circlepath` |
| `.appearance` | `paintbrush` |
| `.terminal` | `terminal` |

---

### `FileInspectorView`
_kind: struct (`View`)_

Displays metadata for the currently active Emacs buffer. Updated via channel push when the active buffer changes.

**Displayed Fields**
| Field | Description |
|-------|-------------|
| Name | Buffer / file name |
| Type | UTType description (e.g., "Swift Source") |
| Size | Byte size of file on disk |
| Path | Full absolute path |
| Encoding | Character encoding (e.g., UTF-8) |
| Line endings | Unix / Windows / Mac |

Uses `InspectorSection` and `InspectorField` from the Shared module for consistent layout.

---

### `HistoryInspectorView`
_kind: struct (`View`)_

Shows the git log for the currently open file. Each entry displays commit hash (abbreviated), author, relative date, and subject line. Tapping an entry sends a channel message to open the commit in a diff buffer.

```swift
struct HistoryInspectorView: View {
    @Environment(InspectorManager.self) private var manager

    var body: some View {
        List(manager.fileHistory) { commit in
            CommitRowView(commit: commit)
                .onTapGesture { openCommit(commit) }
        }
        .listStyle(.inset)
    }
}
```

---

### `InspectorAppearanceView`
_kind: struct (`View`)_

Provides live appearance customisation. All changes are persisted to `UserDefaults` and pushed to Emacs via the appearance channel so the Emacs theme reacts in real time.

**Controls**
| Control | Binding | Description |
|---------|---------|-------------|
| Theme picker | `workspace.backgroundColor` | Color well for editor tint |
| Opacity slider | `workspace.backgroundAlpha` | 0–1 tint opacity |
| Material selector | `selectedMaterial` | `NSVisualEffectView.Material` variant |

```swift
struct InspectorAppearanceView: View {
    @Environment(\.workspaceState) private var workspace
    @AppStorage("hyalo.vibrancyMaterial") private var materialRaw = 0

    var body: some View {
        Form {
            ColorPicker("Editor tint", selection: $workspace.backgroundColor)
            Slider(value: $workspace.backgroundAlpha, in: 0...1) {
                Text("Opacity")
            }
            Picker("Material", selection: $materialRaw) {
                Text("Sidebar").tag(0)
                Text("HUD").tag(1)
                Text("Sheet").tag(2)
            }
        }
        .onChange(of: workspace.backgroundColor) { pushAppearance() }
        .onChange(of: workspace.backgroundAlpha)  { pushAppearance() }
    }
}
```

---

### `InspectorTerminalView`
_kind: struct (`View`, `NSViewRepresentable`)_

Embeds a SwiftTerm `TerminalView` in the inspector panel for quick shell access without leaving the inspector column.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `shell` | `LocalProcess` | SwiftTerm local shell process |

## Design Notes

- Inspector background is always system-adaptive (`NSColor.windowBackgroundColor`). The `workspace.backgroundColor` tint must **never** be applied here — see AGENTS.md.
- `InspectorAppearanceView` is the canonical place where appearance state is mutated from the UI. No other view should write to `workspace.backgroundColor` or `workspace.backgroundAlpha`.
- The appearance channel push happens via `onChange` hooks — no polling, no timers.
- `InspectorManager` is a singleton; it owns the `fileHistory` array and the `activeTab` state. Views access it via `@Environment(InspectorManager.self)`.
