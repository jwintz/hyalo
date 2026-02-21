---
title: CommandPalette
description: Cmd+P command palette and Cmd+O open-quickly panel
navigation:
  icon: i-lucide-command
order: 9
tags:
  - swift
  - module
  - editor
---

## Overview

The CommandPalette module provides two floating `NSPanel`-based overlays that appear on top of the main window: the command palette (`⌘P`) for fuzzy-searching and executing Emacs commands, and the open-quickly panel (`⌘O`) for fast file opening. Both share a common `SearchPanel` base.

## Key Types

### `CommandPaletteView`
_kind: struct (`View`)_

The command palette UI. A search field at the top with a filtered list of Emacs commands below. Commands are loaded from Emacs via the palette channel and filtered client-side using `FuzzyMatcher`.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `viewModel` | `CommandPaletteViewModel` | Observed palette state |

```swift
struct CommandPaletteView: View {
    @Bindable var viewModel: CommandPaletteViewModel

    var body: some View {
        SearchPanel {
            TextField("Command…", text: $viewModel.query)
                .textFieldStyle(.plain)
                .font(.title3)
            Divider()
            List(viewModel.filteredCommands, id: \.name,
                 selection: $viewModel.selectedCommand) { cmd in
                CommandRowView(command: cmd)
            }
            .listStyle(.plain)
        }
        .onSubmit { executeSelected() }
        .onKeyPress(.escape) { dismiss(); return .handled }
    }
}
```

---

### `CommandPaletteViewModel`
_kind: class (`@Observable`)_

Manages the command list, query string, filtered results, and keyboard selection state.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `query` | `String` | Current search text |
| `allCommands` | `[EmacsCommand]` | Full command list from Emacs |
| `filteredCommands` | `[EmacsCommand]` | `allCommands` filtered via `FuzzyMatcher` |
| `selectedCommand` | `EmacsCommand?` | Keyboard-highlighted item |

**Key Methods**
- `executeSelected()` — Sends `(execute-extended-command nil selectedCommand.name)` to Emacs via channel; dismisses panel.
- `loadCommands()` — Requests the full command list from Emacs; called once on first show.

---

### `CommandPaletteManager`
_kind: class (singleton)_

Owns the `NSPanel` window and toggles its visibility. Registers the `⌘P` key handler via `NSEvent.addLocalMonitorForEvents`.

**Key Methods**
- `show()` — Creates or reveals the panel, focuses the search field, resets the query.
- `hide()` — Hides the panel, returns focus to the Emacs window.

---

### `FuzzyMatcher`
_kind: struct_

Fuzzy string matching algorithm. Scores candidate strings against a query, favouring contiguous matches and acronym matches. Returns results sorted by descending score.

**Key Methods**
- `score(_:against:)` — Returns a `Double` score for a candidate/query pair. Returns `nil` if the candidate does not match.
- `filter(_:query:)` — Filters and sorts an array of strings by fuzzy score.

```swift
struct FuzzyMatcher {
    static func score(_ candidate: String, against query: String) -> Double? {
        guard !query.isEmpty else { return 1.0 }
        // Smith-Waterman inspired gap-penalising match
        // ...
    }

    static func filter<T>(_ items: [T], query: String,
                           keyPath: KeyPath<T, String>) -> [T] {
        items
            .compactMap { item -> (T, Double)? in
                guard let s = score(item[keyPath: keyPath], against: query)
                else { return nil }
                return (item, s)
            }
            .sorted { $0.1 > $1.1 }
            .map(\.0)
    }
}
```

---

### `OpenQuicklyView`
_kind: struct (`View`)_

File-centric quick-open panel. Search field filters recent files, project files, and workspace symbols. Selecting an item sends a channel message to open the file in Emacs.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `viewModel` | `OpenQuicklyViewModel` | Observed state |

---

### `OpenQuicklyViewModel`
_kind: class (`@Observable`)_

Holds the file candidate list (recent files + project files) and applies fuzzy filtering.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `query` | `String` | Search text |
| `candidates` | `[FileCandiate]` | All openable files |
| `filtered` | `[FileCandidate]` | Fuzzy-filtered results |

---

### `SearchPanel`
_kind: struct (`View`)_

Shared panel container used by both `CommandPaletteView` and `OpenQuicklyView`. Provides the `NSPanel` configuration: transparent background, rounded corners, shadow, and Liquid Glass material backdrop.

```swift
struct SearchPanel<Content: View>: View {
    @ViewBuilder let content: () -> Content

    var body: some View {
        VStack(spacing: 0, content: content)
            .frame(width: 600)
            .glassEffect(in: .rect(cornerRadius: 12))
            .shadow(radius: 24, y: 8)
    }
}
```

## Design Notes

- Both panels use `NSPanel` (not `NSWindow`) with `becomesKeyOnlyIfNeeded = false` so they capture keyboard events while the Emacs window remains the main window visually.
- `FuzzyMatcher` runs synchronously on the main thread for lists up to ~5 000 items. Larger lists (e.g., full project file trees) should be filtered on a background actor.
- The command list is requested from Emacs once per session; it is cached in `CommandPaletteViewModel.allCommands`. A channel message triggers a refresh when packages are loaded or unloaded.
- `SearchPanel` uses `.glassEffect(in:)` (macOS 26 API) for the frosted backdrop — no `NSVisualEffectView` fallback is needed since the deployment target is macOS 26.
