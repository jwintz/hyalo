---
title: Navigator
description: Left sidebar — buffers, file tree, source control, and search panels
navigation:
  icon: i-lucide-panel-left-open
order: 4
tags:
  - swift
  - module
  - navigator
---

## Overview

The Navigator module provides the left sidebar of the workspace. It is a multi-tab panel — each tab hosts a different navigator view. The four built-in tabs are:

| Tab | View | Data source |
|-----|------|-------------|
| Buffers | `BufferListView` | Channel push from `hyalo-buffers.el` |
| Files | `ProjectNavigatorView` | `mchakravarty/ProjectNavigator` + git status |
| SCM | `SourceControlNavigatorView` | libgit2 / shell |
| Search | `FindNavigatorView` | Channel push from `hyalo-search.el` |

## Key Types

### `NavigatorAreaView`
_kind: struct (`View`)_

Top-level container for the left sidebar. Wraps its content in `HyaloPanelView` to get the shared panel chrome (tab bar, divider, background). Uses `workspace.backgroundColor` for the sidebar background tint.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `workspace` | `HyaloWorkspaceState` | Injected via `@Environment` |

```swift
struct NavigatorAreaView: View {
    @Environment(\.workspaceState) private var workspace

    var body: some View {
        HyaloPanelView(tabs: NavigatorTab.allCases,
                       selected: $workspace.activePanel) { tab in
            switch tab {
            case .buffers:  BufferListView()
            case .files:    ProjectNavigatorView()
            case .scm:      SourceControlNavigatorView()
            case .search:   FindNavigatorView()
            }
        }
        .background(workspace.backgroundColor.opacity(workspace.backgroundAlpha))
    }
}
```

---

### `NavigatorTab`
_kind: enum (`CaseIterable`, `Identifiable`)_

Defines the four navigator tabs with their SF Symbol icons.

| Case | Raw Value | SF Symbol |
|------|-----------|-----------|
| `.buffers` | `"buffers"` | `rectangle.on.rectangle` |
| `.files` | `"files"` | `folder` |
| `.scm` | `"scm"` | `arrow.triangle.branch` |
| `.search` | `"search"` | `magnifyingglass` |

---

### `BufferListView` / `BufferListViewModel`
_kind: struct (`View`) / class (`@Observable`)_

Displays the list of open Emacs buffers. Updated exclusively via channel pushes — no local selection state.

**`BufferListViewModel` Properties**
| Property | Type | Description |
|----------|------|-------------|
| `buffers` | `[BufferItem]` | Current buffer list |
| `activeBufferName` | `String?` | Currently focused buffer |

**`BufferItem`**
| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Buffer name |
| `filePath` | `String?` | Associated file path, if any |
| `isModified` | `Bool` | Unsaved changes |

---

### `ProjectNavigatorView` / `ProjectNavigatorViewModel`
_kind: struct (`View`) / class (`@Observable`)_

File tree using the `mchakravarty/ProjectNavigator` package. Nodes are identified by UUID; expansion and selection state are held locally. Labels are editable in-place for rename operations.

**`ProjectNavigatorViewModel` Properties**
| Property | Type | Description |
|----------|------|-------------|
| `root` | `FileItem?` | Root directory node |
| `expandedIDs` | `Set<UUID>` | Currently expanded folder IDs |
| `selectedID` | `UUID?` | Selected node ID |

**Key Methods**
- `loadDirectory(_:)` — Builds the file tree from a root URL. Rejects home directory (`~`) and filesystem root (`/`).
- `renameItem(_:to:)` — Sends rename to Emacs via channel; does not mutate local state until confirmed.

```swift
// Rejection of unsafe roots
func loadDirectory(_ url: URL) {
    guard url != URL(filePath: NSHomeDirectory()),
          url.pathComponents.count > 1 else {
        return  // refuse home and filesystem root
    }
    // ...
}
```

---

### `HyaloProjectNavigator`
_kind: struct_

Adapter that binds the generic `FileNavigator` from `mchakravarty/ProjectNavigator` to Hyalo's view model and channel infrastructure.

---

### `HyaloFileTreeBuilder`
_kind: struct_

Constructs a `FileItem` tree from a directory URL, enriching each node with a git status badge by querying the SCM state.

**Key Methods**
- `build(from:)` — Recursive directory scan; returns root `FileItem`.
- `gitStatus(for:)` — Returns `.modified`, `.added`, `.untracked`, or `.clean` for a path.

---

### `HyaloFilePayload`
_kind: struct (`Codable`)_

Data model for a single file node transmitted over the Emacs channel.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `path` | `String` | Absolute file path |
| `name` | `String` | Display name |
| `isDirectory` | `Bool` | Whether node is a folder |
| `gitStatus` | `GitStatus` | SCM status badge |

---

### `FindNavigatorView` / `SearchViewModel`
_kind: struct (`View`) / class (`@Observable`)_

Project-wide search panel. Search queries are dispatched to Emacs via `hyalo-search.el`; results are pushed back as structured payloads.

**`SearchViewModel` Properties**
| Property | Type | Description |
|----------|------|-------------|
| `query` | `String` | Current search string |
| `results` | `[SearchResult]` | Result items |
| `isSearching` | `Bool` | Activity indicator state |

---

### `SourceControlNavigatorView` / `SourceControlViewModel`
_kind: struct (`View`) / class (`@Observable`)_

Displays the current git repository state: branch name, staged files, unstaged changes, and untracked files.

**`SourceControlViewModel` Properties**
| Property | Type | Description |
|----------|------|-------------|
| `branch` | `String` | Current branch name |
| `stagedChanges` | `[SCMItem]` | Files staged for commit |
| `unstagedChanges` | `[SCMItem]` | Modified but unstaged files |
| `untrackedFiles` | `[SCMItem]` | New untracked files |

## Design Notes

- All navigator views receive data exclusively through channel pushes — they never query Emacs directly from Swift.
- `NavigatorManager` is a singleton that owns the view model instances; views access models via `@Environment` injection, not direct singleton access.
- The file tree must reject `NSHomeDirectory()` and path-component-count ≤ 1 paths to prevent accidentally displaying enormous directories.
- Git status badges are computed asynchronously after the tree renders; nodes update incrementally as status resolves.

## See Also

- [[lisp/hyalo-navigator|hyalo-navigator.el]] — pushes buffer list and project root to this module
