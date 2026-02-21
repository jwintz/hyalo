---
title: Core
description: Module entry point, workspace state, environment keys, and shared utilities
navigation:
  icon: i-lucide-box
order: 1
tags:
  - swift
  - module
  - core
---

## Overview

The Core module is the foundation of the Hyalo dynamic module. It contains the EmacsSwiftModule entry point that bridges the Swift world to the Emacs Lisp world, the observable workspace state that acts as single source of truth for layout, and SwiftUI environment plumbing.

When Emacs loads `Hyalo.dylib` via `module-load`, the `Module.swift` entry point runs first — it registers all Emacs-callable functions and bootstraps `HyaloManager`.

## Key Types

### `HyaloModule`
_kind: class (EmacsSwiftModule subclass)_

Entry point of the dynamic module. Overrides `Init` to register all Emacs-callable functions via `module.defun`. Resolves private Emacs C symbols at load time using `dlsym`:

| Symbol | Purpose |
|--------|---------|
| `ns_set_fringe_alpha_override` | Suppress fringe rendering artifacts |
| `ns_force_redisplay` | Force immediate Emacs redraw |
| `ns_wake_emacs` | Wake the Emacs run loop from Swift |

```swift
@available(macOS 26.0, *)
class HyaloModule: EmacsSwiftModule {
    let isGPLCompatible = true

    func Init(_ env: Environment) throws {
        try env.defun("hyalo--setup") { ... }
        try env.defun("hyalo--teardown") { ... }
        // ...
    }
}
```

**Key Methods**
- `Init(_:)` — Called once by EmacsSwiftModule infrastructure at `module-load` time
- `resolveSymbol(_:)` — `dlsym`-based lookup for private Emacs C symbols

---

### `HyaloManager`
_kind: class (singleton)_

Orchestrates the full lifecycle of the Hyalo UI layer. Created once from `Module.swift` and held for the process lifetime.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `shared` | `HyaloManager` | Singleton accessor |
| `workspaceState` | `HyaloWorkspaceState` | Observable workspace model |
| `windowController` | `HyaloWindowController?` | The hosting window controller |

**Key Methods**
- `setup(in:)` — Creates workspace state, installs `HyaloWindowController` onto the main Emacs window, starts channel listeners
- `teardown()` — Removes Hyalo UI, restores original Emacs window state

---

### `HyaloWorkspaceState`
_kind: class (`@Observable`)_

Single source of truth for workspace layout state. Injected into the SwiftUI view hierarchy via `@Environment`.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `backgroundColor` | `Color` | Editor area tint color |
| `backgroundAlpha` | `Double` | Tint overlay opacity (0–1) |
| `sidebarWidth` | `CGFloat` | Navigator sidebar width |
| `inspectorWidth` | `CGFloat` | Inspector sidebar width |
| `activePanel` | `NavigatorTab` | Currently selected navigator tab |
| `showInspector` | `Bool` | Whether inspector sidebar is visible |

```swift
@Observable
final class HyaloWorkspaceState {
    var backgroundColor: Color = .clear
    var backgroundAlpha: Double = 0.0
    var sidebarWidth: CGFloat = 240
    var inspectorWidth: CGFloat = 260
    var activePanel: NavigatorTab = .buffers
    var showInspector: Bool = true
}
```

---

### `HyaloShared`
_kind: enum (namespace)_

Shared constants, type aliases, and utility extensions used across all modules.

---

### `EnvironmentKeys`
_kind: file (multiple `EnvironmentKey` conformances)_

Defines custom SwiftUI `EnvironmentKey` types for injecting workspace state and manager references into the view hierarchy without explicit prop-drilling.

```swift
private struct WorkspaceStateKey: EnvironmentKey {
    static let defaultValue = HyaloWorkspaceState()
}

extension EnvironmentValues {
    var workspaceState: HyaloWorkspaceState {
        get { self[WorkspaceStateKey.self] }
        set { self[WorkspaceStateKey.self] = newValue }
    }
}
```

## Design Notes

- All types are `@available(macOS 26.0, *)` — no back-deployment paths exist.
- `HyaloManager` is the only place where `NSWindow` is accessed directly; all other modules receive state through `@Environment`.
- `dlsym` lookups happen once at module load; resolved function pointers are stored as `UnsafeMutableRawPointer?` optionals and called with `typealias` casts.
- `HyaloWorkspaceState` is an `@Observable` class (not `ObservableObject`) — subscribers use `withObservationTracking` or the `.onChange` modifier.

## See Also

- [[lisp/hyalo|hyalo.el]] — calls `module-load` to bring this entry point to life
- [[lisp/hyalo-channels|hyalo-channels.el]] — opens channels that `HyaloManager` listens on
- [[init/hyalo|init-hyalo.el]] — the init module that triggers the full load sequence
