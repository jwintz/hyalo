---
title: Toolbar
description: NSToolbar with environment pill, branch picker, package manager, and keycast
navigation:
  icon: i-lucide-panel-top
order: 8
tags:
  - swift
  - module
  - toolbar
---

## Overview

The Toolbar module defines all toolbar items displayed in the Hyalo window. Items are declared as SwiftUI views via `.toolbar {}` on the `NavigationSplitView` in `HyaloNavigationLayout` — not as `NSToolbarItem` subclasses. A bare `NSToolbar` stub is installed on the `NSWindow` for Emacs C compatibility; all visual content is SwiftUI-managed.

**Toolbar layout (left → right):**
- `BranchPickerView` — `.navigation` placement (leading, next to traffic lights)
- `EnvironmentPillView` — `.principal` placement (centered)
- `ToolbarSpacer(.flexible)` — pushes trailing items right
- `KeycastView` — trailing, in a `ControlGroup` glass capsule
- `PackageManagerView` — trailing, in a `ControlGroup` glass capsule
- Inspector toggle button — trailing, in a `ControlGroup` glass capsule

## Key Types

### `EnvironmentPillView`
_kind: struct (`View`)_

The centered toolbar pill. A three-segment horizontal layout inside a `Capsule` clip:

| Segment | View | Content |
|---------|------|---------|
| 1 | `UserHostDropDownView` | `person.crop.circle` icon + `user@hostname` |
| 2 | `EnvironmentDropDownView` | toolchain icon + environment summary |
| 3 | `BuildStatusView` | activity label + circular progress ring |

State for segments 1 and 2 is pushed from Emacs via `hyalo-environment.el`. State for segment 3 is managed by `ActivityManager`.

```swift
@available(macOS 26.0, *)
struct EnvironmentPillView: View {
    @Bindable var workspace: HyaloWorkspaceState
    var model: EnvironmentBreadcrumbModel = EnvironmentBreadcrumbModel.shared

    var body: some View {
        BreadcrumbContent(workspace: workspace, model: model)
    }
}
```

---

### `EnvironmentBreadcrumbModel`
_kind: class (`@Observable`, singleton)_

Observable model for the environment pill. Updated from Emacs via `hyalo-update-user-host` and `hyalo-update-environments`.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `userHost` | `UserHostInfo?` | Current `username@hostname` |
| `environments` | `[DevEnvironment]` | Detected toolchains in priority order |
| `primaryEnvironment` | `DevEnvironment?` | First active env, or first env |
| `activeEnvironments` | `[DevEnvironment]` | Environments where `isActive == true` |
| `inactiveEnvironments` | `[DevEnvironment]` | Remaining environments |
| `onEnvironmentSwitch` | `((String) -> Void)?` | Callback → Emacs `hyalo-environment--switch` |
| `onOpenTerminal` | `(() -> Void)?` | Callback → Emacs `hyalo-environment--open-terminal` |
| `onCopySSHCommand` | `(() -> Void)?` | Callback → Emacs `hyalo-environment--copy-ssh-command` |

**`UserHostInfo`** (Codable)
| Field | Type | Description |
|-------|------|-------------|
| `username` | `String` | Login name |
| `hostname` | `String` | Machine name |
| `displayName` | `String` | `username@hostname` |

**`DevEnvironment`** (Codable)
| Field | Type | Description |
|-------|------|-------------|
| `type` | `String` | `"pixi"`, `"swift"`, `"npm"`, etc. |
| `name` | `String` | Human-readable name or version |
| `icon` | `String` | SF Symbol name (overridden by `displayIcon`) |
| `isActive` | `Bool?` | Whether the env is currently active |
| `path` | `String?` | Project-relative path of the trigger file |
| `displayIcon` | `String` | Computed SF Symbol, mapped by `type` |

---

### `UserHostDropDownView`
_kind: struct (`View`)_

Pill segment 1. Shows `person.crop.circle` icon and `user@hostname`. On hover a chevron-down appears. Tapping opens an `.instantPopover` with SSH actions: "SSH to this host" and "Copy SSH command". Calls `model.onSSHHost` and `model.onCopySSHCommand` respectively.

---

### `EnvironmentDropDownView`
_kind: struct (`View`)_

Pill segment 2. Shows the primary environment's SF Symbol icon and a summary string:
- 0 envs → "No env"
- 1 env → `env.name`
- N envs → `primary.name + (N-1)`

Tapping opens an `.instantPopover` listing active environments (with a checkmark) and inactive environments. Selecting an inactive env calls `model.onEnvironmentSwitch`. An "Open Terminal Here" action calls `model.onOpenTerminal`.

---

### `BuildStatusView`
_kind: struct (`View`, private)_

Pill segment 3. Displays the most recent active `ActivityItem` from `ActivityManager.shared`. Shows a text label and a `ActivityCircularProgressView` ring (determinate or indeterminate). Tapping opens `BuildStatusDetailView` — a popover listing all activities with log lines and a Reload button for finished module builds.

---

### `ActivityManager`
_kind: class (`@Observable`, singleton)_

Manages the set of in-progress and completed activities. No polling — all updates are push-based from Emacs channels or FSEvents watchers.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `activities` | `[ActivityItem]` | All activities, most recent first |
| `activeCount` | `Int` | Number of in-progress activities |
| `hasActiveWork` | `Bool` | True if any activity is running |
| `onModuleReload` | `(() -> Void)?` | Reload callback for module build |

**`ActivityItem`**
| Field | Type | Description |
|-------|------|-------------|
| `id` | `String` | Stable identifier |
| `kind` | `ActivityKind` | `.nativeCompilation`, `.moduleCompilation`, `.packageInstallation` |
| `title` | `String` | Primary label shown in the pill |
| `message` | `String` | Secondary status message |
| `progress` | `Double?` | 0–1 for determinate, `nil` for indeterminate |
| `isActive` | `Bool` | In-progress vs. completed |
| `logLines` | `[String]` | Capped at 200 lines |

**Convenience methods**

| Method | Purpose |
|--------|---------|
| `startNativeCompilation(total:)` | Begin native compilation activity |
| `updateNativeCompilation(done:total:)` | Update progress |
| `finishNativeCompilation(compiled:)` | Mark done, auto-remove after 8 s |
| `startModuleBuild()` | Begin Swift module build |
| `updateModuleBuild(message:)` | Update build status |
| `finishModuleBuild(success:dylibChanged:)` | Mark done; keeps entry if reload available |
| `startPackageInstallation(name:)` | Begin package install |
| `finishPackageInstallation(message:)` | Mark done, auto-remove after 5 s |

---

### `ActivityCircularProgressView`
_kind: struct (`View`)_

A 16×16 circular progress ring. Determinate: a trimmed `Circle` arc animated to `progress`. Indeterminate: a spinning half-arc. Shows a count badge in the center when multiple tasks are active.

---

### `BranchPickerView`
_kind: struct (`View`)_

Leading toolbar button showing the current git branch name. Tapping opens a popover listing local branches; selecting one sends a channel message to switch branches.

---

### `PackageManagerView`
_kind: struct (`View`)_

Trailing toolbar button with the `shippingbox` SF Symbol. Shows a badge with the count of upgradable packages. Popover lists installed packages with Refresh and Upgrade actions.

---

### `KeycastView`
_kind: struct (`View`)_

Trailing toolbar item. Displays the last key sequence and bound command name. Fades out after a configurable timeout. Useful for screen recordings. Visibility controlled by `ToolbarManager`.

---

### `ToolbarManager`
_kind: class (singleton)_

Manages toolbar item visibility state. Persists preferences to `UserDefaults`.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `showKeycast` | `Bool` | Keycast item visibility |
| `showBranchPicker` | `Bool` | Branch picker visibility |

## Design Notes

- The `NSToolbar` stub installed by `HyaloWindowController` carries zero items. All visual toolbar content is declared in `.toolbar {}` on `HyaloNavigationLayout`. The stub exists solely to satisfy Emacs C code that calls `[window toolbar]`.
- `EnvironmentPillView` uses a `Capsule` clip — not `.glassEffect()` directly — so it can host three independently interactive segments inside one shape.
- `.principal` placement centres the pill in the toolbar. `ToolbarSpacer(.flexible)` pushes keycast, packages, and inspector to the trailing edge.
- `ControlGroup` wraps the trailing items to give them Liquid Glass capsule backgrounds individually sized to their content.
- `InstantPopoverModifier` (in Shared) is used by `UserHostDropDownView` and `EnvironmentDropDownView` for zero-animation popovers. Standard SwiftUI `.popover` has a 200 ms fade that makes dropdowns feel sluggish.

## See Also

- [[lisp/hyalo-compile|hyalo-compile.el]] — drives `ActivityManager` for the build status segment
- [[lisp/hyalo-environment|hyalo-environment.el]] — pushes user/host and environment data to this module
- [[lisp/hyalo-menu|hyalo-menu.el]] — complements toolbar items with macOS Application menu entries
- [[swift/shared|Shared]] — `InstantPopoverModifier` used by dropdown segments
