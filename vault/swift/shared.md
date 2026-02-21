---
title: Shared
description: Reusable panel containers, section headers, field rows, dividers, tab bars, and instant popovers
navigation:
  icon: i-lucide-component
order: 12
tags:
  - swift
  - module
  - core
---

## Overview

The Shared module contains generic, reusable UI components used by multiple modules. It enforces visual consistency across Navigator, Inspector, UtilityArea, and any future panels by providing a common `HyaloPanelView` container and a set of inspector layout primitives.

## Key Types

### `HyaloPanelView`
_kind: struct (`View`)_

The canonical panel container used by `NavigatorAreaView`, `InspectorAreaView`, and `UtilityAreaView`. Provides a configurable tab bar (top or bottom position) and routes content based on the selected tab.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `tabs` | `[Tab]` | Tab definitions (`CaseIterable & Identifiable`) |
| `selected` | `Binding<Tab>` | Currently selected tab |
| `tabBarPosition` | `TabBarPosition` | `.top` or `.bottom` (default: `.bottom`) |
| `content` | `(Tab) -> Content` | View builder for each tab's body |

```swift
struct HyaloPanelView<Tab, Content>: View
    where Tab: CaseIterable & Identifiable & Hashable,
          Content: View
{
    let tabs: [Tab]
    @Binding var selected: Tab
    var tabBarPosition: TabBarPosition = .bottom
    @ViewBuilder let content: (Tab) -> Content

    var body: some View {
        VStack(spacing: 0) {
            if tabBarPosition == .top {
                WorkspacePanelTabBar(tabs: tabs, selected: $selected)
                PanelDivider()
            }
            content(selected)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            if tabBarPosition == .bottom {
                PanelDivider()
                WorkspacePanelTabBar(tabs: tabs, selected: $selected)
            }
        }
    }
}
```

---

### `WorkspacePanelTabBar`
_kind: struct (`View`)_

The icon-only tab bar used inside `HyaloPanelView`. Each tab renders as an SF Symbol button; the selected tab uses a filled/highlighted variant.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `tabs` | `[Tab]` | Tabs to render |
| `selected` | `Binding<Tab>` | Active tab binding |

```swift
struct WorkspacePanelTabBar<Tab>: View
    where Tab: CaseIterable & Identifiable & Hashable
{
    let tabs: [Tab]
    @Binding var selected: Tab

    var body: some View {
        HStack(spacing: 2) {
            ForEach(tabs, id: \.id) { tab in
                Button { selected = tab } label: {
                    Image(systemName: tab.systemImage)
                        .symbolVariant(selected == tab ? .fill : .none)
                }
                .buttonStyle(.plain)
                .padding(6)
            }
        }
        .frame(height: 30)
    }
}
```

---

### `HyaloPaneTextField`
_kind: struct (`View`)_

An inline-editable text field for panel headers. Used in the Navigator for project root label editing. Shows as static text until activated; switches to a focused text field on double-click.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `text` | `Binding<String>` | Editable string |
| `onCommit` | `(String) -> Void` | Called when editing ends |
| `isEditing` | `Bool` | Internal editing state |

---

### `InspectorField`
_kind: struct (`View`)_

A label + value row for use inside inspector panels. Renders a leading label in secondary style and a trailing value in primary style.

```swift
struct InspectorField: View {
    let label: String
    let value: String

    var body: some View {
        HStack {
            Text(label)
                .foregroundStyle(.secondary)
                .frame(width: 80, alignment: .leading)
            Text(value)
                .foregroundStyle(.primary)
                .textSelection(.enabled)
            Spacer()
        }
        .font(.callout)
        .padding(.vertical, 2)
    }
}
```

---

### `InspectorSection`
_kind: struct (`View`)_

A section header for grouping related `InspectorField` rows. Renders a bold title with an optional disclosure triangle for collapsible sections.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `title` | `String` | Section heading text |
| `isCollapsible` | `Bool` | Whether the section can be collapsed |
| `isExpanded` | `Binding<Bool>?` | Expansion state when collapsible |

---

### `PanelDivider`
_kind: struct (`View`)_

A 1 pt hairline divider sized for panel chrome. Supports both horizontal and vertical orientations.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `axis` | `Axis` | `.horizontal` (default) or `.vertical` |

```swift
struct PanelDivider: View {
    var axis: Axis = .horizontal

    var body: some View {
        switch axis {
        case .horizontal:
            Rectangle()
                .frame(height: 0.5)
                .foregroundStyle(.separator)
        case .vertical:
            Rectangle()
                .frame(width: 0.5)
                .foregroundStyle(.separator)
        }
    }
}
```

---

### `InstantPopoverModifier`
_kind: struct (`ViewModifier`)_

Zero-animation `NSPopover` presented directly — bypassing SwiftUI's 200 ms fade. Ported from CodeEdit's pattern. Used by `UserHostDropDownView` and `EnvironmentDropDownView` in the toolbar.

**Behavior**
- `NSPopover.animates = false`
- `NSPopover.behavior = .semitransient` — closes when the user clicks outside
- Observes `NSWindow.didResignKeyNotification` to close when the window loses focus
- Dismisses the containing SwiftUI binding on close via `NSPopoverDelegate`

**Warning**: Views inside must dismiss by negating the `isPresented` binding. Using SwiftUI's `dismiss` environment value crashes (FB16221871).

**View extension**

```swift
extension View {
    func instantPopover<Content: View>(
        isPresented: Binding<Bool>,
        arrowEdge: Edge = .bottom,
        @ViewBuilder content: @escaping () -> Content
    ) -> some View
}
```

Content is automatically wrapped in `InstantPopoverContainer` which applies standard Tahoe-aware padding (`13 pt`), a minimum width of 215 pt, and `.subheadline` font.

---

### `DropdownItemStyleModifier`
_kind: struct (`ViewModifier`)_

Hover-aware row style used inside instant popovers. On hover: accent-color fill, white foreground. At rest: transparent. Applied via `.dropdownItemStyle()` extension on `View`.

## Design Notes

- `HyaloPanelView` is generic over the tab type — it does not import or reference any specific tab enum. This keeps Shared free of dependencies on Navigator, Inspector, or UtilityArea.
- `WorkspacePanelTabBar` requires tab types to expose a `systemImage: String` property (via a protocol or extension). This is declared in the Shared module as a `PanelTab` protocol.
- `InspectorField` uses `.textSelection(.enabled)` so users can copy values like file paths without a secondary interaction.
- `PanelDivider` renders at 0.5 pt (hairline) using `.foregroundStyle(.separator)` — this adapts automatically to dark/light mode and uses the system separator colour.
- `InstantPopoverModifier` uses `NSViewRepresentable` as the presentation anchor so the popover is positioned relative to the button, not the window origin.
