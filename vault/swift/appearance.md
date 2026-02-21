---
title: Appearance
description: Liquid Glass effects, vibrancy materials, color theme mapping, and decorative patterns
navigation:
  icon: i-lucide-sparkles
order: 10
tags:
  - swift
  - module
  - appearance
---

## Overview

The Appearance module implements Hyalo's visual language: macOS 26 Liquid Glass materials, `NSVisualEffectView`-backed vibrancy wrappers, Emacs theme colour mapping, and decorative panel elements. It provides the building blocks used by all other modules to achieve a consistent, system-native aesthetic.

## Key Types

### `GlassEffectView`
_kind: struct (`View`)_

The primary Liquid Glass surface used throughout the UI. Wraps SwiftUI content with the macOS 26 `.glassEffect()` modifier and exposes tinting and container morphing options.

**Properties**
| Property | Type | Description |
|----------|------|-------------|
| `tint` | `Color` | Optional tint colour overlaid on the glass |
| `cornerRadius` | `CGFloat` | Corner radius of the glass shape |
| `morphTarget` | `AnyView?` | Optional view to morph the glass container toward |

```swift
@available(macOS 26.0, *)
struct GlassEffectView<Content: View>: View {
    var tint: Color = .clear
    var cornerRadius: CGFloat = 10
    @ViewBuilder let content: () -> Content

    var body: some View {
        content()
            .background(tint.opacity(0.08))
            .glassEffect(in: .rect(cornerRadius: cornerRadius))
    }
}
```

**Usage — environment pill in toolbar:**
```swift
EnvironmentPillView(workspace: workspace)
    .clipShape(Capsule())
```

**Usage — command palette backdrop:**
```swift
SearchPanel { ... }
    .glassEffect(in: .rect(cornerRadius: 12))
```

---

### `VibrancyViews`
_kind: file (multiple `NSViewRepresentable` structs)_

Provides SwiftUI-composable wrappers around `NSVisualEffectView` for cases where vibrancy is needed without the full Liquid Glass treatment (e.g., panel backgrounds that fall back gracefully).

**Exported types:**

| Type | Material | Blending mode |
|------|----------|---------------|
| `SidebarVibrancyView` | `.sidebar` | `.behindWindow` |
| `HUDVibrancyView` | `.hudWindow` | `.behindWindow` |
| `SheetVibrancyView` | `.sheet` | `.withinWindow` |

```swift
struct SidebarVibrancyView: NSViewRepresentable {
    func makeNSView(context: Context) -> NSVisualEffectView {
        let view = NSVisualEffectView()
        view.material = .sidebar
        view.blendingMode = .behindWindow
        view.state = .active
        return view
    }

    func updateNSView(_ nsView: NSVisualEffectView, context: Context) {}
}
```

---

### `HyaloColorTheme`
_kind: struct_

Maps Emacs face colours — received as hex strings via the appearance channel — to Swift `Color` values. Used by `InspectorAppearanceView` and `MainContentView` to keep the Swift colour palette in sync with the active Emacs theme.

**Key Methods**
- `apply(_:)` — Accepts a `ThemePayload` from the channel and updates `workspace.backgroundColor` and associated accent colours.
- `color(fromHex:)` — Converts a `#RRGGBB` hex string to `Color`.

```swift
struct HyaloColorTheme {
    static func color(fromHex hex: String) -> Color {
        var sanitised = hex.trimmingCharacters(in: .init(charactersIn: "#"))
        guard sanitised.count == 6,
              let rgb = UInt64(sanitised, radix: 16) else { return .clear }
        return Color(
            red:   Double((rgb >> 16) & 0xFF) / 255,
            green: Double((rgb >> 8)  & 0xFF) / 255,
            blue:  Double( rgb        & 0xFF) / 255
        )
    }
}
```

**`ThemePayload`** (channel payload)
| Field | Type | Description |
|-------|------|-------------|
| `background` | `String` | `default` face background hex |
| `foreground` | `String` | `default` face foreground hex |
| `accent` | `String` | Primary accent colour hex |

---

### `FooterPattern`
_kind: struct (`View`)_

A decorative, low-opacity pattern rendered at the bottom of panel views. Uses Canvas-drawn geometry for a subtle texture that enhances depth without distracting from content.

```swift
struct FooterPattern: View {
    var body: some View {
        Canvas { context, size in
            // Dot-grid pattern, opacity 0.04
            let spacing: CGFloat = 8
            for x in stride(from: 0, to: size.width, by: spacing) {
                for y in stride(from: 0, to: size.height, by: spacing) {
                    context.fill(
                        Circle().path(in: CGRect(x: x, y: y, width: 1.5, height: 1.5)),
                        with: .color(.primary.opacity(0.04))
                    )
                }
            }
        }
        .allowsHitTesting(false)
    }
}
```

## Design Notes

- `.glassEffect()` is a macOS 26-exclusive API. No conditional compilation or fallback is needed — the entire module is `@available(macOS 26.0, *)`.
- Tinting via `GlassEffectView` uses a very low opacity (0.08) to avoid overpowering the glass material. The editor area tint (from `workspace.backgroundAlpha`) is applied separately in `MainContentView` and is intentionally stronger.
- `NSVisualEffectView` wrappers in `VibrancyViews` are provided for inspector and navigator backgrounds where the Liquid Glass `.glassEffect` container would be visually inconsistent with the panel chrome.
- `HyaloColorTheme` only maps background and accent colours. Full syntax highlighting colours remain managed by Emacs themes — Swift does not attempt to replicate the full face table.

## See Also

- [[lisp/hyalo-appearance|hyalo-appearance.el]] — pushes theme colors to this module via the appearance channel
- [[init/appearance|init-appearance.el]] — configures Emacs themes that drive the color sync
- [[features/appearance|Appearance feature overview]]
