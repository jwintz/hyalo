// HyaloColorTheme.swift - Color theme derived from Emacs theme faces
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Provides semantic color roles for SwiftUI views, with light and dark
// variants inferred from the current Emacs theme. The Elisp side extracts
// colors from theme faces and pushes them via `hyalo-set-color-theme`.
//
// Views access colors via `workspace.colorTheme.accent` etc.

import SwiftUI

// MARK: - Color Theme Variant

/// A single-variant color set (either light or dark).
public struct HyaloColorVariant: Equatable {
    // Core surfaces
    public var background: String    // default face background
    public var backgroundDim: String // bg-dim (subtle alternate)
    public var foreground: String    // default face foreground
    public var foregroundDim: String // secondary text (shadow/comments)

    // Accent
    public var accent: String        // keyword face — primary accent
    public var accentSecondary: String // type face — secondary accent

    // Semantic
    public var error: String         // error face
    public var warning: String       // warning face
    public var success: String       // success face
    public var link: String          // link face

    // Syntax (for terminal/code display)
    public var string: String        // string face
    public var comment: String       // comment face
    public var constant: String      // constant face

    // Chrome
    public var border: String        // window divider / separator
    public var selection: String     // hl-line / selection background
}

// MARK: - Color Theme

/// Observable color theme with light and dark variants.
/// Both variants are always populated — the active variant is selected
/// based on `NSAppearance`.
@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
public final class HyaloColorTheme {

    // MARK: - Variants

    public var light: HyaloColorVariant = HyaloColorVariant.defaultLight
    public var dark: HyaloColorVariant = HyaloColorVariant.defaultDark

    // MARK: - Appearance Tracking

    /// Tracks the current interface style. Must be a stored property so
    /// SwiftUI's @Observable graph registers a dependency on it. Computed
    /// properties that read external state (e.g. NSApp.effectiveAppearance)
    /// are invisible to the observation system and cannot trigger re-renders.
    public var isDark: Bool = platformIsDarkMode()

    /// Call this when the system appearance changes so all dependent views
    /// re-render. Invoked by HyaloManager's DistributedNotificationCenter observer.
    public func refreshAppearance() {
        isDark = platformIsDarkMode()
    }

    // MARK: - Active Variant

    /// Returns the variant matching the current effective appearance.
    public var active: HyaloColorVariant {
        isDark ? dark : light
    }

    // MARK: - SwiftUI Color Accessors

    public var accent: Color { Color(hex: active.accent) ?? .accentColor }
    public var accentSecondary: Color { Color(hex: active.accentSecondary) ?? .secondary }
    public var background: Color { Color(hex: active.background) ?? Color(platformColor: .systemBackground) }
    public var backgroundDim: Color { Color(hex: active.backgroundDim) ?? Color(platformColor: .secondarySystemBackground) }
    public var foreground: Color { Color(hex: active.foreground) ?? .primary }
    public var foregroundDim: Color { Color(hex: active.foregroundDim) ?? .secondary }
    public var error: Color { Color(hex: active.error) ?? .red }
    public var warning: Color { Color(hex: active.warning) ?? .orange }
    public var success: Color { Color(hex: active.success) ?? .green }
    public var link: Color { Color(hex: active.link) ?? .blue }
    public var string: Color { Color(hex: active.string) ?? .primary }
    public var comment: Color { Color(hex: active.comment) ?? .secondary }
    public var constant: Color { Color(hex: active.constant) ?? .primary }
    public var border: Color { Color(hex: active.border) ?? Color(platformColor: .separator) }
    public var selection: Color { Color(hex: active.selection) ?? Color(platformColor: .selectedContentBackground) }

    // MARK: - Update from JSON

    /// Update a variant from a JSON dictionary.
    /// Keys match the property names of `HyaloColorVariant`.
    public func update(variant: String, from dict: [String: String]) {
        let v = HyaloColorVariant(
            background: dict["background"] ?? (variant == "dark" ? "#1e1e1e" : "#ffffff"),
            backgroundDim: dict["backgroundDim"] ?? (variant == "dark" ? "#27272a" : "#fafafa"),
            foreground: dict["foreground"] ?? (variant == "dark" ? "#f4f4f5" : "#18181b"),
            foregroundDim: dict["foregroundDim"] ?? (variant == "dark" ? "#71717a" : "#a1a1aa"),
            accent: dict["accent"] ?? "#A58AF9",
            accentSecondary: dict["accentSecondary"] ?? (variant == "dark" ? "#dcd3f8" : "#321685"),
            error: dict["error"] ?? (variant == "dark" ? "#f38ba8" : "#D32F2F"),
            warning: dict["warning"] ?? (variant == "dark" ? "#f9e2af" : "#F57F17"),
            success: dict["success"] ?? (variant == "dark" ? "#a6e3a1" : "#2E7D32"),
            link: dict["link"] ?? (variant == "dark" ? "#89b4fa" : "#7c3aed"),
            string: dict["string"] ?? (variant == "dark" ? "#ede8fc" : "#240e67"),
            comment: dict["comment"] ?? (variant == "dark" ? "#71717a" : "#a1a1aa"),
            constant: dict["constant"] ?? (variant == "dark" ? "#c4b5fd" : "#5E35B1"),
            border: dict["border"] ?? (variant == "dark" ? "#3f3f46" : "#d4d4d8"),
            selection: dict["selection"] ?? (variant == "dark" ? "#655594" : "#c5beda")
        )
        if variant == "dark" {
            dark = v
        } else {
            light = v
        }
    }
}

// MARK: - Default Variants

public extension HyaloColorVariant {

    /// Default dark variant — Lithos Zinc + Violet dichromatic
    static let defaultDark = HyaloColorVariant(
        background: "#1e1e1e",
        backgroundDim: "#27272a",
        foreground: "#f4f4f5",
        foregroundDim: "#71717a",
        accent: "#A58AF9",
        accentSecondary: "#dcd3f8",
        error: "#f38ba8",
        warning: "#f9e2af",
        success: "#a6e3a1",
        link: "#89b4fa",
        string: "#ede8fc",
        comment: "#71717a",
        constant: "#c4b5fd",
        border: "#3f3f46",
        selection: "#655594"
    )

    /// Default light variant — Lithos Zinc + Violet dichromatic
    static let defaultLight = HyaloColorVariant(
        background: "#ffffff",
        backgroundDim: "#fafafa",
        foreground: "#18181b",
        foregroundDim: "#a1a1aa",
        accent: "#A58AF9",
        accentSecondary: "#321685",
        error: "#D32F2F",
        warning: "#F57F17",
        success: "#2E7D32",
        link: "#7c3aed",
        string: "#240e67",
        comment: "#a1a1aa",
        constant: "#5E35B1",
        border: "#d4d4d8",
        selection: "#c5beda"
    )
}

// MARK: - Color from Hex

private extension Color {
    init?(hex: String) {
        let trimmed = hex.trimmingCharacters(in: .whitespaces)
        let str = trimmed.hasPrefix("#") ? String(trimmed.dropFirst()) : trimmed
        guard str.count == 6 else { return nil }
        var rgb: UInt64 = 0
        Scanner(string: str).scanHexInt64(&rgb)
        let r = Double((rgb >> 16) & 0xFF) / 255.0
        let g = Double((rgb >> 8) & 0xFF) / 255.0
        let b = Double(rgb & 0xFF) / 255.0
        self.init(red: r, green: g, blue: b)
    }
}
