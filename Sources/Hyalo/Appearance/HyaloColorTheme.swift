// HyaloColorTheme.swift - Color theme derived from Emacs theme faces
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Provides semantic color roles for SwiftUI views, with light and dark
// variants inferred from the current Emacs theme. The Elisp side extracts
// colors from theme faces and pushes them via `hyalo-set-color-theme`.
//
// Views access colors via `workspace.colorTheme.accent` etc.

import AppKit
import SwiftUI

// MARK: - Color Theme Variant

/// A single-variant color set (either light or dark).
struct HyaloColorVariant: Equatable {
    // Core surfaces
    var background: String    // default face background
    var backgroundDim: String // bg-dim (subtle alternate)
    var foreground: String    // default face foreground
    var foregroundDim: String // secondary text (shadow/comments)

    // Accent
    var accent: String        // keyword face — primary accent
    var accentSecondary: String // type face — secondary accent

    // Semantic
    var error: String         // error face
    var warning: String       // warning face
    var success: String       // success face
    var link: String          // link face

    // Syntax (for terminal/code display)
    var string: String        // string face
    var comment: String       // comment face
    var constant: String      // constant face

    // Chrome
    var border: String        // window divider / separator
    var selection: String     // hl-line / selection background
}

// MARK: - Color Theme

/// Observable color theme with light and dark variants.
/// Both variants are always populated — the active variant is selected
/// based on `NSAppearance`.
@available(macOS 26.0, *)
@MainActor
@Observable
final class HyaloColorTheme {

    // MARK: - Variants

    var light: HyaloColorVariant = HyaloColorVariant.defaultLight
    var dark: HyaloColorVariant = HyaloColorVariant.defaultDark

    // MARK: - Active Variant

    /// Returns the variant matching the current effective appearance.
    var active: HyaloColorVariant {
        let appearance = NSApp.effectiveAppearance
        let isDark = appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        return isDark ? dark : light
    }

    // MARK: - SwiftUI Color Accessors

    var accent: Color { Color(hex: active.accent) ?? .accentColor }
    var accentSecondary: Color { Color(hex: active.accentSecondary) ?? .secondary }
    var background: Color { Color(hex: active.background) ?? Color(.windowBackgroundColor) }
    var backgroundDim: Color { Color(hex: active.backgroundDim) ?? Color(.controlBackgroundColor) }
    var foreground: Color { Color(hex: active.foreground) ?? .primary }
    var foregroundDim: Color { Color(hex: active.foregroundDim) ?? .secondary }
    var error: Color { Color(hex: active.error) ?? .red }
    var warning: Color { Color(hex: active.warning) ?? .orange }
    var success: Color { Color(hex: active.success) ?? .green }
    var link: Color { Color(hex: active.link) ?? .blue }
    var string: Color { Color(hex: active.string) ?? .primary }
    var comment: Color { Color(hex: active.comment) ?? .secondary }
    var constant: Color { Color(hex: active.constant) ?? .primary }
    var border: Color { Color(hex: active.border) ?? Color(.separatorColor) }
    var selection: Color { Color(hex: active.selection) ?? Color(.selectedContentBackgroundColor) }

    // MARK: - Update from JSON

    /// Update a variant from a JSON dictionary.
    /// Keys match the property names of `HyaloColorVariant`.
    func update(variant: String, from dict: [String: String]) {
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

extension HyaloColorVariant {

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
