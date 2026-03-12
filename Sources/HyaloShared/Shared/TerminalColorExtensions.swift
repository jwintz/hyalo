// TerminalColorExtensions.swift - Color conversion helpers for SwiftTerm
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Provides platform-agnostic color conversion utilities for applying
// TerminalPalette colors to SwiftTerm terminal views.

import SwiftUI
import SwiftTerm
import OSLog

@available(macOS 26.0, *)
private let logger = Logger(subsystem: "hyalo", category: "TerminalColorExtensions")

import AppKit

// MARK: - AppKit (macOS) Extensions

@available(macOS 26.0, *)
public extension NSColor {
    /// Create NSColor from hex string (e.g., "#FF0000" or "FF0000")
    static func fromHex(_ hex: String) -> NSColor? {
        let trimmed = hex.trimmingCharacters(in: .whitespaces)
        let hexString = trimmed.hasPrefix("#") ? String(trimmed.dropFirst()) : trimmed
        guard hexString.count == 6 else { return nil }
        var rgb: UInt64 = 0
        Scanner(string: hexString).scanHexInt64(&rgb)
        let r = CGFloat((rgb >> 16) & 0xFF) / 255.0
        let g = CGFloat((rgb >> 8) & 0xFF) / 255.0
        let b = CGFloat(rgb & 0xFF) / 255.0
        return NSColor(red: r, green: g, blue: b, alpha: 1.0)
    }

    /// Convert to SwiftTerm.Color
    func toSwiftTermColor() -> SwiftTerm.Color? {
        var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
        let converted = self.usingColorSpace(.sRGB) ?? self
        converted.getRed(&r, green: &g, blue: &b, alpha: &a)
        return SwiftTerm.Color(
            red: UInt16(r * 65535),
            green: UInt16(g * 65535),
            blue: UInt16(b * 65535)
        )
    }
}

@available(macOS 26.0, *)
public extension TerminalView {
    /// Apply a complete color scheme from TerminalPalette
    func applyPalette(_ palette: TerminalPalette) {
        // Apply foreground
        if let fg = NSColor.fromHex(palette.foreground) {
            self.nativeForegroundColor = fg
        }

        // Apply cursor
        if let cursorColor = NSColor.fromHex(palette.cursor) {
            self.caretColor = cursorColor
        }

        // Apply ANSI colors
        guard palette.ansiColors.count == 16 else {
            logger.error("ANSI colors count is \(palette.ansiColors.count), expected 16")
            return
        }

        let swiftTermColors: [SwiftTerm.Color] = palette.ansiColors.compactMap { hex in
            guard let ns = NSColor.fromHex(hex) else { return nil }
            return ns.toSwiftTermColor()
        }

        if swiftTermColors.count == 16 {
            self.installColors(swiftTermColors)
        } else {
            logger.error("Only \(swiftTermColors.count) ANSI colors converted successfully")
        }
    }
}

// Note: SwiftUI.Color.init(hex:) is already defined in HyaloColorTheme.swift
