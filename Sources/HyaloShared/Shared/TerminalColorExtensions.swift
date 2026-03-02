// TerminalColorExtensions.swift - Color conversion helpers for SwiftTerm
// Target: macOS 26 Tahoe / iOS 26 with Liquid Glass design
//
// Provides platform-agnostic color conversion utilities for applying
// TerminalPalette colors to SwiftTerm terminal views.

import SwiftUI
import SwiftTerm
import OSLog

@available(macOS 26.0, iOS 26.0, *)
private let logger = Logger(subsystem: "hyalo", category: "TerminalColorExtensions")

#if canImport(AppKit)
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
        logger.info("macOS TerminalView.applyPalette called")
        logger.info("   - isDark: \(palette.isDark)")
        logger.info("   - foreground: \(palette.foreground)")
        logger.info("   - background: \(palette.background)")
        logger.info("   - cursor: \(palette.cursor)")
        logger.info("   - ansiColors count: \(palette.ansiColors.count)")

        // Apply foreground
        if let fg = NSColor.fromHex(palette.foreground) {
            self.nativeForegroundColor = fg
            logger.info("Applied foreground: \(palette.foreground, privacy: .public)")
        } else {
            logger.error("Failed to convert foreground color: \(palette.foreground, privacy: .public)")
        }

        // Apply cursor
        if let cursorColor = NSColor.fromHex(palette.cursor) {
            self.caretColor = cursorColor
            logger.info("Applied cursor: \(palette.cursor, privacy: .public)")
        } else {
            logger.error("Failed to convert cursor color: \(palette.cursor, privacy: .public)")
        }

        // Apply ANSI colors
        guard palette.ansiColors.count == 16 else {
            logger.error("ANSI colors count is \(palette.ansiColors.count), expected 16")
            return
        }

        let swiftTermColors: [SwiftTerm.Color] = palette.ansiColors.compactMap { hex in
            guard let ns = NSColor.fromHex(hex) else {
                logger.error("Failed to convert ANSI color: \(hex, privacy: .public)")
                return nil
            }
            return ns.toSwiftTermColor()
        }

        if swiftTermColors.count == 16 {
            self.installColors(swiftTermColors)
            logger.info("Applied \(swiftTermColors.count) ANSI colors")
            // Log first few ANSI colors for debugging
            logger.debug("   ANSI 0 (black): \(palette.ansiColors[0], privacy: .public)")
            logger.debug("   ANSI 1 (red): \(palette.ansiColors[1], privacy: .public)")
            logger.debug("   ANSI 2 (green): \(palette.ansiColors[2], privacy: .public)")
            logger.debug("   ANSI 7 (white): \(palette.ansiColors[7], privacy: .public)")
            logger.debug("   ANSI 8 (bright black): \(palette.ansiColors[8], privacy: .public)")
            logger.debug("   ANSI 15 (bright white): \(palette.ansiColors[15], privacy: .public)")
        } else {
            logger.error("Only \(swiftTermColors.count) ANSI colors converted successfully")
        }
    }
}

#endif

#if canImport(UIKit)
import UIKit

// MARK: - UIKit (iOS) Extensions

@available(iOS 26.0, *)
public extension UIColor {
    /// Create UIColor from hex string (e.g., "#FF0000" or "FF0000")
    static func fromHex(_ hex: String) -> UIColor? {
        let trimmed = hex.trimmingCharacters(in: .whitespaces)
        let hexString = trimmed.hasPrefix("#") ? String(trimmed.dropFirst()) : trimmed
        guard hexString.count == 6 else { return nil }
        var rgb: UInt64 = 0
        Scanner(string: hexString).scanHexInt64(&rgb)
        let r = CGFloat((rgb >> 16) & 0xFF) / 255.0
        let g = CGFloat((rgb >> 8) & 0xFF) / 255.0
        let b = CGFloat(rgb & 0xFF) / 255.0
        return UIColor(red: r, green: g, blue: b, alpha: 1.0)
    }

    /// Convert to SwiftTerm.Color
    func toSwiftTermColor() -> SwiftTerm.Color? {
        var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
        guard self.getRed(&r, green: &g, blue: &b, alpha: &a) else { return nil }
        return SwiftTerm.Color(
            red: UInt16(r * 65535),
            green: UInt16(g * 65535),
            blue: UInt16(b * 65535)
        )
    }
}

@available(iOS 26.0, *)
public extension TerminalView {
    /// Apply a complete color scheme from TerminalPalette
    func applyPalette(_ palette: TerminalPalette) {
        logger.info("iOS TerminalView.applyPalette called")
        logger.info("   - isDark: \(palette.isDark)")
        logger.info("   - foreground: \(palette.foreground)")
        logger.info("   - background: \(palette.background)")
        logger.info("   - cursor: \(palette.cursor)")
        logger.info("   - ansiColors count: \(palette.ansiColors.count)")

        // Apply foreground
        if let fg = UIColor.fromHex(palette.foreground) {
            self.nativeForegroundColor = fg
            logger.info("Applied foreground: \(palette.foreground)")
        } else {
            logger.error("Failed to convert foreground color: \(palette.foreground)")
        }

        // Apply cursor
        if let cursorColor = UIColor.fromHex(palette.cursor) {
            self.caretColor = cursorColor
            logger.info("Applied cursor: \(palette.cursor)")
        } else {
            logger.error("Failed to convert cursor color: \(palette.cursor)")
        }

        // Apply ANSI colors
        guard palette.ansiColors.count == 16 else {
            logger.error("ANSI colors count is \(palette.ansiColors.count), expected 16")
            return
        }

        let swiftTermColors: [SwiftTerm.Color] = palette.ansiColors.compactMap { hex in
            guard let ui = UIColor.fromHex(hex) else {
                logger.error("Failed to convert ANSI color: \(hex)")
                return nil
            }
            return ui.toSwiftTermColor()
        }

        if swiftTermColors.count == 16 {
            self.installColors(swiftTermColors)
            logger.info("Applied \(swiftTermColors.count) ANSI colors")
        } else {
            logger.error("Only \(swiftTermColors.count) ANSI colors converted successfully")
        }
    }
}

#endif

// Note: SwiftUI.Color.init(hex:) is already defined in HyaloColorTheme.swift
