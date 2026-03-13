// StatusBarViewModel.swift - Status bar state model
// Target: macOS 26 Tahoe with Liquid Glass design
// Shows emacs modeline segments: cursor, mode, minor modes, encoding, etc.

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
public final class StatusBarViewModel {
    // Cursor
    public var line: Int = 1
    public var column: Int = 1

    // Mode information
    public var mode: String = "Fundamental"
    public var minorModes: [String] = [] {
        didSet { classifiedMinorModes = minorModes.map { ($0, $0.containsNerdIconGlyph) } }
    }

    /// Pre-classified minor modes to avoid Unicode scanning in view body.
    public private(set) var classifiedMinorModes: [(text: String, isNerdGlyph: Bool)] = []

    // File metadata
    public var encoding: String = "UTF-8"
    public var lineEnding: String = "LF"
    public var indentStyle: String = "Spaces"
    public var indentWidth: Int = 4
    public var fileType: String = ""
    public var fileSize: String = ""

    // Raw modeline segments from Emacs (LHS and RHS)
    public var modelineLHS: String = ""
    public var modelineRHS: String = ""

    // Callbacks (set by channel)
    public var onEncodingChange: ((String) -> Void)?
    public var onLineEndingChange: ((String) -> Void)?
    public var onIndentStyleChange: ((String, Int) -> Void)?

    public var cursorPosition: String {
        "Ln \(line), Col \(column)"
    }

    public var indentDescription: String {
        "\(indentStyle): \(indentWidth)"
    }
}

// MARK: - Nerd Font Glyph Detection

extension String {
    /// Whether this string contains any Nerd Fonts Private Use Area glyphs.
    /// Nerd Fonts use PUA ranges: U+E000–U+F8FF (BMP PUA),
    /// U+F0000–U+FFFFD (Supplementary PUA-A), U+100000–U+10FFFD (Supplementary PUA-B).
    var containsNerdIconGlyph: Bool {
        unicodeScalars.contains { scalar in
            (0xE000...0xF8FF).contains(scalar.value) ||
            (0xF0000...0xFFFFD).contains(scalar.value) ||
            (0x100000...0x10FFFD).contains(scalar.value)
        }
    }
}
