// StatusBarViewModel.swift - Status bar state model
// Target: macOS 26 Tahoe with Liquid Glass design
// Shows emacs modeline segments: cursor, mode, minor modes, encoding, etc.

import Foundation

@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
public final class StatusBarViewModel {
    // Cursor
    public var line: Int = 1
    public var column: Int = 1

    // Mode information
    public var mode: String = "Fundamental"
    public var minorModes: [String] = []

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
