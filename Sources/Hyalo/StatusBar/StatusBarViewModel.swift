// StatusBarViewModel.swift - Status bar state model
// Target: macOS 26 Tahoe with Liquid Glass design
// Shows emacs modeline segments: cursor, mode, minor modes, encoding, etc.

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
final class StatusBarViewModel {
    // Cursor
    var line: Int = 1
    var column: Int = 1

    // Mode information
    var mode: String = "Fundamental"
    var minorModes: [String] = []

    // File metadata
    var encoding: String = "UTF-8"
    var lineEnding: String = "LF"
    var indentStyle: String = "Spaces"
    var indentWidth: Int = 4
    var fileType: String = ""
    var fileSize: String = ""

    // Raw modeline segments from Emacs (LHS and RHS)
    var modelineLHS: String = ""
    var modelineRHS: String = ""

    // Callbacks (set by channel)
    var onEncodingChange: ((String) -> Void)?
    var onLineEndingChange: ((String) -> Void)?
    var onIndentStyleChange: ((String, Int) -> Void)?

    var cursorPosition: String {
        "Ln \(line), Col \(column)"
    }

    var indentDescription: String {
        "\(indentStyle): \(indentWidth)"
    }
}
