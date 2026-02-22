import SwiftUI
import Observation

public final class TerminalPalette {
    public static let shared = TerminalPalette()

    /// 16 ANSI colors (0-7 normal, 8-15 bright) as hex strings
    public var ansiColors: [String] = TerminalPalette.defaultDarkAnsi
    /// Foreground color hex
    public var foreground: String = "#F4F4F5"
    /// Background color hex (terminal internal bg, not view bg)
    public var background: String = "#18181B"
    /// Cursor color hex
    public var cursor: String = "#A58AF9"

    /// Monotonically increasing version; triggers SwiftUI `updateNSView`.
    public var version: Int = 0

    // Default dark ANSI palette
    public static let defaultDarkAnsi: [String] = [
        // Normal (0-7): black, red, green, yellow, blue, magenta, cyan, white
        "#27272A", "#EF5350", "#66BB6A", "#FFEE58",
        "#42A5F5", "#AB47BC", "#26C6DA", "#F4F4F5",
        // Bright (8-15)
        "#52525B", "#F87171", "#4ADE80", "#FDE047",
        "#60A5FA", "#C084FC", "#22D3EE", "#FFFFFF",
    ]

    private init() {}
}

/// Default terminal font size in points.
public let terminalDefaultFontSize: CGFloat = 11
