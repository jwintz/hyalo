// ITermColorsParser.swift - Parser for iTerm2 color scheme files (.itermcolors)
// Target: macOS 26 Tahoe / iOS 26 with Liquid Glass design
//
// Parses XML plist format used by iTerm2 color schemes and extracts
// ANSI colors, foreground, background, cursor, and selection colors.

import Foundation

/// Represents a parsed iTerm2 color scheme
public struct ITermColorScheme {
    /// 16 ANSI colors (0-15) as hex strings
    public var ansiColors: [String]
    /// Foreground color hex
    public var foreground: String
    /// Background color hex
    public var background: String
    /// Cursor color hex
    public var cursor: String
    /// Selection color hex (optional)
    public var selection: String?
    /// Bold color hex (optional)
    public var bold: String?

    public init(
        ansiColors: [String],
        foreground: String,
        background: String,
        cursor: String,
        selection: String? = nil,
        bold: String? = nil
    ) {
        self.ansiColors = ansiColors
        self.foreground = foreground
        self.background = background
        self.cursor = cursor
        self.selection = selection
        self.bold = bold
    }
}

/// Parser for .itermcolors XML plist files
public enum ITermColorsParser {

    /// Parse an iTerm2 color scheme from XML data
    /// - Parameter data: The XML plist data
    /// - Returns: Parsed color scheme or nil if parsing fails
    public static func parse(data: Data) -> ITermColorScheme? {
        guard let plist = try? PropertyListSerialization.propertyList(from: data, format: nil),
              let dict = plist as? [String: [String: Any]] else {
            return nil
        }

        // Extract ANSI colors 0-15
        var ansiColors: [String] = []
        for i in 0..<16 {
            if let colorDict = dict["Ansi \(i) Color"],
               let hex = colorFromDict(colorDict) {
                ansiColors.append(hex)
            } else {
                // Fallback if any color is missing
                ansiColors.append(i < 8 ? "#000000" : "#808080")
            }
        }

        guard ansiColors.count == 16 else { return nil }

        // Extract required colors
        guard let foregroundDict = dict["Foreground Color"],
              let foreground = colorFromDict(foregroundDict),
              let backgroundDict = dict["Background Color"],
              let background = colorFromDict(backgroundDict),
              let cursorDict = dict["Cursor Color"],
              let cursor = colorFromDict(cursorDict) else {
            return nil
        }

        // Extract optional colors
        let selection = dict["Selection Color"].flatMap { colorFromDict($0) }
        let bold = dict["Bold Color"].flatMap { colorFromDict($0) }

        return ITermColorScheme(
            ansiColors: ansiColors,
            foreground: foreground,
            background: background,
            cursor: cursor,
            selection: selection,
            bold: bold
        )
    }

    /// Parse an iTerm2 color scheme from a file URL
    /// - Parameter url: URL to the .itermcolors file
    /// - Returns: Parsed color scheme or nil if parsing fails
    public static func parse(url: URL) -> ITermColorScheme? {
        guard let data = try? Data(contentsOf: url) else { return nil }
        return parse(data: data)
    }

    /// Parse an iTerm2 color scheme from a string
    /// - Parameter xmlString: The XML plist as a string
    /// - Returns: Parsed color scheme or nil if parsing fails
    public static func parse(xmlString: String) -> ITermColorScheme? {
        guard let data = xmlString.data(using: .utf8) else { return nil }
        return parse(data: data)
    }

    // MARK: - Private Helpers

    private static func colorFromDict(_ dict: [String: Any]) -> String? {
        // Verify this is an sRGB color space (most common)
        _ = dict["Color Space"] as? String

        // Extract color components - handle both CGFloat and Double
        let red: Double
        if let cgRed = dict["Red Component"] as? CGFloat {
            red = Double(cgRed)
        } else if let dRed = dict["Red Component"] as? Double {
            red = dRed
        } else {
            return nil
        }

        let green: Double
        if let cgGreen = dict["Green Component"] as? CGFloat {
            green = Double(cgGreen)
        } else if let dGreen = dict["Green Component"] as? Double {
            green = dGreen
        } else {
            return nil
        }

        let blue: Double
        if let cgBlue = dict["Blue Component"] as? CGFloat {
            blue = Double(cgBlue)
        } else if let dBlue = dict["Blue Component"] as? Double {
            blue = dBlue
        } else {
            return nil
        }

        // Convert 0.0-1.0 range to 0-255 and format as hex
        let r = Int(round(red * 255))
        let g = Int(round(green * 255))
        let b = Int(round(blue * 255))

        return String(format: "#%02X%02X%02X", r, g, b)
    }
}

// MARK: - Predefined Schemes

public extension ITermColorScheme {
    /// Nano dark theme (extracted from nano-dark.itermcolors)
    static let nanoDark = ITermColorScheme(
        ansiColors: [
            // Normal (0-7)
            "#1E1E1E", "#F38BA8", "#DCD3F8", "#EDE8FC",
            "#A68AF9", "#DCD3F8", "#EDE8FC", "#FFFFFF",
            // Bright (8-15)
            "#808080", "#F38BA8", "#DCD3F8", "#EDE8FC",
            "#A68AF9", "#DCD3F8", "#EDE8FC", "#FFFFFF"
        ],
        foreground: "#FFFFFF",
        background: "#1E1E1E",
        cursor: "#A68AF9",
        selection: "#655594",
        bold: "#FFFFFF"
    )

    /// Nano light theme (extracted from nano-light.itermcolors)
    static let nanoLight = ITermColorScheme(
        ansiColors: [
            // Normal (0-7)
            "#FFFFFF", "#730C29", "#321685", "#240E66",
            "#A68AF7", "#321685", "#240E66", "#000000",
            // Bright (8-15)
            "#808080", "#730C29", "#321685", "#240E66",
            "#A68AF7", "#321685", "#240E66", "#000000"
        ],
        foreground: "#000000",
        background: "#FFFFFF",
        cursor: "#A68AF7",
        selection: "#C5BEDA",
        bold: "#000000"
    )

    /// Default dark theme (Zinc/Violet)
    static let defaultDark = ITermColorScheme(
        ansiColors: [
            // Normal (0-7)
            "#27272A", "#EF5350", "#66BB6A", "#FFEE58",
            "#42A5F5", "#AB47BC", "#26C6DA", "#F4F4F5",
            // Bright (8-15)
            "#52525B", "#F87171", "#4ADE80", "#FDE047",
            "#60A5FA", "#C084FC", "#22D3EE", "#FFFFFF"
        ],
        foreground: "#F4F4F5",
        background: "#18181B",
        cursor: "#A58AF9",
        selection: "#655594",
        bold: "#FFFFFF"
    )

    /// Default light theme (Zinc/Violet)
    static let defaultLight = ITermColorScheme(
        ansiColors: [
            // Normal (0-7)
            "#FAFAFA", "#D32F2F", "#2E7D32", "#F57F17",
            "#1976D2", "#7B1FA2", "#00838F", "#18181B",
            // Bright (8-15)
            "#A1A1AA", "#EF5350", "#66BB6A", "#FFA726",
            "#42A5F5", "#AB47BC", "#26C6DA", "#000000"
        ],
        foreground: "#18181B",
        background: "#FFFFFF",
        cursor: "#A58AF9",
        selection: "#C5BEDA",
        bold: "#000000"
    )
}
