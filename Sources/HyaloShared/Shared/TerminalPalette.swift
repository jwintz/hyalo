// TerminalPalette.swift - Appearance-aware terminal color palette
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Provides 16 ANSI colors, foreground, background, and cursor colors for
// SwiftTerm terminal views. Automatically switches between light and dark
// variants based on the app's color theme appearance.
//
// Supports loading from iTerm2 .itermcolors files via ITermColorsParser.

import SwiftUI
import Observation
import OSLog

@available(macOS 26.0, *)
private let logger = Logger(subsystem: "hyalo", category: "TerminalPalette")

/// Appearance-aware terminal color palette with light/dark variants.
/// Integrates with HyaloColorTheme to automatically follow system appearance.
@available(macOS 26.0, *)
@MainActor
@Observable
public final class TerminalPalette {
    public static let shared = TerminalPalette()

    // MARK: - Light/Dark Variants

    /// Light mode color scheme
    public var light: ITermColorScheme = .nanoLight
    /// Dark mode color scheme
    public var dark: ITermColorScheme = .nanoDark

    // MARK: - Current Colors (computed from active appearance)

    /// 16 ANSI colors (0-7 normal, 8-15 bright) as hex strings
    public var ansiColors: [String] { isDark ? dark.ansiColors : light.ansiColors }

    /// Foreground color hex
    public var foreground: String { isDark ? dark.foreground : light.foreground }

    /// Background color hex (terminal internal bg, not view bg)
    public var background: String { isDark ? dark.background : light.background }

    /// Cursor color hex
    public var cursor: String { isDark ? dark.cursor : light.cursor }

    /// Selection color hex (optional)
    public var selection: String? { isDark ? dark.selection : light.selection }

    /// Bold color hex (optional)
    public var bold: String? { isDark ? dark.bold : light.bold }

    // MARK: - Version Tracking

    /// Monotonically increasing version; triggers SwiftUI `updateNSView`.
    /// Increment this when changing colors to force terminal views to refresh.
    public private(set) var version: Int = 0

    /// Current appearance mode
    public private(set) var isDark: Bool = platformIsDarkMode()

    // MARK: - Initialization

    nonisolated(unsafe) private var appearanceObserver: NSObjectProtocol?

    private init() {
        // Try to load nano themes from iTermColors files
        loadNanoThemes()
        // Subscribe to appearance changes from HyaloColorTheme
        setupAppearanceObserver()
    }

    deinit {
        if let observer = appearanceObserver {
            NotificationCenter.default.removeObserver(observer)
        }
    }

    // MARK: - Nano Theme Loading

    /// Load nano dark/light themes from .itermcolors files if available.
    /// Looks for nano-dark.itermcolors and nano-light.itermcolors in multiple locations:
    /// - ~/.config/hyalo/
    /// - ~/Library/Application Support/hyalo/
    /// - Current working directory
    private func loadNanoThemes() {
        let fileManager = FileManager.default

        var searchPaths: [String] = []

        if let home = ProcessInfo.processInfo.environment["HOME"] {
            searchPaths.append((home as NSString).appendingPathComponent(".config/hyalo"))
            searchPaths.append((home as NSString).appendingPathComponent("Library/Application Support/hyalo"))
        }

        searchPaths.append(fileManager.currentDirectoryPath)

        var loadedDark: ITermColorScheme?
        var loadedLight: ITermColorScheme?

        for path in searchPaths {
            let darkURL = URL(fileURLWithPath: (path as NSString).appendingPathComponent("nano-dark.itermcolors"))
            let lightURL = URL(fileURLWithPath: (path as NSString).appendingPathComponent("nano-light.itermcolors"))

            if loadedDark == nil,
               fileManager.fileExists(atPath: darkURL.path),
               let scheme = ITermColorsParser.parse(url: darkURL) {
                loadedDark = scheme
                logger.info("TerminalPalette: Loaded dark theme from \(darkURL.path, privacy: .public)")
            }

            if loadedLight == nil,
               fileManager.fileExists(atPath: lightURL.path),
               let scheme = ITermColorsParser.parse(url: lightURL) {
                loadedLight = scheme
                logger.info("TerminalPalette: Loaded light theme from \(lightURL.path, privacy: .public)")
            }

            if loadedDark != nil && loadedLight != nil {
                break
            }
        }

        if let dark = loadedDark {
            self.dark = dark
        } else {
            logger.warning("TerminalPalette: Using embedded dark theme (no file found)")
        }
        if let light = loadedLight {
            self.light = light
        } else {
            logger.warning("TerminalPalette: Using embedded light theme (no file found)")
        }
    }

    // MARK: - Appearance Management

    /// Refresh appearance and update colors if needed.
    /// Call this when the system appearance changes.
    public func refreshAppearance() {
        let newIsDark = platformIsDarkMode()
        if newIsDark != self.isDark {
            self.isDark = newIsDark
            logger.info("TerminalPalette: Appearance changed to \(newIsDark ? "dark" : "light")")
            incrementVersion()
        }
    }

    /// Explicitly set the appearance mode (for manual override)
    public func setAppearance(isDark: Bool) {
        if self.isDark != isDark {
            self.isDark = isDark
            logger.info("TerminalPalette: Appearance set to \(isDark ? "dark" : "light")")
            incrementVersion()
        }
    }

    // MARK: - Color Scheme Management

    /// Set both light and dark color schemes
    public func setSchemes(light: ITermColorScheme, dark: ITermColorScheme) {
        self.light = light
        self.dark = dark
        incrementVersion()
    }

    /// Set only the light color scheme
    public func setLightScheme(_ scheme: ITermColorScheme) {
        self.light = scheme
        incrementVersion()
    }

    /// Set only the dark color scheme
    public func setDarkScheme(_ scheme: ITermColorScheme) {
        self.dark = scheme
        incrementVersion()
    }

    /// Update the current appearance's color scheme with individual colors.
    /// This is useful for dynamic updates from Emacs or other sources.
    /// - Parameters:
    ///   - foreground: Foreground color hex (optional)
    ///   - background: Background color hex (optional)
    ///   - cursor: Cursor color hex (optional)
    ///   - ansiColors: ANSI colors array of 16 hex strings (optional)
    public func updateCurrentScheme(
        foreground: String? = nil,
        background: String? = nil,
        cursor: String? = nil,
        ansiColors: [String]? = nil
    ) {
        if self.isDark {
            var updated = self.dark
            if let fg = foreground { updated.foreground = fg }
            if let bg = background { updated.background = bg }
            if let cur = cursor { updated.cursor = cur }
            if let ansi = ansiColors, ansi.count == 16 { updated.ansiColors = ansi }
            self.dark = updated
        } else {
            var updated = self.light
            if let fg = foreground { updated.foreground = fg }
            if let bg = background { updated.background = bg }
            if let cur = cursor { updated.cursor = cur }
            if let ansi = ansiColors, ansi.count == 16 { updated.ansiColors = ansi }
            self.light = updated
        }
        incrementVersion()
    }

    /// Load color schemes from iTerm2 .itermcolors files
    /// - Parameters:
    ///   - lightURL: URL to the light theme .itermcolors file (optional)
    ///   - darkURL: URL to the dark theme .itermcolors file (optional)
    /// - Returns: True if at least one scheme was loaded successfully
    @discardableResult
    public func loadFromITermColors(light lightURL: URL? = nil, dark darkURL: URL? = nil) -> Bool {
        var loaded = false

        if let lightURL = lightURL,
           let scheme = ITermColorsParser.parse(url: lightURL) {
            self.light = scheme
            loaded = true
        }

        if let darkURL = darkURL,
           let scheme = ITermColorsParser.parse(url: darkURL) {
            self.dark = scheme
            loaded = true
        }

        if loaded {
            incrementVersion()
        }

        return loaded
    }

    /// Load color schemes from iTerm2 .itermcolors file contents
    /// - Parameters:
    ///   - lightData: XML data for light theme (optional)
    ///   - darkData: XML data for dark theme (optional)
    /// - Returns: True if at least one scheme was loaded successfully
    @discardableResult
    public func loadFromITermColors(lightData: Data? = nil, darkData: Data? = nil) -> Bool {
        var loaded = false

        if let lightData = lightData,
           let scheme = ITermColorsParser.parse(data: lightData) {
            self.light = scheme
            loaded = true
        }

        if let darkData = darkData,
           let scheme = ITermColorsParser.parse(data: darkData) {
            self.dark = scheme
            loaded = true
        }

        if loaded {
            incrementVersion()
        }

        return loaded
    }

    // MARK: - Version Control

    /// Increment version to trigger view updates
    public func incrementVersion() {
        self.version += 1
    }

    // MARK: - Private

    private func setupAppearanceObserver() {
        appearanceObserver = NotificationCenter.default.addObserver(
            forName: .init("HyaloAppearanceChanged"),
            object: nil,
            queue: .main
        ) { [unowned self] notification in
            Task { @MainActor in
                if let isDark = notification.userInfo?["isDark"] as? Bool {
                    self.setAppearance(isDark: isDark)
                } else {
                    self.refreshAppearance()
                }
            }
        }
    }
}

// MARK: - Backward Compatibility (Non-@Observable access)

/// Legacy static palette for non-observable contexts
public final class TerminalPaletteLegacy {
    public static let shared = TerminalPaletteLegacy()

    /// 16 ANSI colors (0-7 normal, 8-15 bright) as hex strings
    public var ansiColors: [String] = TerminalPaletteLegacy.defaultDarkAnsi
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
