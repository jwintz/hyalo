// Platform.swift - Platform type definitions for Hyalo (macOS)

import SwiftUI
import AppKit

public typealias PlatformColor = NSColor
public typealias PlatformImage = NSImage

// MARK: - Cross-Platform Color Initializer

extension Color {
    /// Initialize from a platform-native color (NSColor).
    init(platformColor: PlatformColor) {
        self.init(nsColor: platformColor)
    }
}

// MARK: - Cross-Platform System Color Names
//
// NSColor and UIColor use slightly different naming conventions
// for the same semantic colors. These extensions unify the names
// so shared code can use `PlatformColor.separator` on both platforms.

extension NSColor {
    /// macOS equivalent of UIColor.systemBackground
    static var systemBackground: NSColor { .windowBackgroundColor }

    /// macOS equivalent of UIColor.separator (NSColor uses "Color" suffix)
    static var separator: NSColor { .separatorColor }

    /// macOS equivalent of UIColor.quaternaryLabel
    static var quaternaryLabel: NSColor { .quaternaryLabelColor }

    /// macOS equivalent of UIColor.tertiaryLabel
    static var tertiaryLabel: NSColor { .tertiaryLabelColor }

    /// macOS equivalent of UIColor.placeholderText
    static var placeholderText: NSColor { .placeholderTextColor }

    /// macOS equivalent of UIColor.secondarySystemBackground
    static var secondarySystemBackground: NSColor { .controlBackgroundColor }
}

extension NSColor {
    /// Unified name for NSColor.selectedContentBackgroundColor
    static var selectedContentBackground: NSColor { .selectedContentBackgroundColor }
}

// MARK: - Cross-Platform Appearance Detection

/// Determines the current interface style (light/dark) in a cross-platform manner.
@MainActor
func platformIsDarkMode() -> Bool {
    let appearance = NSApp.effectiveAppearance
    return appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
}

// MARK: - Cross-Platform Clipboard

/// Copy a string to the system clipboard.
func platformCopyToClipboard(_ string: String) {
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(string, forType: .string)
}

// MARK: - Cross-Platform File Reveal

/// Reveal a file in Finder.
func platformRevealInFinder(_ paths: [String]) {
    let urls = paths.map { URL(fileURLWithPath: $0) }
    NSWorkspace.shared.activateFileViewerSelecting(urls)
}

// MARK: - Cross-Platform Emacs Event Loop

/// Wake the Emacs event loop so it processes pending I/O.
/// Set by each platform at startup:
/// - macOS: calls ns_wake_emacs() via dlsym
public nonisolated(unsafe) var platformWakeEmacs: (() -> Void)?

/// Convenience wrapper used throughout shared code.
func wakeEmacs() {
    platformWakeEmacs?()
}

// MARK: - Cross-Platform Logging

/// Platform log (replaces NSLog usage in shared code).
func platformLog(_ message: String) {
    NSLog("%@", message)
}
