// Platform.swift - Cross-platform type abstraction for Hyalo
// Provides unified types for macOS (AppKit) and iOS (UIKit)

import SwiftUI

#if os(macOS)
import AppKit
public typealias PlatformColor = NSColor
public typealias PlatformImage = NSImage
#else
import UIKit
public typealias PlatformColor = UIColor
public typealias PlatformImage = UIImage
#endif

// MARK: - Cross-Platform Color Initializer

extension Color {
    /// Initialize from a platform-native color (NSColor on macOS, UIColor on iOS).
    init(platformColor: PlatformColor) {
        #if os(macOS)
        self.init(nsColor: platformColor)
        #else
        self.init(uiColor: platformColor)
        #endif
    }
}

// MARK: - Cross-Platform System Color Names
//
// NSColor and UIColor use slightly different naming conventions
// for the same semantic colors. These extensions unify the names
// so shared code can use `PlatformColor.separator` on both platforms.

#if os(macOS)
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
#else
extension UIColor {
    /// iOS equivalent of NSColor.secondaryLabelColor
    static var secondaryLabelColor: UIColor { .secondaryLabel }

    /// iOS equivalent of NSColor.selectedContentBackgroundColor
    static var selectedContentBackground: UIColor { .systemFill }
}
#endif

// MARK: - Cross-Platform Control Active State
//
// macOS has @Environment(\.controlActiveState) to detect window focus.
// iOS has no equivalent (windows are always "active"), so we provide
// a stub enum and environment key that always returns .key.

#if !os(macOS)
/// Stub to match AppKit's ControlActiveState on iOS.
enum ControlActiveState: Sendable {
    case key, active, inactive
}

private struct ControlActiveStateKey: EnvironmentKey {
    static let defaultValue: ControlActiveState = .key
}

extension EnvironmentValues {
    var controlActiveState: ControlActiveState {
        get { self[ControlActiveStateKey.self] }
        set { self[ControlActiveStateKey.self] = newValue }
    }
}
#endif

// MARK: - Cross-Platform Appearance Detection

/// Determines the current interface style (light/dark) in a cross-platform manner.
@MainActor
func platformIsDarkMode() -> Bool {
    #if os(macOS)
    let appearance = NSApp.effectiveAppearance
    return appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
    #else
    return UITraitCollection.current.userInterfaceStyle == .dark
    #endif
}

// MARK: - Cross-Platform Clipboard

/// Copy a string to the system clipboard.
func platformCopyToClipboard(_ string: String) {
    #if os(macOS)
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(string, forType: .string)
    #else
    UIPasteboard.general.string = string
    #endif
}

// MARK: - Cross-Platform File Reveal

/// Reveal a file in Finder (macOS) or do nothing (iOS — no file manager).
func platformRevealInFinder(_ paths: [String]) {
    #if os(macOS)
    let urls = paths.map { URL(fileURLWithPath: $0) }
    NSWorkspace.shared.activateFileViewerSelecting(urls)
    #else
    // No equivalent on iOS
    #endif
}

// MARK: - Cross-Platform Emacs Event Loop

/// Wake the Emacs event loop so it processes pending I/O.
/// Set by each platform at startup:
/// - macOS: calls ns_wake_emacs() via dlsym
/// - iOS: posts an event to the iOS terminal
public nonisolated(unsafe) var platformWakeEmacs: (() -> Void)?

/// Convenience wrapper used throughout shared code.
func wakeEmacs() {
    platformWakeEmacs?()
}

// MARK: - Cross-Platform Logging

/// Platform log (replaces NSLog usage in shared code).
func platformLog(_ message: String) {
    #if os(macOS)
    NSLog("%@", message)
    #else
    print(message)
    #endif
}
