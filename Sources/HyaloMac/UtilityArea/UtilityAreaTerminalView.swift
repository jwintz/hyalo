// UtilityAreaTerminalView.swift - Persistent SwiftTerm terminal for the utility area
// Target: macOS 26 Tahoe with Liquid Glass design
// The terminal instance is created once and reused — no reinit on tab switch.

import AppKit
import SwiftTerm
import SwiftUI

/// Holds a persistent TerminalContainerView for a single frame's utility area.
/// Each window controller owns its own holder via UtilityAreaViewModel,
/// so multiple frames each get their own terminal process.
@available(macOS 26.0, *)
@MainActor
final class UtilityAreaTerminalHolder {

    /// The persistent terminal container. Created lazily on first access.
    private(set) var container: TerminalContainerView?

    init() {}

    /// Returns the persistent terminal container, creating it on first call.
    /// - Parameter currentDirectory: Working directory for the shell. Used only on first creation.
    func ensureTerminal(currentDirectory: String? = nil) -> TerminalContainerView {
        if let existing = container {
            return existing
        }

        let cwd = currentDirectory ?? NSHomeDirectory()

        let newContainer = TerminalContainerView()
        newContainer.autoresizesSubviews = true

        let terminalView = HyaloTerminalView(frame: newContainer.bounds)
        terminalView.autoresizingMask = [.width, .height]

        // Font: SF Mono at default size
        if let sfMono = NSFont(name: "SF Mono", size: terminalDefaultFontSize) {
            terminalView.font = sfMono
        } else {
            terminalView.font = NSFont.monospacedSystemFont(ofSize: terminalDefaultFontSize, weight: .regular)
        }

        // Transparent background
        terminalView.nativeBackgroundColor = NSColor.clear
        terminalView.layer?.backgroundColor = NSColor.clear.cgColor
        terminalView.layer?.isOpaque = false

        // Cursor: non-blinking underscore
        terminalView.terminal.setCursorStyle(.steadyUnderline)

        // Option key sends Meta
        terminalView.optionAsMetaKey = true

        // Apply palette
        let palette = TerminalPalette.shared
        if let fg = nsColorFromHex(palette.foreground) {
            terminalView.nativeForegroundColor = fg
        }
        terminalView.nativeBackgroundColor = NSColor.clear
        if let cursorColor = nsColorFromHex(palette.cursor) {
            terminalView.caretColor = cursorColor
        }
        if palette.ansiColors.count == 16 {
            let swiftTermColors: [SwiftTerm.Color] = palette.ansiColors.compactMap { hex in
                guard let ns = nsColorFromHex(hex) else { return nil }
                var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
                let converted = ns.usingColorSpace(.sRGB) ?? ns
                converted.getRed(&r, green: &g, blue: &b, alpha: &a)
                return SwiftTerm.Color(
                    red: UInt16(r * 65535),
                    green: UInt16(g * 65535),
                    blue: UInt16(b * 65535)
                )
            }
            if swiftTermColors.count == 16 {
                terminalView.installColors(swiftTermColors)
            }
        }

        let coordinator = InspectorTerminalView.Coordinator()
        terminalView.processDelegate = coordinator
        coordinator.terminalView = terminalView

        newContainer.addSubview(terminalView)
        newContainer.terminalView = terminalView

        // Start shell
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        terminalView.startProcess(
            executable: shell,
            args: ["--login"],
            environment: SwiftTerm.Terminal.getEnvironmentVariables(termName: "xterm-256color"),
            currentDirectory: cwd
        )

        container = newContainer
        return newContainer
    }

    private func nsColorFromHex(_ hex: String) -> NSColor? {
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
}

/// NSViewRepresentable wrapper that reuses the persistent terminal container.
/// Each instance is bound to a specific UtilityAreaTerminalHolder (per-frame).
@available(macOS 26.0, *)
struct UtilityAreaTerminalView: NSViewRepresentable {
    /// Value-type snapshot that forces SwiftUI to call `updateNSView` on change.
    let paletteVersion: Int
    private let palette: TerminalPalette
    private let holder: UtilityAreaTerminalHolder

    init(holder: UtilityAreaTerminalHolder, palette: TerminalPalette = .shared) {
        self.holder = holder
        self.palette = palette
        self.paletteVersion = palette.version
    }

    func makeNSView(context: Context) -> NSView {
        let projectRoot = NavigatorManager.shared.projectNavigatorViewModel.projectRoot
        return holder.ensureTerminal(currentDirectory: projectRoot)
    }

    func updateNSView(_ nsView: NSView, context: Context) {
        guard let container = nsView as? TerminalContainerView,
              let tv = container.terminalView else { return }
        // Reapply palette on theme change
        if let fg = nsColorFromHex(palette.foreground) {
            tv.nativeForegroundColor = fg
        }
        tv.nativeBackgroundColor = NSColor.clear
        if let cursorColor = nsColorFromHex(palette.cursor) {
            tv.caretColor = cursorColor
        }
        if palette.ansiColors.count == 16 {
            let swiftTermColors: [SwiftTerm.Color] = palette.ansiColors.compactMap { hex in
                guard let ns = nsColorFromHex(hex) else { return nil }
                var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
                let converted = ns.usingColorSpace(.sRGB) ?? ns
                converted.getRed(&r, green: &g, blue: &b, alpha: &a)
                return SwiftTerm.Color(
                    red: UInt16(r * 65535),
                    green: UInt16(g * 65535),
                    blue: UInt16(b * 65535)
                )
            }
            if swiftTermColors.count == 16 {
                tv.installColors(swiftTermColors)
            }
        }
    }

    private func nsColorFromHex(_ hex: String) -> NSColor? {
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
}
