// UtilityAreaTerminalView.swift - Persistent SwiftTerm terminal for the utility area
// Target: macOS 26 Tahoe with Liquid Glass design
//
// The terminal instance is created once and reused — no reinit on tab switch.
// Uses @Observable TerminalPalette for appearance-aware theming.

import AppKit
import SwiftTerm
import SwiftUI
import HyaloShared
import OSLog

@available(macOS 26.0, *)
private let logger = Logger(subsystem: "hyalo", category: "UtilityAreaTerminalView")

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
    /// - Parameter palette: The color palette to apply
    func ensureTerminal(
        currentDirectory: String? = nil,
        palette: TerminalPalette
    ) -> TerminalContainerView {
        if let existing = container {
            if let tv = existing.terminalView {
                tv.applyPalette(palette)
            }
            return existing
        }

        logger.info("Creating new terminal container")
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
        terminalView.applyPalette(palette)

        let coordinator = InspectorTerminalView.Coordinator()
        terminalView.processDelegate = coordinator
        coordinator.terminalView = terminalView

        newContainer.addSubview(terminalView)
        newContainer.terminalView = terminalView

        // Start shell
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"

        // Prepare environment: merge host environment with terminal-specific variables.
        // SwiftTerm.Terminal.getEnvironmentVariables() is too sparse (missing PATH, etc).
        var env = ProcessInfo.processInfo.environment
        env["TERM"] = "xterm-256color"
        env["COLORTERM"] = "truecolor"
        if env["LANG"] == nil { env["LANG"] = "en_US.UTF-8" }
        let envArray = env.map { "\($0.key)=\($0.value)" }

        terminalView.startProcess(
            executable: shell,
            args: ["-l", "-i"],
            environment: envArray,
            currentDirectory: cwd
        )

        container = newContainer
        return newContainer
    }
}

/// NSViewRepresentable wrapper that reuses the persistent terminal container.
/// Each instance is bound to a specific UtilityAreaTerminalHolder (per-frame).
/// Automatically updates when the palette changes (appearance or scheme).
@available(macOS 26.0, *)
struct UtilityAreaTerminalView: NSViewRepresentable {
    /// The palette binding ensures updates when appearance or colors change
    @Bindable var palette: TerminalPalette
    /// Tracks system appearance; changes here guarantee updateNSView is called.
    @Environment(\.colorScheme) private var colorScheme
    private let holder: UtilityAreaTerminalHolder
    /// Tracks the last applied palette version to skip redundant applyPalette calls.
    private class PaletteVersionBox {
        var lastAppliedVersion: Int = -1
    }
    private let paletteVersionBox = PaletteVersionBox()

    init(holder: UtilityAreaTerminalHolder, palette: TerminalPalette) {
        self.holder = holder
        self.palette = palette
    }

    func makeNSView(context: Context) -> NSView {
        let projectRoot = NavigatorManager.shared.projectNavigatorViewModel.projectRoot
        return holder.ensureTerminal(currentDirectory: projectRoot, palette: palette)
    }

    func updateNSView(_ nsView: NSView, context: Context) {
        guard let container = nsView as? TerminalContainerView,
              let tv = container.terminalView else {
            return
        }

        // Sync palette appearance from SwiftUI's colorScheme so "auto" mode
        // picks up system dark/light switches immediately.
        let systemIsDark = colorScheme == .dark
        if palette.isDark != systemIsDark {
            palette.setAppearance(isDark: systemIsDark)
        }

        // Only reapply palette when it has actually changed
        let currentVersion = palette.version
        guard currentVersion != paletteVersionBox.lastAppliedVersion else { return }
        paletteVersionBox.lastAppliedVersion = currentVersion

        tv.applyPalette(palette)
    }
}
