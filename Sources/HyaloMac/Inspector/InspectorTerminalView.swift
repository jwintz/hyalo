// InspectorTerminalView.swift - SwiftTerm-based terminal for inspector and utility panels
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Ported from emacs.d's InspectorTerminalView. Now uses @Observable TerminalPalette
// for automatic appearance-aware theming that follows hyalo's color theme.

#if os(macOS)
import AppKit
import HyaloShared
import SwiftTerm
import SwiftUI
import OSLog

@available(macOS 26.0, *)
private let logger = Logger(subsystem: "hyalo", category: "InspectorTerminalView")

// MARK: - HyaloTerminalView (key interception)

/// Subclass of `LocalProcessTerminalView` that intercepts Ctrl, arrow,
/// and function keys via `performKeyEquivalent` so Emacs' NSWindow
/// `sendEvent:` does not consume them.
@available(macOS 26.0, *)
final class HyaloTerminalView: LocalProcessTerminalView {

    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        // Only intercept when we are first responder
        guard window?.firstResponder === self else {
            return super.performKeyEquivalent(with: event)
        }

        let flags = event.modifierFlags.intersection(.deviceIndependentFlagsMask)

        // Ctrl+key — send to terminal via keyDown
        if flags.contains(.control) && !flags.contains(.command) {
            self.keyDown(with: event)
            return true
        }

        // Arrow keys and function keys (no Cmd)
        if !flags.contains(.command),
           let chars = event.charactersIgnoringModifiers,
           let scalar = chars.unicodeScalars.first {
            let v = Int(scalar.value)
            let isFunctionKey = (v >= NSUpArrowFunctionKey && v <= NSRightArrowFunctionKey)
                || (v >= NSF1FunctionKey && v <= NSF35FunctionKey)
                || v == NSDeleteFunctionKey
                || v == NSHomeFunctionKey || v == NSEndFunctionKey
                || v == NSPageUpFunctionKey || v == NSPageDownFunctionKey
            if isFunctionKey {
                self.keyDown(with: event)
                return true
            }
        }

        return super.performKeyEquivalent(with: event)
    }
}

// MARK: - Terminal Container (font scaling + click-to-focus)

/// Container NSView that hosts `HyaloTerminalView`.
/// Handles click-to-focus and Cmd+/Cmd- font scaling.
@available(macOS 26.0, *)
final class TerminalContainerView: NSView {
    var terminalView: HyaloTerminalView?
    private var currentFontSize: CGFloat = terminalDefaultFontSize

    override var acceptsFirstResponder: Bool { true }

    override func mouseDown(with event: NSEvent) {
        if let tv = terminalView {
            window?.makeFirstResponder(tv)
        }
    }

    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        guard event.modifierFlags.contains(.command),
              let chars = event.charactersIgnoringModifiers
        else {
            return super.performKeyEquivalent(with: event)
        }

        guard let tv = terminalView,
              window?.firstResponder === tv
        else {
            return super.performKeyEquivalent(with: event)
        }

        switch chars {
        case "+", "=":
            adjustFontSize(delta: 1)
            return true
        case "-":
            adjustFontSize(delta: -1)
            return true
        case "0":
            setFontSize(terminalDefaultFontSize)
            return true
        case "c":
            tv.copy(self)
            return true
        case "v":
            tv.paste(self)
            return true
        default:
            return super.performKeyEquivalent(with: event)
        }
    }

    private func adjustFontSize(delta: CGFloat) {
        let newSize = max(8, min(24, currentFontSize + delta))
        setFontSize(newSize)
    }

    private func setFontSize(_ size: CGFloat) {
        currentFontSize = size
        guard let tv = terminalView else { return }
        if let sfMono = NSFont(name: "SF Mono", size: size) {
            tv.font = sfMono
        } else {
            tv.font = NSFont.monospacedSystemFont(ofSize: size, weight: .regular)
        }
    }
}

// MARK: - NSViewRepresentable Wrapper

/// Wraps `HyaloTerminalView` for SwiftUI. Configures transparent background,
/// SF Mono font, steady underline cursor, and appearance-aware ANSI colors.
///
/// The palette binding ensures the terminal updates when:
/// - The appearance changes (light/dark)
/// - The color scheme is modified
@available(macOS 26.0, *)
struct InspectorTerminalView: NSViewRepresentable {
    /// The palette to use for theming. Use `.shared` for the global palette.
    var palette: TerminalPalette
    /// Tracks system appearance; changes here guarantee updateNSView is called.
    @Environment(\.colorScheme) private var colorScheme

    init(palette: TerminalPalette) {
        self.palette = palette
    }

    func makeNSView(context: Context) -> TerminalContainerView {
        let container = TerminalContainerView()
        container.autoresizesSubviews = true

        let terminalView = HyaloTerminalView(frame: container.bounds)
        terminalView.autoresizingMask = [.width, .height]
        terminalView.processDelegate = context.coordinator

        // Font: SF Mono at default size
        if let sfMono = NSFont(name: "SF Mono", size: terminalDefaultFontSize) {
            terminalView.font = sfMono
        } else {
            terminalView.font = NSFont.monospacedSystemFont(ofSize: terminalDefaultFontSize, weight: .regular)
        }

        // Transparent background: the panel already provides vibrancy
        terminalView.nativeBackgroundColor = NSColor.clear
        terminalView.layer?.backgroundColor = NSColor.clear.cgColor
        terminalView.layer?.isOpaque = false

        // Cursor: non-blinking underscore
        terminalView.terminal.setCursorStyle(.steadyUnderline)

        // Apply palette colors
        terminalView.applyPalette(palette)

        // Option key sends Meta (ESC prefix) for terminal apps
        terminalView.optionAsMetaKey = true

        container.addSubview(terminalView)
        container.terminalView = terminalView

        // Start a login shell
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
            currentDirectory: NSHomeDirectory()
        )

        context.coordinator.terminalView = terminalView

        return container
    }

    func updateNSView(_ container: TerminalContainerView, context: Context) {
        // Sync palette appearance from SwiftUI's colorScheme so "auto" mode
        // picks up system dark/light switches immediately.
        let systemIsDark = colorScheme == .dark
        if palette.isDark != systemIsDark {
            palette.setAppearance(isDark: systemIsDark)
        }

        // Only reapply palette when it has actually changed
        let currentVersion = palette.version
        guard currentVersion != context.coordinator.lastAppliedPaletteVersion else { return }
        context.coordinator.lastAppliedPaletteVersion = currentVersion

        if let tv = container.terminalView {
            tv.applyPalette(palette)
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator()
    }

    // MARK: - Coordinator

    final class Coordinator: NSObject, LocalProcessTerminalViewDelegate {
        weak var terminalView: HyaloTerminalView?
        /// Tracks the last applied palette version to skip redundant applyPalette calls.
        var lastAppliedPaletteVersion: Int = -1

        func sizeChanged(source: LocalProcessTerminalView, newCols: Int, newRows: Int) {}
        func setTerminalTitle(source: LocalProcessTerminalView, title: String) {}
        func hostCurrentDirectoryUpdate(source: TerminalView, directory: String?) {}

        func processTerminated(source: TerminalView, exitCode: Int32?) {
            guard let tv = terminalView else { return }

            // Reset terminal state (process is already dead when this
            // delegate callback fires -- no need to call terminate()).
            tv.terminal.feed(text: "\u{001b}c")
            tv.terminal.feed(text: "\u{001b}[2J\u{001b}[H")

            // Re-attach delegate before starting the new process
            tv.processDelegate = self

            let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
            var env = ProcessInfo.processInfo.environment
            env["TERM"] = "xterm-256color"
            env["COLORTERM"] = "truecolor"
            if env["LANG"] == nil { env["LANG"] = "en_US.UTF-8" }
            let envArray = env.map { "\($0.key)=\($0.value)" }

            tv.startProcess(
                executable: shell,
                args: ["-l", "-i"],
                environment: envArray,
                currentDirectory: NSHomeDirectory()
            )
        }
    }
}

#endif
