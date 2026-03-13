// HyaloManager.swift - Singleton manager for system-level integration
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import HyaloShared

/// Singleton manager for system-level features:
/// - System appearance detection
/// - Window appearance setting
final class HyaloManager {
    static let shared = HyaloManager()

    /// Corner radius for macOS Tahoe windows
    static let cornerRadius: CGFloat = 12.0

    private var appearanceObserver: NSObjectProtocol?

    private init() {
        setupAppearanceObserver()
    }

    // MARK: - System Appearance

    private func setupAppearanceObserver() {
        appearanceObserver = DistributedNotificationCenter.default.addObserver(
            forName: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil,
            queue: .main
        ) { [unowned self] _ in
            MainActor.assumeIsolated {
                self.updateWindowsForSystemAppearanceChange()
            }
        }
    }

    @MainActor
    private func updateWindowsForSystemAppearanceChange() {
        for controller in HyaloModule.allControllers {
            // Refresh the tracked isDark property so @Observable views re-render.
            controller.workspace.colorTheme.refreshAppearance()
            // Also refresh terminal palette for appearance-aware terminal theming
            TerminalPalette.shared.refreshAppearance()
            // Reset window appearance for windows in "auto" mode.
            if controller.shellState.windowAppearance == "auto" {
                controller.window?.appearance = nil
            }
        }
    }

    func getSystemAppearance() -> String {
        let appearance = NSApp.effectiveAppearance
        if appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua {
            return "dark"
        }
        return "light"
    }

    func setWindowAppearance(_ appearance: String, for window: NSWindow) {
        switch appearance {
        case "light":
            window.appearance = NSAppearance(named: .aqua)
        case "dark":
            window.appearance = NSAppearance(named: .darkAqua)
        default:
            window.appearance = nil
        }
    }

    deinit {
        if let observer = appearanceObserver {
            DistributedNotificationCenter.default.removeObserver(observer)
        }
    }
}
