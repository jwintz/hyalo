// HyaloManager.swift - Singleton manager for system-level integration
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit

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
        ) { [weak self] _ in
            _ = self?.getSystemAppearance()
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
