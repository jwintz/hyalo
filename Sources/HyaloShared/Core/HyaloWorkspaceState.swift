// HyaloWorkspaceState.swift - Central workspace state for Hyalo
// Cross-platform: macOS 26 / iOS 26

import SwiftUI

#if os(macOS)
import AppKit
#else
import UIKit
#endif

// MARK: - Vibrancy Material

enum VibrancyMaterial: String, CaseIterable {
    case none = "none"
    case ultraThick = "ultraThick"
    case thick = "thick"
    case regular = "regular"
    case thin = "thin"
    case ultraThin = "ultraThin"
}

// MARK: - Workspace State

/// Central observable state for the Hyalo workspace.
/// All UI components bind to this single source of truth.
@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
final class HyaloWorkspaceState {

    // MARK: - Window Identity

    var projectName: String = ""
    var projectRoot: String = ""
    var projectURL: URL?

    // MARK: - Appearance (persisted to UserDefaults)

    var backgroundColor: PlatformColor = .systemBackground
    var backgroundAlpha: CGFloat = 0.5
    var windowAppearance: String = "auto"
    var vibrancyMaterial: VibrancyMaterial = .ultraThin
    var decorationsVisible: Bool = true

    // MARK: - Loading State

    /// True while Emacs init.el is still running.  Cleared by `hyalo-loading-done'.
    /// Used to track initialization state across the workspace.
    var isLoading: Bool = true

    // MARK: - Theme Name (not persisted — pushed from Emacs)

    var currentThemeName: String = ""

    // MARK: - Color Theme (inferred from Emacs theme faces)

    var colorTheme = HyaloColorTheme()

    // MARK: - Panel Visibility

    var navigatorVisible: Bool = false
    var inspectorVisible: Bool = false
    var utilityAreaVisible: Bool = false
    var utilityAreaMaximized: Bool = false

    // MARK: - Panel Dimensions

    var navigatorWidth: CGFloat = 280
    var inspectorWidth: CGFloat = 300
    var utilityAreaHeight: CGFloat = 260

    // MARK: - Footer Pattern

    var footerPattern: FooterPattern = .none
    var footerHeight: CGFloat = 0
    var footerBackgroundAlpha: CGFloat = 0.3
    var footerPatternAlpha: CGFloat = 0.15

    // MARK: - Toolbar

    var toolbarHeight: CGFloat = 52

    // MARK: - Persistence

    private static let kAlpha = "hyalo.appearance.alpha"
    private static let kMaterial = "hyalo.appearance.material"

    init() {
        // Appearance mode is always inferred from system settings
        // (not persisted) so the picker reflects the actual system state.
        self.windowAppearance = Self.systemAppearanceMode()

        let defaults = UserDefaults.standard
        if defaults.object(forKey: Self.kAlpha) != nil {
            self.backgroundAlpha = defaults.double(forKey: Self.kAlpha)
        }
        if let mat = defaults.string(forKey: Self.kMaterial),
           let material = VibrancyMaterial(rawValue: mat) {
            self.vibrancyMaterial = material
        }
    }

    /// Read the system appearance preference.
    /// - Returns: "auto", "light", or "dark"
    private static func systemAppearanceMode() -> String {
        #if os(macOS)
        let defaults = UserDefaults.standard
        let autoSwitches = defaults.bool(forKey: "AppleInterfaceStyleSwitchesAutomatically")
        if autoSwitches {
            return "auto"
        }
        let style = defaults.string(forKey: "AppleInterfaceStyle")
        if style?.caseInsensitiveCompare("dark") == .orderedSame {
            return "dark"
        }
        return "light"
        #else
        // On iOS, always use "auto" — the system controls appearance.
        return "auto"
        #endif
    }

    func saveAppearance() {
        let defaults = UserDefaults.standard
        // Appearance mode is not persisted — always inferred from system.
        defaults.set(backgroundAlpha, forKey: Self.kAlpha)
        defaults.set(vibrancyMaterial.rawValue, forKey: Self.kMaterial)
    }

    // MARK: - Computed Properties

    var isDarkMode: Bool {
        switch windowAppearance {
        case "dark": return true
        case "light": return false
        default:
            return platformIsDarkMode()
        }
    }

    #if os(macOS)
    var vibrancyMaterialNS: NSVisualEffectView.Material {
        switch vibrancyMaterial {
        case .none: return .windowBackground
        case .ultraThick: return .headerView
        case .thick: return .titlebar
        case .regular: return .menu
        case .thin: return .popover
        case .ultraThin: return .hudWindow
        }
    }
    #endif
}

// MARK: - Panel State Protocols

/// Protocol for panel view models
protocol HyaloPanelState: AnyObject {
    associatedtype Tab: HyaloPanelTab
    var selectedTab: Tab? { get set }
    var tabItems: [Tab] { get set }
}

/// Protocol for panel tabs
protocol HyaloPanelTab: View, Identifiable, Hashable {
    var title: String { get }
    var systemImage: String { get }
}
