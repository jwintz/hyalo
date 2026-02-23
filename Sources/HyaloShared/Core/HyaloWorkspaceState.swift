// HyaloWorkspaceState.swift - Central workspace state for Hyalo
// Cross-platform: macOS 26 / iOS 26

import SwiftUI

#if os(macOS)
import AppKit
#else
import UIKit
#endif

// MARK: - Vibrancy Material

public enum VibrancyMaterial: String, CaseIterable {
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
public final class HyaloWorkspaceState {

    // MARK: - Window Identity

    public var projectName: String = ""
    public var projectRoot: String = ""
    public var projectURL: URL?

    // MARK: - Appearance (persisted to UserDefaults)

    public var backgroundColor: PlatformColor = .systemBackground
    public var backgroundAlpha: CGFloat = 0.5
    public var windowAppearance: String = "auto"
    public var vibrancyMaterial: VibrancyMaterial = .ultraThin
    public var decorationsVisible: Bool = true

    // MARK: - Loading State

    /// True while Emacs init.el is still running.  Cleared by `hyalo-loading-done'.
    /// Used to track initialization state across the workspace.
    public var isLoading: Bool = true

    // MARK: - Theme Name (not persisted — pushed from Emacs)

    public var currentThemeName: String = ""

    // MARK: - Color Theme (inferred from Emacs theme faces)

    public var colorTheme = HyaloColorTheme()

    // MARK: - Panel Visibility

    public var navigatorVisible: Bool = false
    public var inspectorVisible: Bool = false
    public var utilityAreaVisible: Bool = false
    public var utilityAreaMaximized: Bool = false

    // MARK: - Panel Dimensions

    public var navigatorWidth: CGFloat = 280
    public var inspectorWidth: CGFloat = 300
    public var utilityAreaHeight: CGFloat = 260

    // MARK: - Footer Pattern

    public var footerPattern: FooterPattern = .none
    public var footerHeight: CGFloat = 0
    public var footerBackgroundAlpha: CGFloat = 0.3
    public var footerPatternAlpha: CGFloat = 0.15

    // MARK: - Toolbar

    public var toolbarHeight: CGFloat = 52

    // MARK: - Persistence

    private static let kAlpha = "hyalo.appearance.alpha"
    private static let kMaterial = "hyalo.appearance.material"

    public init() {
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

    public func saveAppearance() {
        let defaults = UserDefaults.standard
        // Appearance mode is not persisted — always inferred from system.
        defaults.set(backgroundAlpha, forKey: Self.kAlpha)
        defaults.set(vibrancyMaterial.rawValue, forKey: Self.kMaterial)
    }

    // MARK: - Computed Properties

    public var isDarkMode: Bool {
        switch windowAppearance {
        case "dark": return true
        case "light": return false
        default:
            // Read from colorTheme.isDark — a tracked @Observable property —
            // so SwiftUI re-evaluates any view that depends on isDarkMode when
            // the system appearance changes.
            return colorTheme.isDark
        }
    }

    #if os(macOS)
    public var vibrancyMaterialNS: NSVisualEffectView.Material {
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
public protocol HyaloPanelState: AnyObject {
    associatedtype Tab: HyaloPanelTab
    var selectedTab: Tab? { get set }
    var tabItems: [Tab] { get set }
}

/// Protocol for panel tabs
public protocol HyaloPanelTab: View, Identifiable, Hashable {
    var title: String { get }
    var systemImage: String { get }
}
