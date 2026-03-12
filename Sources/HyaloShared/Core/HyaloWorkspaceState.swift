// HyaloWorkspaceState.swift - Emacs-specific workspace state for Hyalo
// Target: macOS 26 Tahoe
//
// Shell state (appearance, panel visibility, dimensions) is owned by
// KelyphosShellState. This class holds only Emacs-specific state.

import SwiftUI
import KelyphosKit

// MARK: - Workspace State

/// Emacs-specific observable state for the Hyalo workspace.
/// Shell appearance and panel visibility live in KelyphosShellState.
@available(macOS 26.0, *)
@MainActor
@Observable
public final class HyaloWorkspaceState {

    // MARK: - Window Identity

    public var projectName: String = ""

    // MARK: - Loading State

    /// True while Emacs init.el is still running. Cleared by `hyalo-loading-done'.
    public var isLoading: Bool = true

    // MARK: - Theme Name (not persisted — pushed from Emacs)

    public var currentThemeName: String = ""

    // MARK: - Color Theme (inferred from Emacs theme faces)

    public var colorTheme = HyaloColorTheme()

    // MARK: - Terminal Settings

    private static let horizontalMarginKey = "hyalo_horizontalMargin"

    public var horizontalMargin: CGFloat {
        didSet { UserDefaults.standard.set(horizontalMargin, forKey: Self.horizontalMarginKey) }
    }

    public init() {
        UserDefaults.standard.register(defaults: [
            Self.horizontalMarginKey: 6.0
        ])
        self.horizontalMargin = CGFloat(UserDefaults.standard.double(forKey: Self.horizontalMarginKey))
    }
}
