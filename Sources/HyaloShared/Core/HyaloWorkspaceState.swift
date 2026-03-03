// HyaloWorkspaceState.swift - Emacs-specific workspace state for Hyalo
// Cross-platform: macOS 26 / iOS 26
//
// Shell state (appearance, panel visibility, dimensions) is owned by
// KelyphosShellState. This class holds only Emacs-specific state.

import SwiftUI
import KelyphosKit

// MARK: - Workspace State

/// Emacs-specific observable state for the Hyalo workspace.
/// Shell appearance and panel visibility live in KelyphosShellState.
@available(macOS 26.0, iOS 26.0, *)
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

    public init() {}
}
