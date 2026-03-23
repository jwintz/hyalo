// WelcomeState.swift - Observable state for the welcome panel
// Target: macOS 26 Tahoe

import SwiftUI

/// Holds the project list and visibility state for the welcome panel.
@available(macOS 26.0, *)
@MainActor
@Observable
public final class WelcomeState {

    /// A known project from project.el.
    public struct Project: Identifiable, Sendable {
        public let id: String
        public let name: String
        public let path: String

        public init(name: String, path: String) {
            self.id = path
            self.name = name
            self.path = path
        }
    }

    /// Projects gathered from `project-known-project-roots`.
    public var projects: [Project] = []

    /// Whether the welcome panel is currently visible.
    public var isVisible: Bool = false

    /// Init time string (e.g. "0.42 seconds") from Emacs, displayed as subtitle.
    public var initTime: String?

    /// Callback invoked when the user selects a project.
    public var onProjectSelected: ((String) -> Void)?

    public init() {}
}
