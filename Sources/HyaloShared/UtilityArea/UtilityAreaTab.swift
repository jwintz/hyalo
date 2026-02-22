// UtilityAreaTab.swift - Utility area tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

@available(macOS 26.0, iOS 26.0, *)
public enum UtilityAreaTab: String, CaseIterable, HyaloPanelTab {
    case terminal
    case diagnostics

    public var id: String { rawValue }

    public var title: String {
        switch self {
        case .terminal: return "Terminal"
        case .diagnostics: return "Diagnostics"
        }
    }

    public var systemImage: String {
        switch self {
        case .terminal: return "terminal"
        case .diagnostics: return "exclamationmark.triangle"
        }
    }

    /// Default body for HyaloPanelTab conformance.
    /// Not used at runtime — UtilityAreaView.tabContent handles rendering.
    public var body: some View {
        EmptyView()
    }
}
