// UtilityAreaTab.swift - Utility area tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

@available(macOS 26.0, *)
enum UtilityAreaTab: String, CaseIterable, HyaloPanelTab {
    case terminal
    case diagnostics

    var id: String { rawValue }

    var title: String {
        switch self {
        case .terminal: return "Terminal"
        case .diagnostics: return "Diagnostics"
        }
    }

    var systemImage: String {
        switch self {
        case .terminal: return "terminal"
        case .diagnostics: return "exclamationmark.triangle"
        }
    }

    /// Default body for HyaloPanelTab conformance.
    /// Not used at runtime — UtilityAreaView.tabContent handles rendering.
    var body: some View {
        EmptyView()
    }
}
