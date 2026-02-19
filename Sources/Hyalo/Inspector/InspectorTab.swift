// InspectorTab.swift - Inspector sidebar tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design
// Terminal removed (available in utility area). Appearance panel integrated.

import SwiftUI

@available(macOS 26.0, *)
enum InspectorTab: String, CaseIterable, HyaloPanelTab {
    case file
    case history
    case appearance

    var id: String { rawValue }

    var title: String {
        switch self {
        case .file: return "File"
        case .history: return "History"
        case .appearance: return "Appearance"
        }
    }

    var systemImage: String {
        switch self {
        case .file: return "doc"
        case .history: return "clock.arrow.circlepath"
        case .appearance: return "paintbrush"
        }
    }

    var body: some View {
        switch self {
        case .file: FileInspectorView(viewModel: InspectorManager.shared.viewModel)
        case .history: HistoryInspectorView(viewModel: InspectorManager.shared.viewModel)
        case .appearance: InspectorAppearanceView()
        }
    }
}
