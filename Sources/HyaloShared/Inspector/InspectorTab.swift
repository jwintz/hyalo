// InspectorTab.swift - Inspector sidebar tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design
// Terminal removed (available in utility area). Appearance panel integrated.

import SwiftUI

@available(macOS 26.0, iOS 26.0, *)
public enum InspectorTab: String, CaseIterable, HyaloPanelTab {
    case file
    case history
    case appearance

    public var id: String { rawValue }

    public var title: String {
        switch self {
        case .file: return "File"
        case .history: return "History"
        case .appearance: return "Appearance"
        }
    }

    public var systemImage: String {
        switch self {
        case .file: return "doc"
        case .history: return "clock.arrow.circlepath"
        case .appearance: return "paintbrush"
        }
    }

    public var body: some View {
        switch self {
        case .file: InspectorTabContent(tab: .file)
        case .history: InspectorTabContent(tab: .history)
        case .appearance:
#if os(macOS)
            InspectorAppearanceView()
#else
            AppearanceTabContent()
#endif
        }
    }
}

@available(macOS 26.0, iOS 26.0, *)
private struct InspectorTabContent: View {
    let tab: InspectorTab
    @Environment(\.inspectorViewModel) private var envVM

    private var viewModel: InspectorViewModel { envVM ?? InspectorManager.shared.viewModel }

    var body: some View {
        switch tab {
        case .file: FileInspectorView(viewModel: viewModel)
        case .history: HistoryInspectorView(viewModel: viewModel)
        default: EmptyView()
        }
    }
}

#if !os(macOS)
@available(iOS 26.0, *)
private struct AppearanceTabContent: View {
    @Environment(HyaloWorkspaceState.self) private var workspace

    var body: some View {
        InspectorAppearanceView(workspace: workspace)
    }
}
#endif
