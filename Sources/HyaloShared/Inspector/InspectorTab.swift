// InspectorTab.swift - Inspector sidebar tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI
import KelyphosKit

@available(macOS 26.0, iOS 26.0, *)
public enum InspectorTab: String, CaseIterable, KelyphosPanel {
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
        case .file: FileInspectorTabBody()
        case .history: HistoryInspectorTabBody()
        case .appearance: AppearanceInspectorTabBody()
        }
    }
}

// MARK: - Tab Bodies (read managers from environment)

@available(macOS 26.0, iOS 26.0, *)
private struct FileInspectorTabBody: View {
    @Environment(\.inspectorViewModel) private var envVM
    private var viewModel: InspectorViewModel { envVM ?? InspectorManager.shared.viewModel }

    var body: some View {
        FileInspectorView(viewModel: viewModel)
    }
}

@available(macOS 26.0, iOS 26.0, *)
private struct HistoryInspectorTabBody: View {
    @Environment(\.inspectorViewModel) private var envVM
    private var viewModel: InspectorViewModel { envVM ?? InspectorManager.shared.viewModel }

    var body: some View {
        HistoryInspectorView(viewModel: viewModel)
    }
}

@available(macOS 26.0, iOS 26.0, *)
private struct AppearanceInspectorTabBody: View {
    var body: some View {
        InspectorAppearanceView()
    }
}
