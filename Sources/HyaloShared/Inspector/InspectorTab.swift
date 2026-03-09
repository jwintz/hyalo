// InspectorTab.swift - Inspector sidebar tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI
import KelyphosKit

@available(macOS 26.0, *)
public enum InspectorTab: String, CaseIterable, KelyphosPanel {
    case file
    case settings

    public var id: String { rawValue }

    public var title: String {
        switch self {
        case .file: return "File"
        case .settings: return "Settings"
        }
    }

    public var systemImage: String {
        switch self {
        case .file: return "doc"
        case .settings: return "gearshape"
        }
    }

    public var body: some View {
        switch self {
        case .file: FileInspectorTabBody()
        case .settings: SettingsInspectorTabBody()
        }
    }
}

// MARK: - Tab Bodies (read managers from environment)

@available(macOS 26.0, *)
private struct FileInspectorTabBody: View {
    @Environment(\.inspectorViewModel) private var envVM
    private var viewModel: InspectorViewModel { envVM ?? InspectorManager.shared.viewModel }

    var body: some View {
        FileInspectorView(viewModel: viewModel)
    }
}

@available(macOS 26.0, *)
private struct SettingsInspectorTabBody: View {
    var body: some View {
        InspectorAppearanceView()
    }
}
