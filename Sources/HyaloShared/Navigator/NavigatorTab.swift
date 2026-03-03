// NavigatorTab.swift - Navigator sidebar tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI
import KelyphosKit

@available(macOS 26.0, iOS 26.0, *)
public enum NavigatorTab: String, CaseIterable, KelyphosPanel {
    case project
    case buffers
    case sourceControl
    case search

    public var id: String { rawValue }

    public var title: String {
        switch self {
        case .project: return "Project"
        case .buffers: return "Buffers"
        case .sourceControl: return "VCS"
        case .search: return "Search"
        }
    }

    public var systemImage: String {
        switch self {
        case .project: return "folder"
        case .buffers: return "doc.on.doc"
        case .sourceControl: return "arrow.triangle.branch"
        case .search: return "magnifyingglass"
        }
    }

    public var body: some View {
        switch self {
        case .project: ProjectNavigatorView()
        case .buffers: BufferListView()
        case .sourceControl: SourceControlNavigatorView()
        case .search: FindNavigatorView()
        }
    }
}
