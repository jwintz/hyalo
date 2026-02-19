// NavigatorTab.swift - Navigator sidebar tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

@available(macOS 26.0, *)
enum NavigatorTab: String, CaseIterable, HyaloPanelTab {
    case project
    case buffers
    case sourceControl
    case search

    var id: String { rawValue }

    var title: String {
        switch self {
        case .project: return "Project"
        case .buffers: return "Buffers"
        case .sourceControl: return "Source Control"
        case .search: return "Search"
        }
    }

    var systemImage: String {
        switch self {
        case .project: return "folder"
        case .buffers: return "doc.on.doc"
        case .sourceControl: return "arrow.triangle.branch"
        case .search: return "magnifyingglass"
        }
    }

    var body: some View {
        switch self {
        case .project: ProjectNavigatorView()
        case .buffers: BufferListView()
        case .sourceControl: SourceControlNavigatorView()
        case .search: FindNavigatorView()
        }
    }
}
