// NavigatorTab.swift - Navigator sidebar tab definitions
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI
import KelyphosKit

@available(macOS 26.0, *)
public enum NavigatorTab: String, CaseIterable, KelyphosPanel {
    case project
    case buffers
    case search

    public var id: String { rawValue }

    public var title: String {
        switch self {
        case .project: return "Project"
        case .buffers: return "Buffers"
        case .search: return "Search"
        }
    }

    public var systemImage: String {
        switch self {
        case .project: return "folder"
        case .buffers: return "doc.on.doc"
        case .search: return "magnifyingglass"
        }
    }

    public var body: some View {
        switch self {
        case .project: ProjectNavigatorView()
        case .buffers: BufferListView()
        case .search: FindNavigatorView()
        }
    }
}
