// EnvironmentKeys.swift - SwiftUI environment keys for dependency injection
// Target: macOS 26 Tahoe with Liquid Glass design
// Implements AUDIT.md #4: @Bindable injection via @Environment

import SwiftUI

// MARK: - Navigator View Models

private struct ProjectNavigatorViewModelKey: EnvironmentKey {
    static let defaultValue: ProjectNavigatorViewModel? = nil
}

private struct BufferListViewModelKey: EnvironmentKey {
    static let defaultValue: BufferListViewModel? = nil
}

private struct SearchViewModelKey: EnvironmentKey {
    static let defaultValue: SearchViewModel? = nil
}

// MARK: - Inspector View Model

private struct InspectorViewModelKey: EnvironmentKey {
    static let defaultValue: InspectorViewModel? = nil
}

// MARK: - Color Theme

@available(macOS 26.0, *)
private struct ColorThemeKey: @preconcurrency EnvironmentKey {
    @MainActor static let defaultValue: HyaloColorTheme = HyaloColorTheme()
}

// MARK: - Environment Values Extensions

extension EnvironmentValues {
    var projectNavigatorViewModel: ProjectNavigatorViewModel? {
        get { self[ProjectNavigatorViewModelKey.self] }
        set { self[ProjectNavigatorViewModelKey.self] = newValue }
    }

    var bufferListViewModel: BufferListViewModel? {
        get { self[BufferListViewModelKey.self] }
        set { self[BufferListViewModelKey.self] = newValue }
    }

    var searchViewModel: SearchViewModel? {
        get { self[SearchViewModelKey.self] }
        set { self[SearchViewModelKey.self] = newValue }
    }

    var inspectorViewModel: InspectorViewModel? {
        get { self[InspectorViewModelKey.self] }
        set { self[InspectorViewModelKey.self] = newValue }
    }

    @available(macOS 26.0, *)
    var colorTheme: HyaloColorTheme {
        get { self[ColorThemeKey.self] }
        set { self[ColorThemeKey.self] = newValue }
    }
}
