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

@available(macOS 26.0, iOS 26.0, *)
private struct SourceControlViewModelKey: @preconcurrency EnvironmentKey {
    @MainActor static let defaultValue: SourceControlViewModel? = nil
}

// MARK: - Navigator Manager (for callbacks only)

@available(macOS 26.0, iOS 26.0, *)
private struct NavigatorManagerKey: @preconcurrency EnvironmentKey {
    @MainActor static let defaultValue: NavigatorManager? = nil
}

// MARK: - Inspector View Model

private struct InspectorViewModelKey: EnvironmentKey {
    static let defaultValue: InspectorViewModel? = nil
}

@available(macOS 26.0, iOS 26.0, *)
private struct InspectorManagerKey: @preconcurrency EnvironmentKey {
    @MainActor static let defaultValue: InspectorManager? = nil
}

// MARK: - Utility Area View Model

@available(macOS 26.0, iOS 26.0, *)
private struct UtilityAreaViewModelKey: @preconcurrency EnvironmentKey {
    @MainActor static let defaultValue: UtilityAreaViewModel? = nil
}

// MARK: - Color Theme

@available(macOS 26.0, iOS 26.0, *)
private struct ColorThemeKey: @preconcurrency EnvironmentKey {
    @MainActor static let defaultValue: HyaloColorTheme = HyaloColorTheme()
}

// MARK: - Environment Values Extensions

public extension EnvironmentValues {
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

    @available(macOS 26.0, iOS 26.0, *)
    var sourceControlViewModel: SourceControlViewModel? {
        get { self[SourceControlViewModelKey.self] }
        set { self[SourceControlViewModelKey.self] = newValue }
    }

    @available(macOS 26.0, iOS 26.0, *)
    var utilityAreaViewModel: UtilityAreaViewModel? {
        get { self[UtilityAreaViewModelKey.self] }
        set { self[UtilityAreaViewModelKey.self] = newValue }
    }

    @available(macOS 26.0, iOS 26.0, *)
    var navigatorManager: NavigatorManager? {
        get { self[NavigatorManagerKey.self] }
        set { self[NavigatorManagerKey.self] = newValue }
    }

    @available(macOS 26.0, iOS 26.0, *)
    var inspectorManager: InspectorManager? {
        get { self[InspectorManagerKey.self] }
        set { self[InspectorManagerKey.self] = newValue }
    }

    @available(macOS 26.0, iOS 26.0, *)
    var colorTheme: HyaloColorTheme {
        get { self[ColorThemeKey.self] }
        set { self[ColorThemeKey.self] = newValue }
    }
}
