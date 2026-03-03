#if canImport(UIKit)
// HyaloiOSNavigationLayout.swift - iPad shell using KelyphosShellView
// Target: iPadOS 26

import SwiftUI
import HyaloShared
import KelyphosKit

// MARK: - Root View

@available(iOS 26.0, *)
public struct HyaloRootView: View {
    @State private var module = HyaloiOSModule.shared
    @Environment(\.scenePhase) private var scenePhase

    public init() {}

    public var body: some View {
        Group {
            switch module.lifecycle.state {
            case .idle, .starting, .bootstrapping:
                HyaloLoadingView(lifecycle: module.lifecycle)
            case .running:
                HyaloiOSShellView(module: module)
            case .failed(let message):
                VStack(spacing: 12) {
                    Image(systemName: "exclamationmark.triangle")
                        .font(.largeTitle)
                        .foregroundStyle(.red)
                    Text("Emacs Failed to Start")
                        .font(.headline)
                    Text(message)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
        }
        .onAppear { module.start() }
        .onChange(of: scenePhase) { _, phase in
            switch phase {
            case .active:
                module.lifecycle.resume()
            case .background:
                module.lifecycle.suspend()
                module.shellState.saveAppearance()
            case .inactive:
                break
            @unknown default:
                break
            }
        }
        .preferredColorScheme({
            switch module.shellState.windowAppearance {
            case "light": return .light
            case "dark": return .dark
            default: return nil
            }
        }())
    }
}

// MARK: - Shell View

@available(iOS 26.0, *)
private struct HyaloiOSShellView: View {
    let module: HyaloiOSModule

    var body: some View {
        KelyphosShellView(
            state: module.shellState,
            configuration: KelyphosShellConfiguration(
                navigatorTabs: NavigatorTab.allCases.map { $0 },
                inspectorTabs: InspectorTab.allCases.map { $0 },
                utilityTabs: UtilityAreaTab.allCases.map { $0 },
                scrollable: false,
                leadingToolbar: {
                    AnyView(HStack(spacing: 8) {
                        BranchPickerView(viewModel: ToolbarManager.shared.viewModel)
                            .frame(minWidth: 80, maxWidth: 200)
                            .buttonStyle(.plain)
                    })
                },
                principalToolbar: {
                    AnyView(EnvironmentPillView(workspace: module.workspace))
                },
                trailingToolbarPrefix: {
                    AnyView(HStack(spacing: 0) {
                        KeycastView(viewModel: ToolbarManager.shared.viewModel)
                        PackageManagerView(viewModel: ToolbarManager.shared.viewModel)
                    })
                },
                detail: {
                    AnyView(HyaloiOSDetailView(module: module))
                }
            )
        )
        .environment(module.workspace)
        .environment(\.navigatorManager, NavigatorManager.shared)
        .environment(\.searchViewModel, NavigatorManager.shared.searchViewModel)
        .environment(\.bufferListViewModel, NavigatorManager.shared.bufferListViewModel)
        .environment(\.projectNavigatorViewModel, NavigatorManager.shared.projectNavigatorViewModel)
        .environment(\.sourceControlViewModel, NavigatorManager.shared.sourceControlViewModel)
        .environment(\.inspectorViewModel, InspectorManager.shared.viewModel)
        .environment(\.inspectorManager, InspectorManager.shared)
        .environment(\.utilityAreaViewModel, module.utilityAreaViewModel)
        .environment(\.colorTheme, module.workspace.colorTheme)
        .environment(\.terminalContent, AnyView(UtilityAreaTerminalViewiOS()))
    }
}

// MARK: - Detail Content

@available(iOS 26.0, *)
private struct HyaloiOSDetailView: View {
    let module: HyaloiOSModule

    @State private var statusBarViewModel = StatusBarManager.shared.viewModel

    var body: some View {
        VStack(spacing: 0) {
            EditorTabBarView(viewModel: module.editorTabViewModel)
            EmacsUIViewRepresentable(emacsView: module.emacsView)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            StatusBarView(viewModel: statusBarViewModel)
        }
    }
}

// MARK: - Loading View

@available(iOS 26.0, *)
struct HyaloLoadingView: View {
    let lifecycle: EmacsLifecycle

    var body: some View {
        VStack(spacing: 16) {
            ProgressView()
                .controlSize(.large)
            Text("Hyalo")
                .font(.largeTitle)
                .fontWeight(.bold)
            Group {
                switch lifecycle.state {
                case .bootstrapping(let progress):
                    Text("Loading \(progress)...")
                case .starting:
                    Text("Starting Emacs...")
                default:
                    Text("Initializing...")
                }
            }
            .font(.subheadline)
            .foregroundStyle(.secondary)
        }
    }
}

#endif // canImport(UIKit)
