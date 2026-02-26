#if canImport(UIKit)
// HyaloiOSNavigationLayout.swift - iPad NavigationSplitView layout
// Mirrors macOS HyaloNavigationLayout with iPad-appropriate toolbar and materials.

import SwiftUI
import HyaloShared

@available(iOS 26.0, *)
struct HyaloiOSNavigationLayout: View {
    @Bindable var workspace: HyaloWorkspaceState
    let emacsView: UIView?

    var editorTabViewModel: EditorTabViewModel?
    var utilityAreaViewModel: UtilityAreaViewModel?

    @Binding var showCommandPalette: Bool
    @Binding var showOpenQuickly: Bool

    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly
    @State private var didAppear = false

    var body: some View {
        NavigationSplitView(columnVisibility: $columnVisibility) {
            NavigatorAreaView(workspace: workspace)
                .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
        } detail: {
            detailContent
        }
        .navigationSplitViewStyle(.balanced)
        .sheet(isPresented: $showOpenQuickly) {
            OpenQuicklyView(
                viewModel: HyaloiOSModule.shared.openQuicklyViewModel,
                onClose: { showOpenQuickly = false },
                openFile: { item in
                    NavigatorManager.shared.setActiveFile(item.path)
                    HyaloiOSModule.shared.onOpenFile?(item.path)
                    showOpenQuickly = false
                }
            )
            .presentationDetents([.medium, .large])
        }
        .sheet(isPresented: $showCommandPalette) {
            CommandPaletteView(
                viewModel: HyaloiOSModule.shared.commandPaletteViewModel,
                onClose: { showCommandPalette = false },
                executeCommand: { command in
                    HyaloiOSModule.shared.onExecuteCommand?(command.name)
                    showCommandPalette = false
                }
            )
            .presentationDetents([.medium, .large])
        }
        .background(.regularMaterial)
        .onAppear {
            var transaction = Transaction()
            transaction.disablesAnimations = true
            withTransaction(transaction) {
                columnVisibility = workspace.navigatorVisible ? .all : .detailOnly
            }
            DispatchQueue.main.async { didAppear = true }
        }
        .onChange(of: workspace.navigatorVisible) { _, isVisible in
            let target: NavigationSplitViewVisibility = isVisible ? .all : .detailOnly
            if columnVisibility != target {
                if didAppear {
                    withAnimation(.easeInOut(duration: 0.15)) {
                        columnVisibility = target
                    }
                } else {
                    var transaction = Transaction()
                    transaction.disablesAnimations = true
                    withTransaction(transaction) {
                        columnVisibility = target
                    }
                }
            }
        }
        .onChange(of: columnVisibility) { _, newValue in
            guard didAppear else { return }
            let isVisible = (newValue == .all || newValue == .doubleColumn)
            if workspace.navigatorVisible != isVisible {
                workspace.navigatorVisible = isVisible
            }
        }
    }

    // MARK: - Detail Content

    private var detailContent: some View {
        VStack(spacing: 0) {
            if let editorTabViewModel {
                EditorTabBarView(
                    viewModel: editorTabViewModel,
                    workspace: workspace
                )
            }

            EmacsUIViewRepresentable(emacsView: emacsView)
                .frame(maxWidth: .infinity, maxHeight: .infinity)

            if let utilityAreaViewModel, workspace.utilityAreaVisible {
                Divider()
                UtilityAreaView(
                    viewModel: utilityAreaViewModel,
                    workspace: workspace,
                    terminalContent: { AnyView(UtilityAreaTerminalViewiOS()) }
                )
                .frame(height: workspace.utilityAreaHeight)
            }

            StatusBarView(
                viewModel: StatusBarManager.shared.viewModel,
                workspace: workspace
            )
        }
        .inspector(isPresented: $workspace.inspectorVisible) {
            InspectorAreaView(workspace: workspace)
                .inspectorColumnWidth(min: 242, ideal: 300, max: 600)
        }
        .toolbar(removing: .sidebarToggle)
        .navigationBarTitleDisplayMode(.inline)
        .toolbarVisibility(.visible, for: .navigationBar)
        .toolbar {
            ToolbarItem(placement: .topBarLeading) {
                HStack(spacing: 8) {
                    Button {
                        withAnimation(.easeInOut(duration: 0.15)) {
                            workspace.navigatorVisible.toggle()
                        }
                    } label: {
                        Image(systemName: "sidebar.left")
                    }
                    BranchPickerView(viewModel: ToolbarManager.shared.viewModel)
                        .frame(minWidth: 80, maxWidth: 200)
                        .buttonStyle(.plain)
                }
            }
            ToolbarItem(placement: .principal) {
                EnvironmentPillView(workspace: workspace)
            }
            ToolbarItemGroup(placement: .topBarTrailing) {
                KeycastView(viewModel: ToolbarManager.shared.viewModel)
                PackageManagerView(viewModel: ToolbarManager.shared.viewModel)
                Button {
                    withAnimation(.easeInOut(duration: 0.15)) {
                        workspace.inspectorVisible.toggle()
                    }
                } label: {
                    Image(systemName: "sidebar.right")
                }
            }
        }
    }
}

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
                HyaloiOSNavigationLayout(
                    workspace: module.workspace,
                    emacsView: module.emacsView,
                    editorTabViewModel: module.editorTabViewModel,
                    utilityAreaViewModel: module.utilityAreaViewModel,
                    showCommandPalette: $module.showCommandPalette,
                    showOpenQuickly: $module.showOpenQuickly
                )
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
        .onAppear {
            module.start()
        }
        .onChange(of: scenePhase) { _, phase in
            switch phase {
            case .active:
                module.lifecycle.resume()
            case .background:
                module.lifecycle.suspend()
                module.workspace.saveAppearance()
            case .inactive:
                break
            @unknown default:
                break
            }
        }
        .preferredColorScheme({
            switch module.workspace.windowAppearance {
            case "light": return .light
            case "dark": return .dark
            default: return nil
            }
        }())
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
