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

    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly
    @State private var didAppear = false

    var body: some View {
        NavigationSplitView(columnVisibility: $columnVisibility) {
            NavigatorAreaView(workspace: workspace)
                .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
        } detail: {
            // Editor area with Emacs view
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
                        workspace: workspace
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
        }
        .navigationSplitViewStyle(.balanced)
        .toolbar {
            ToolbarItem(placement: .topBarLeading) {
                BranchPickerView(viewModel: ToolbarManager.shared.viewModel)
            }
            ToolbarItem(placement: .principal) {
                EnvironmentPillView(workspace: workspace)
                    .frame(minWidth: 120)
                    .fixedSize(horizontal: true, vertical: false)
            }
            ToolbarItemGroup(placement: .topBarTrailing) {
                Button {
                    withAnimation(.easeInOut(duration: 0.15)) {
                        workspace.inspectorVisible.toggle()
                    }
                } label: {
                    Image(systemName: "sidebar.right")
                }

                Button {
                    withAnimation(.easeInOut(duration: 0.15)) {
                        workspace.utilityAreaVisible.toggle()
                    }
                } label: {
                    Image(systemName: "square.bottomthird.inset.filled")
                }
            }
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
}

// MARK: - Root View

/// Root view for the Hyalo iOS app.
/// Shows a loading screen until Emacs is ready, then the full IDE layout.
@available(iOS 26.0, *)
public struct HyaloRootView: View {
    @State private var module = HyaloiOSModule.shared

    public init() {}

    public var body: some View {
        Group {
            switch module.lifecycle.state {
            case .idle, .starting, .bootstrapping:
                HyaloLoadingView(lifecycle: module.lifecycle)
            case .running:
                HyaloiOSNavigationLayout(
                    workspace: module.workspace,
                    emacsView: nil // TODO: Receive from iosterm.m
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
