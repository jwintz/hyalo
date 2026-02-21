// HyaloNavigationLayout.swift - NavigationSplitView layout
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Replaces HyaloSplitViewController with a SwiftUI NavigationSplitView,
// following the emacs.d NavigationSidebarLayout.swift pattern.
// Provides: sidebar (navigator), detail (editor + inspector),
// vibrancy background stack.
//
// REFACTORED (AUDIT.md #2, #5):
// - Replaced bidirectional onChange with computed Binding
// - Replaced GeometryReader width tracking with preference keys

import AppKit
import SwiftUI

// MARK: - Preference Keys for Width Tracking (AUDIT.md #2)

private struct NavigatorWidthKey: PreferenceKey {
    static let defaultValue: CGFloat = 280
    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
        value = nextValue()
    }
}

private struct InspectorWidthKey: PreferenceKey {
    static let defaultValue: CGFloat = 300
    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
        value = nextValue()
    }
}

@available(macOS 26.0, *)
struct HyaloNavigationLayout: View {
    @Bindable var workspace: HyaloWorkspaceState
    let emacsView: NSView

    var editorTabViewModel: EditorTabViewModel?
    var utilityAreaViewModel: UtilityAreaViewModel?

    /// Owns the column visibility state so NavigationSplitView starts
    /// collapsed.  Synced bidirectionally with workspace.navigatorVisible.
    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly

    /// Suppresses animation on the initial workspace -> columnVisibility sync.
    @State private var didAppear = false

    /// Binding for inspector visibility with a fast animation
    private var inspectorVisibleBinding: Binding<Bool> {
        Binding(
            get: { workspace.inspectorVisible },
            set: { newValue in
                withAnimation(.easeInOut(duration: 0.15)) {
                    workspace.inspectorVisible = newValue
                }
            }
        )
    }

    // MARK: - Vibrancy Material Mapping

    private var effectMaterialForState: NSVisualEffectView.Material {
        switch workspace.vibrancyMaterial {
        case .none: return .windowBackground
        case .ultraThick: return .headerView
        case .thick: return .titlebar
        case .regular: return .menu
        case .thin: return .popover
        case .ultraThin: return .hudWindow
        }
    }

    // MARK: - Body

    var body: some View {
        NavigationSplitView(columnVisibility: $columnVisibility) {
            // Sidebar: NavigatorAreaView
            NavigatorAreaView(workspace: workspace)
                .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
                // Track width via preference key (AUDIT.md #2)
                .background {
                    GeometryReader { geo in
                        Color.clear
                            .preference(key: NavigatorWidthKey.self, value: geo.size.width)
                    }
                }
        } detail: {
            // Center: MainContentView with .inspector
            MainContentView(
                workspace: workspace,
                emacsView: emacsView,
                editorTabViewModel: editorTabViewModel,
                utilityAreaViewModel: utilityAreaViewModel
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .inspector(isPresented: inspectorVisibleBinding) {
                InspectorAreaView(workspace: workspace)
                    .inspectorColumnWidth(min: 242, ideal: 300, max: 600)
                    // Track width via preference key (AUDIT.md #2)
                    .background {
                        GeometryReader { geo in
                            Color.clear
                                .preference(key: InspectorWidthKey.self, value: geo.size.width)
                        }
                    }
            }
        }
        .navigationSplitViewStyle(.balanced)
        .toolbarBackgroundVisibility(workspace.decorationsVisible ? .visible : .hidden, for: .windowToolbar)
        .toolbarTitleDisplayMode(.inline)
        .toolbar {
            // Branch picker — positioned at leading edge next to traffic lights
            ToolbarItem(placement: .navigation) {
                BranchPickerView(viewModel: ToolbarManager.shared.viewModel)
            }
            .sharedBackgroundVisibility(.hidden)

            // Environment pill — centered in the toolbar via .principal placement.
            // SwiftUI .toolbar does not support expandable items; .principal
            // gives the best centered positioning (same as Xcode/CodeEdit).
            // No ControlGroup wrapper: the Capsule clip in EnvironmentPillView
            // provides its own Liquid Glass surface and sizing.
            // Dynamic width: hug content with minimum for usability.
            ToolbarItem(placement: .principal) {
                EnvironmentPillView(workspace: workspace)
                    .frame(minWidth: 120)
                    .fixedSize(horizontal: true, vertical: false)
            }

            // Push trailing items to the right
            ToolbarSpacer(.flexible)

            // Keycast pill — trailing, compact glass capsule.
            // ControlGroup sizes to content in Liquid Glass.
            ToolbarItem {
                ControlGroup {
                    KeycastView(viewModel: ToolbarManager.shared.viewModel)
                }
            }

            // Package manager — its own glass group
            ToolbarItem {
	        ControlGroup {
                    PackageManagerView(viewModel: ToolbarManager.shared.viewModel)
		}
                .fixedSize()
            }

            // Visual break between package manager and inspector
            ToolbarSpacer(.fixed)

            // Inspector toggle — its own glass group
            ToolbarItem {
	        ControlGroup {
                    Button {
                        withAnimation(.easeInOut(duration: 0.15)) {
                            workspace.inspectorVisible.toggle()
                        }
                    } label: {
                        Image(systemName: "sidebar.right")
                    }
                }
                .fixedSize()
                .help(workspace.inspectorVisible ? "Hide Inspector" : "Show Inspector")
            }

            // Visual break between package manager and keycast
            ToolbarSpacer(.fixed)
        }
        
        // Background after toolbar
        .background {
            ZStack {
                VibrancyBackgroundView(
                    material: effectMaterialForState,
                    blendingMode: .behindWindow,
                    isActive: workspace.vibrancyMaterial != .none
                )
                Color(nsColor: workspace.backgroundColor)
                    .opacity(Double(workspace.backgroundAlpha))
            }
            .ignoresSafeArea()
        }
        // Handle width preference changes (AUDIT.md #2)
        .onPreferenceChange(NavigatorWidthKey.self) { width in
            workspace.navigatorWidth = width
        }
        .onPreferenceChange(InspectorWidthKey.self) { width in
            workspace.inspectorWidth = width
        }
        .onAppear {
            // Force column visibility to match workspace without animation.
            // NavigationSplitView may override .detailOnly on first layout;
            // setting it here in a non-animated transaction prevents a flash.
            var transaction = Transaction()
            transaction.disablesAnimations = true
            withTransaction(transaction) {
                columnVisibility = workspace.navigatorVisible ? .all : .detailOnly
            }
            // Mark appearance complete after the first layout settles,
            // allowing subsequent changes to animate normally.
            DispatchQueue.main.async { didAppear = true }
        }
        // Sync workspace.navigatorVisible -> columnVisibility
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
        // Sync columnVisibility -> workspace.navigatorVisible
        // Guarded by didAppear to ignore NavigationSplitView's
        // internal layout adjustments during initial setup.
        .onChange(of: columnVisibility) { _, newValue in
            guard didAppear else { return }
            let isVisible = (newValue == .all || newValue == .doubleColumn)
            if workspace.navigatorVisible != isVisible {
                workspace.navigatorVisible = isVisible
            }
        }
    }
}
