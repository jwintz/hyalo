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
import HyaloShared

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

            // Keycast pill — trailing. Glass background is owned by KeycastView
            // itself. visibilityPriority .low (-1000): disappears before package
            // (.high = 1000) and inspector (.user = 2000) when space is tight.
            ToolbarItem {
                _KeycastToolbarContent(
                    viewModel: ToolbarManager.shared.viewModel
                )
            }

            // Package manager — bordered single-action button (.buttonStyle(.bordered)
            // is set inside PackageManagerView on its trigger button).
            // visibilityPriority .user: NSToolbar hides this before the inspector.
            ToolbarItem {
                _PackageManagerToolbarContent(
                    viewModel: ToolbarManager.shared.viewModel
                )
            }

            // Visual break between package manager and inspector
            ToolbarSpacer(.fixed)

            // Inspector toggle — plain bordered button anchored to trailing edge.
            // visibilityPriority .user (2000): NSToolbar never auto-overflows this.
            // No ControlGroup: ControlGroup → NSToolbarItemGroup → collapses under
            // space pressure, which is the primary cause of the shrinking pill bug.
            ToolbarItem {
                _InspectorToggleToolbarContent(workspace: workspace)
            }

            // Trailing gap — keeps the inspector button off the window edge.
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

// MARK: - Private toolbar item content views
//
// These are extracted from the .toolbar {} closure to keep generic type
// complexity manageable for @ToolbarContentBuilder inference. Each
// ToolbarItem<Content> needs a concrete Content type; deeply nested
// ModifiedContent chains cause the tuple builder to fail.

@available(macOS 26.0, *)
private struct _KeycastToolbarContent: View {
    let viewModel: ToolbarViewModel

    var body: some View {
        KeycastView(viewModel: viewModel)
            // External left gap: ToolbarSpacer(.fixed) between a flexible spacer
            // and a custom-capsule item produces no visible gap on macOS 26.
            // Padding inside the ToolbarItem frame is reliable.
            .padding(.leading, 8)
            // .low (-1000): keycast overflows before package (.high = 1000) and
            // inspector (.user = 2000). It is informational, not structural.
            .toolbarItemVisibilityPriority(.low)
    }
}

@available(macOS 26.0, *)
private struct _PackageManagerToolbarContent: View {
    let viewModel: ToolbarViewModel

    var body: some View {
        // PackageManagerView uses .buttonStyle(.bordered) internally on its
        // trigger button, giving the Liquid Glass single-action pill look.
        PackageManagerView(viewModel: viewModel)
            .fixedSize()
            // .high (1000): NSToolbar moves this to overflow before the inspector
            // (.user = 2000) when horizontal space is exhausted.
            .toolbarItemVisibilityPriority(.high)
    }
}

@available(macOS 26.0, *)
private struct _InspectorToggleToolbarContent: View {
    @Bindable var workspace: HyaloWorkspaceState

    var body: some View {
        Button {
            withAnimation(.easeInOut(duration: 0.15)) {
                workspace.inspectorVisible.toggle()
            }
        } label: {
            Image(systemName: "sidebar.right")
        }
        .buttonStyle(.bordered)
        .fixedSize()
        .help(workspace.inspectorVisible ? "Hide Inspector" : "Show Inspector")
        // .user (2000): highest built-in priority. NSToolbar never automatically
        // moves this item to overflow. On a non-customizable toolbar this means
        // the inspector toggle is always visible regardless of window width.
        .toolbarItemVisibilityPriority(.user)
    }
}
