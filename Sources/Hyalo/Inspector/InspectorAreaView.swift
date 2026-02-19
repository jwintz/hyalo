// InspectorAreaView.swift - Right sidebar container with tabs
// Target: macOS 26 Tahoe with Liquid Glass design
// Chrome uses system-adaptive colors (not workspace.backgroundColor per AGENTS.md).
// REFACTORED (AUDIT.md #4): Uses @Bindable for view model injection instead of manual bindings

import SwiftUI

@available(macOS 26.0, *)
struct InspectorAreaView: View {
    @Bindable var workspace: HyaloWorkspaceState
    @Bindable var viewModel: InspectorViewModel

    init(workspace: HyaloWorkspaceState, viewModel: InspectorViewModel? = nil) {
        self.workspace = workspace
        self.viewModel = viewModel ?? InspectorManager.shared.viewModel
    }

    var body: some View {
        HyaloPanelView(
            selectedTab: $viewModel.selectedTab,
            tabItems: $viewModel.tabItems,
            tabBarPosition: .top,
            darkDivider: true
        )
        .background {
            Color(nsColor: workspace.backgroundColor)
                .opacity(Double(workspace.backgroundAlpha))
                .ignoresSafeArea()
        }
        .accessibilityElement(children: .contain)
        .accessibilityLabel("inspector")
    }
}
