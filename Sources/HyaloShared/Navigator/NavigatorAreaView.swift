// NavigatorAreaView.swift - Left sidebar container with tabs
// Target: macOS 26 Tahoe with Liquid Glass design
// Chrome uses system-adaptive colors (not workspace.backgroundColor per AGENTS.md).
// REFACTORED (AUDIT.md #4): Uses @Bindable for view model injection instead of manual bindings

import SwiftUI

@available(macOS 26.0, iOS 26.0, *)
public struct NavigatorAreaView: View {
    @Bindable public var workspace: HyaloWorkspaceState
    @Bindable public var viewModel: NavigatorViewModel

    public init(workspace: HyaloWorkspaceState, viewModel: NavigatorViewModel? = nil) {
        self.workspace = workspace
        self.viewModel = viewModel ?? NavigatorManager.shared.viewModel
    }

    public var body: some View {
        HyaloPanelView(
            selectedTab: $viewModel.selectedTab,
            tabItems: $viewModel.tabItems,
            tabBarPosition: .top,
            darkDivider: true
        )
        .background {
            Color(platformColor: workspace.backgroundColor)
                .opacity(Double(workspace.backgroundAlpha))
                .ignoresSafeArea()
        }
        .accessibilityElement(children: .contain)
        .accessibilityLabel("navigator")
    }
}
