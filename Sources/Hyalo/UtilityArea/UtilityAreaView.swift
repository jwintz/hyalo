// UtilityAreaView.swift - Collapsible bottom panel with tabs
// Target: macOS 26 Tahoe with Liquid Glass design
// Transparent background — vibrancy comes from the parent EffectView.

import SwiftUI

@available(macOS 26.0, *)
struct UtilityAreaView: View {
    @Bindable var viewModel: UtilityAreaViewModel
    @Bindable var workspace: HyaloWorkspaceState
    @State private var tabItems: [UtilityAreaTab] = UtilityAreaTab.allCases
    @Environment(\.colorTheme) private var theme

    var body: some View {
        VStack(spacing: 0) {
            PanelDivider()

            // Tab bar
            HStack(spacing: 0) {
                ForEach(tabItems) { tab in
                    Button {
                        viewModel.selectedTab = tab
                    } label: {
                        Label(tab.title, systemImage: tab.systemImage)
                            .font(.system(size: HyaloDesign.FontSize.caption))
                            .padding(.horizontal, HyaloDesign.Spacing.compact)
                            .padding(.vertical, 4)
                            .foregroundStyle(viewModel.selectedTab == tab ? theme.accent : .secondary)
                    }
                    .buttonStyle(.plain)
                }
                Spacer()

                // Maximize button
                Button {
                    // No animation — instant layout swap prevents the
                    // utility area from bleeding into the toolbar during
                    // the transition.
                    var t = Transaction()
                    t.disablesAnimations = true
                    withTransaction(t) {
                        viewModel.isMaximized.toggle()
                    }
                } label: {
                    Image(systemName: viewModel.isMaximized ? "arrow.down.right.and.arrow.up.left" : "arrow.up.left.and.arrow.down.right")
                        .font(.system(size: HyaloDesign.FontSize.caption))
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
                .padding(.trailing, HyaloDesign.Spacing.compact)
            }
            .frame(height: HyaloDesign.Height.tabBar)

            PanelDivider()

            // Content
            if let tab = viewModel.selectedTab {
                tabContent(for: tab)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
        }
        .frame(height: viewModel.isMaximized ? nil : viewModel.height)
        .frame(maxHeight: viewModel.isMaximized ? .infinity : nil)
        // No opaque background — inherits parent EffectView vibrancy
    }

    @ViewBuilder
    private func tabContent(for tab: UtilityAreaTab) -> some View {
        switch tab {
        case .terminal:
            // Read palette version in the view body so SwiftUI tracks it
            // and re-creates UtilityAreaTerminalView when the palette changes.
            let _ = TerminalPalette.shared.version
            UtilityAreaTerminalView(holder: viewModel.terminalHolder)
        case .diagnostics:
            DiagnosticsView(viewModel: viewModel.diagnosticsViewModel)
        }
    }
}
