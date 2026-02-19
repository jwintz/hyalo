// HyaloPanelView.swift - Reusable panel container with tab bar
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

@available(macOS 26.0, *)
struct HyaloPanelView<Tab: HyaloPanelTab>: View {
    @Binding var selectedTab: Tab?
    @Binding var tabItems: [Tab]
    var tabBarPosition: HyaloTabBarPosition
    var darkDivider: Bool = false

    @Environment(\.colorScheme)
    private var colorScheme

    var body: some View {
        VStack(spacing: 0) {
            if let selectedTab {
                selectedTab
            } else {
                HyaloContentUnavailableView("No Selection")
            }
        }
        .safeAreaInset(edge: .leading, spacing: 0) {
            if tabBarPosition == .side {
                HStack(spacing: 0) {
                    WorkspacePanelTabBar(
                        items: $tabItems,
                        selection: $selectedTab,
                        position: .side
                    )
                    Divider()
                        .overlay(dividerColor)
                }
            }
        }
        .safeAreaInset(edge: .top, spacing: 0) {
            if tabBarPosition == .top {
                VStack(spacing: 0) {
                    Divider()
                    WorkspacePanelTabBar(
                        items: $tabItems,
                        selection: $selectedTab,
                        position: .top
                    )
                    Divider()
                }
                .background(.bar)
            } else if !darkDivider {
                Divider()
            }
        }
    }

    private var dividerColor: Color {
        if darkDivider && colorScheme == .dark {
            return Color.black
        }
        return Color.clear
    }
}
