// EditorTabBarView.swift - Editor tab bar
// Target: macOS 26 Tahoe with Liquid Glass design
// Glass capsule background, overlapping dividers.

import SwiftUI

@available(macOS 26.0, *)
struct EditorTabBarView: View {
    @Bindable var viewModel: EditorTabViewModel
    @Bindable var workspace: HyaloWorkspaceState

    var body: some View {
        VStack(spacing: 0) {
            Divider()
            HStack(spacing: 0) {
                // Leading accessories: Back/Forward navigation
                HStack(spacing: 4) {
                    Button { viewModel.onNavigateBack?() } label: {
                        Image(systemName: "chevron.left")
                            .font(.system(size: 11, weight: .medium))
                    }
                    .buttonStyle(TabNavigationButtonStyle())
                    .disabled(viewModel.tabs.isEmpty)

                    Button { viewModel.onNavigateForward?() } label: {
                        Image(systemName: "chevron.right")
                            .font(.system(size: 11, weight: .medium))
                    }
                    .buttonStyle(TabNavigationButtonStyle())
                    .disabled(viewModel.tabs.isEmpty)
                }
                .padding(.leading, 8)
                .padding(.trailing, 4)

                PanelDivider()

                // Tabs
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: 0) {
                        ForEach(Array(viewModel.tabs.enumerated()), id: \.element.id) { index, tab in
                            let isSelected = tab.id == viewModel.selectedTabId

                            EditorTabItemView(
                                tab: tab,
                                isSelected: isSelected,
                                onSelect: { viewModel.selectTab(tab) },
                                onClose: { viewModel.closeTab(tab) }
                            )

                            // Divider between non-selected tabs
                            if !isSelected && index < viewModel.tabs.count - 1 {
                                let nextIsSelected = viewModel.tabs[index + 1].id == viewModel.selectedTabId
                                if !nextIsSelected {
                                    EditorTabDivider()
                                }
                            }
                        }
                    }
                    .padding(.vertical, 0)
                }

                Spacer()
            }
            .frame(height: HyaloDesign.Height.tabBar)
            Divider()
        }
    }
}

// MARK: - Navigation Button Style

@available(macOS 26.0, *)
private struct TabNavigationButtonStyle: ButtonStyle {
    @Environment(\.isEnabled) private var isEnabled

    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .foregroundStyle(isEnabled ? Color.primary.opacity(0.8) : Color.secondary.opacity(0.4))
            .frame(width: 22, height: 22)
            .background(configuration.isPressed ? Color.primary.opacity(0.1) : Color.clear)
            .clipShape(.rect(cornerRadius: 4))
            .contentShape(Rectangle())
            .opacity(isEnabled ? 1.0 : 0.5)
    }
}

// MARK: - Tab Divider

@available(macOS 26.0, *)
private struct EditorTabDivider: View {
    var body: some View {
        Rectangle()
            .frame(width: 1)
            .padding(.vertical, 8)
            .foregroundStyle(Color.primary.opacity(0.12))
    }
}

// MARK: - Tab Item

@available(macOS 26.0, *)
private struct EditorTabItemView: View {
    let tab: EditorTab
    let isSelected: Bool
    let onSelect: () -> Void
    let onClose: () -> Void

    @State private var isHovering = false
    @Environment(\.colorTheme) private var theme

    var body: some View {
        Button(action: onSelect) {
            HStack(spacing: HyaloDesign.Spacing.tight) {
                Image(systemName: tab.icon ?? "doc.text")
                    .font(.system(size: HyaloDesign.FontSize.caption))
                    .foregroundStyle(isSelected ? .primary : .secondary)

                Text(tab.name)
                    .font(.system(size: HyaloDesign.FontSize.body))
                    .lineLimit(1)
                    .foregroundStyle(isSelected ? .primary : .secondary)

                // Fixed-width trailing accessory container to prevent tab resizing
                ZStack {
                    if tab.isModified {
                        Circle()
                            .fill(theme.accent)
                            .frame(width: 7, height: 7)
                    } else {
                        Button(action: onClose) {
                            Image(systemName: "xmark")
                                .font(.system(size: 7, weight: .bold))
                                .foregroundStyle(.tertiary)
                                .frame(width: 14, height: 14)
                                .background(isHovering ? Color.primary.opacity(0.08) : Color.clear)
                                .clipShape(Circle())
                        }
                        .buttonStyle(.plain)
                        .opacity(isHovering ? 1 : 0)
                    }
                }
                .frame(width: 16)
            }
            .padding(.horizontal, 10)
            .padding(.vertical, 4)
            .background(isSelected ? Color.primary.opacity(0.08) : Color.clear)
            .clipShape(.capsule)
            .contentShape(.capsule)
        }
        .buttonStyle(.plain)
        .padding(.vertical, 2)
        .padding(.horizontal, 2)
        .onHover { isHovering = $0 }
    }
}
