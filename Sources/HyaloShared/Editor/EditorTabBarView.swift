// EditorTabBarView.swift - Editor tab bar with Liquid Glass design
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Visual design matches KelyphosPanelTabBar:
//   Outer padding: .horizontal 8, .vertical 4  (same as KelyphosPanelTabBar)
//   Chevrons: GlassEffectContainer + glassEffect(in: .capsule)
//   Tabs: GlassEffectContainer + glassEffect(in: .capsule) + glassEffectID per tab

import SwiftUI
import KelyphosKit

@available(macOS 26.0, iOS 26.0, *)
public struct EditorTabBarView: View {
    @Bindable public var viewModel: EditorTabViewModel

    @Namespace private var glassNamespace

    public init(viewModel: EditorTabViewModel) {
        self.viewModel = viewModel
    }

    public var body: some View {
        HStack(spacing: KelyphosDesign.Spacing.tight) {
            chevronPill
            tabScrollArea
            Spacer(minLength: 0)
        }
        .padding(.horizontal, KelyphosDesign.Padding.compact)
        .padding(.vertical, KelyphosDesign.Spacing.tight)
    }

    // MARK: - Back / Forward Pill

    private var chevronPill: some View {
        GlassEffectContainer(spacing: 4) {
            HStack(spacing: 4) {
                chevronButton(
                    systemImage: "chevron.left",
                    action: { viewModel.onNavigateBack?(); wakeEmacs() },
                    label: "Navigate Back"
                )
                chevronButton(
                    systemImage: "chevron.right",
                    action: { viewModel.onNavigateForward?(); wakeEmacs() },
                    label: "Navigate Forward"
                )
            }
            .padding(.horizontal, 6)
            .padding(.vertical, 5)
        }
        .glassEffect(in: .capsule)
        .disabled(viewModel.tabs.isEmpty)
    }

    private func chevronButton(systemImage: String, action: @escaping () -> Void, label: String) -> some View {
        Button(action: action) {
            Image(systemName: systemImage)
                .font(.system(size: 11, weight: .medium))
                .foregroundStyle(.primary.opacity(0.8))
                .frame(width: 18, height: 18)
        }
        .buttonStyle(.plain)
        .focusable(false)
        .accessibilityLabel(label)
    }

    // MARK: - Tab Row

    private var tabScrollArea: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            GlassEffectContainer(spacing: 0) {
                HStack(spacing: 2) {
                    ForEach(viewModel.tabs, id: \.id) { tab in
                        let isSelected = tab.id == viewModel.selectedTabId
                        EditorTabItemView(
                            tab: tab,
                            isSelected: isSelected,
                            namespace: glassNamespace,
                            onSelect: { viewModel.selectTab(tab) },
                            onClose: { viewModel.closeTab(tab) }
                        )
                    }
                }
                .padding(3)
            }
            .glassEffect(in: .capsule)
        }
    }
}

// MARK: - Tab Item

@available(macOS 26.0, iOS 26.0, *)
private struct EditorTabItemView: View {
    let tab: EditorTab
    let isSelected: Bool
    let namespace: Namespace.ID
    let onSelect: () -> Void
    let onClose: () -> Void

    @State private var isHovering = false
    @Environment(\.colorTheme) private var theme

    var body: some View {
        Button(action: onSelect) {
            HStack(spacing: KelyphosDesign.Spacing.tight) {
                Image(systemName: tab.icon ?? "doc.text")
                    .font(.system(size: KelyphosDesign.FontSize.caption))
                    .foregroundStyle(isSelected ? .primary : .secondary)

                Text(tab.name)
                    .font(.system(size: KelyphosDesign.FontSize.body))
                    .lineLimit(1)
                    .foregroundStyle(isSelected ? .primary : .secondary)

                // Fixed-width trailing: modified dot or close button
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
                        .accessibilityLabel("Close \(tab.name)")
                    }
                }
                .frame(width: 16)
            }
            .padding(.horizontal, 10)
            .padding(.vertical, 5)
            .background {
                if isSelected {
                    Capsule()
                        .fill(.regularMaterial)
                        .shadow(color: .primary.opacity(0.12), radius: 1, y: 0.5)
                }
            }
        }
        .buttonStyle(.plain)
        .glassEffectID(tab.id, in: namespace)
        .onHover { isHovering = $0 }
        .focusable(false)
        .accessibilityElement(children: .combine)
        .accessibilityLabel("Tab: \(tab.name)")
        .accessibilityAddTraits(isSelected ? .isSelected : [])
    }
}
