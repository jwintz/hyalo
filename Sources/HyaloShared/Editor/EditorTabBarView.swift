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
        }
        .fixedSize(horizontal: false, vertical: true)
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
            .padding(.horizontal, 4)
            .frame(maxHeight: .infinity)
        }
        .glassEffect(in: .capsule)
        .fixedSize(horizontal: true, vertical: false)
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
        // GlassEffectContainer + glassEffect are OUTSIDE the ScrollView so the
        // pill is always full-width.  The ScrollView sits inside the pill and
        // clips its content — tabs keep their natural width and scroll within.
        GlassEffectContainer(spacing: 0) {
            ScrollView(.horizontal, showsIndicators: false) {
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
                        .fixedSize(horizontal: true, vertical: false)
                    }
                }
                .padding(3)
            }
            .frame(maxWidth: .infinity)
        }
        .glassEffect(in: .capsule)
        .frame(maxWidth: .infinity)
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
        // ZStack: select button fills the full item; close button overlaid at
        // trailing edge so both have independent, non-nested hit areas.
        ZStack(alignment: .trailing) {
            // Select button — full item width
            Button(action: onSelect) {
                HStack(spacing: KelyphosDesign.Spacing.tight) {
                    Image(systemName: tab.icon ?? "doc.text")
                        .font(.system(size: KelyphosDesign.FontSize.caption))
                        .foregroundStyle(isSelected ? .primary : .secondary)

                    Text(tab.name)
                        .font(.system(size: KelyphosDesign.FontSize.body))
                        .lineLimit(1)
                        .foregroundStyle(isSelected ? .primary : .secondary)

                    // Reserve trailing space for dot / close button
                    Color.clear.frame(width: 16)
                }
                .padding(.horizontal, 10)
                .padding(.vertical, 5)
                .background {
                    if isSelected || isHovering {
                        Capsule()
                            .fill(isSelected ? .regularMaterial : .thinMaterial)
                            .shadow(color: .primary.opacity(isSelected ? 0.12 : 0), radius: 1, y: 0.5)
                    }
                }
                .contentShape(Capsule())
            }
            .buttonStyle(.plain)
            .glassEffectID(tab.id, in: namespace)

            // Trailing indicator: modified dot or close button
            ZStack {
                if tab.isModified {
                    Circle()
                        .fill(theme.accent)
                        .frame(width: 7, height: 7)
                } else if isHovering {
                    Button(action: onClose) {
                        Image(systemName: "xmark")
                            .font(.system(size: 7, weight: .bold))
                            .foregroundStyle(.secondary)
                            .frame(width: 14, height: 14)
                            .background(Color.primary.opacity(0.08))
                            .clipShape(Circle())
                    }
                    .buttonStyle(.plain)
                    .accessibilityLabel("Close \(tab.name)")
                }
            }
            .frame(width: 16)
            .padding(.trailing, 10)
        }
        .onHover { isHovering = $0 }
        .focusable(false)
        .accessibilityElement(children: .combine)
        .accessibilityLabel("Tab: \(tab.name)")
        .accessibilityAddTraits(isSelected ? .isSelected : [])
    }
}
