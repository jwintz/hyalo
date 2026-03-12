// EditorTabBarView.swift - Editor tab bar with Liquid Glass design
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Height matches KelyphosPanelTabBar:
//   .glassEffect(in: .capsule) on GlassEffectContainer (outside)
//   Outer padding: .horizontal 8, .vertical 4

import SwiftUI
import KelyphosKit

@available(macOS 26.0, *)
public struct EditorTabBarView: View {
    @Bindable public var viewModel: EditorTabViewModel

    @Environment(\.colorTheme) private var theme

    public init(viewModel: EditorTabViewModel) {
        self.viewModel = viewModel
    }

    public var body: some View {
        HStack(spacing: KelyphosDesign.Spacing.tight) {
            chevronPill
            activeBufferLabel
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

    // MARK: - Active Buffer Label

    private var activeBufferLabel: some View {
        GlassEffectContainer(spacing: 0) {
            HStack(spacing: KelyphosDesign.Spacing.tight) {
                if let tab = viewModel.selectedTab {
                    Image(systemName: tab.icon ?? "doc.text")
                        .font(.system(size: KelyphosDesign.FontSize.caption))
                        .foregroundStyle(.primary)

                    Text(tab.name)
                        .font(.system(size: KelyphosDesign.FontSize.body))
                        .lineLimit(1)
                        .foregroundStyle(.primary)

                    if tab.isModified {
                        Circle()
                            .fill(theme.accent)
                            .frame(width: 7, height: 7)
                    }
                } else if let id = viewModel.selectedTabId, !id.isEmpty {
                    Image(systemName: "doc.text")
                        .font(.system(size: KelyphosDesign.FontSize.caption))
                        .foregroundStyle(.primary)

                    Text(id)
                        .font(.system(size: KelyphosDesign.FontSize.body))
                        .lineLimit(1)
                        .foregroundStyle(.primary)
                } else {
                    Image(systemName: "sparkles")
                        .font(.system(size: KelyphosDesign.FontSize.caption))
                        .foregroundStyle(.secondary)

                    Text("Splash")
                        .font(.system(size: KelyphosDesign.FontSize.body))
                        .lineLimit(1)
                        .foregroundStyle(.secondary)
                }

                Spacer()
            }
            .padding(.vertical, 5)
            .padding(.horizontal, 10)
            .padding(3)
            .frame(maxWidth: .infinity)
        }
        .glassEffect(in: .capsule)
        .frame(maxWidth: .infinity)
    }
}
