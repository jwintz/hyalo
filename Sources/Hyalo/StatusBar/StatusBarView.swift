// StatusBarView.swift - Bottom status bar with emacs modeline segments
// Target: macOS 26 Tahoe with Liquid Glass design
// Transparent background — vibrancy comes from the parent EffectView.
// Shows: cursor | mode | minor modes | spacer | encoding | line-ending | indent | utility toggle

import SwiftUI

@available(macOS 26.0, *)
struct StatusBarView: View {
    @Bindable var viewModel: StatusBarViewModel
    @Bindable var workspace: HyaloWorkspaceState

    @Environment(\.controlActiveState)
    private var controlActive

    @Environment(\.colorScheme)
    private var colorScheme

    private let statusFont = Font.system(size: HyaloDesign.FontSize.body)

    static let height: CGFloat = HyaloDesign.Height.statusBar

    var body: some View {
        ViewThatFits(in: .horizontal) {
            // Full: all segments visible
            statusBarFull
            // Compact: no minor modes
            statusBarCompact
            // Minimal: cursor + mode + utility toggle only
            statusBarMinimal
        }
        .frame(height: Self.height)
        .overlay(alignment: .top) { topDivider }
        .disabled(controlActive == .inactive)
        // No opaque background — inherits parent EffectView vibrancy
    }

    // MARK: - Full Layout (all segments)

    private var statusBarFull: some View {
        HStack(alignment: .center, spacing: 0) {
            leftSection
            Spacer(minLength: 4)
            rightSection
        }
        .frame(maxHeight: .infinity)
    }

    // MARK: - Compact Layout (no minor modes)

    private var statusBarCompact: some View {
        HStack(alignment: .center, spacing: 0) {
            HStack(spacing: HyaloDesign.Spacing.standard) {
                Text(viewModel.cursorPosition)
                    .font(statusFont.monospaced())
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                statusBarDivider()
                Text(viewModel.mode)
                    .font(statusFont)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
            }
            .padding(.leading, HyaloDesign.Padding.horizontal)

            Spacer(minLength: 4)
            rightSection
        }
        .frame(maxHeight: .infinity)
    }

    // MARK: - Minimal Layout (cursor + utility toggle)

    private var statusBarMinimal: some View {
        HStack(alignment: .center, spacing: 0) {
            Text(viewModel.cursorPosition)
                .font(statusFont.monospaced())
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .padding(.leading, HyaloDesign.Padding.horizontal)

            Spacer(minLength: 4)

            utilityToggle
                .padding(.trailing, HyaloDesign.Padding.horizontal)
        }
        .frame(maxHeight: .infinity)
    }

    // MARK: - Shared Sections

    private var leftSection: some View {
        HStack(spacing: HyaloDesign.Spacing.standard) {
            Text(viewModel.cursorPosition)
                .font(statusFont.monospaced())
                .foregroundStyle(.secondary)
                .lineLimit(1)

            statusBarDivider()

            Text(viewModel.mode)
                .font(statusFont)
                .foregroundStyle(.secondary)
                .lineLimit(1)

            if !viewModel.minorModes.isEmpty {
                statusBarDivider()
                HStack(spacing: 4) {
                    ForEach(viewModel.minorModes, id: \.self) { mode in
                        if mode.containsNerdIconGlyph {
                            Text(mode)
                                .font(.custom(
                                    "Symbols Nerd Font Mono",
                                    size: HyaloDesign.FontSize.large
                                ))
                                .foregroundStyle(.tertiary)
                        } else {
                            Text(mode)
                                .font(statusFont)
                                .foregroundStyle(.tertiary)
                        }
                    }
                }
                .lineLimit(1)
            }
        }
        .padding(.leading, HyaloDesign.Padding.horizontal)
    }

    private var rightSection: some View {
        HStack(spacing: HyaloDesign.Spacing.standard) {
            statusBarButton(viewModel.encoding) {
                viewModel.onEncodingChange?(viewModel.encoding)
            }
            statusBarDivider()
            statusBarButton(viewModel.lineEnding) {
                viewModel.onLineEndingChange?(viewModel.lineEnding)
            }
            statusBarDivider()
            statusBarButton(viewModel.indentDescription) {
                viewModel.onIndentStyleChange?(viewModel.indentStyle, viewModel.indentWidth)
            }
            statusBarDivider()
            utilityToggle
        }
        .padding(.trailing, HyaloDesign.Padding.horizontal)
    }

    private var utilityToggle: some View {
        Button {
            withAnimation(.easeInOut(duration: 0.15)) {
                workspace.utilityAreaVisible.toggle()
            }
        } label: {
            Image(systemName: "rectangle.bottomthird.inset.filled")
                .font(.system(size: HyaloDesign.FontSize.body))
                .foregroundStyle(workspace.utilityAreaVisible ? .primary : .secondary)
                .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .help(workspace.utilityAreaVisible ? "Hide Utility Area" : "Show Utility Area")
    }

    private var topDivider: some View {
        Divider()
            .overlay(Color(nsColor: colorScheme == .dark ? .black : .clear))
    }

    // MARK: - Helpers

    private func statusBarButton(_ text: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Text(text)
                .font(statusFont)
                .foregroundStyle(.secondary)
                .lineLimit(1)
        }
        .buttonStyle(.plain)
    }

    private func statusBarDivider() -> some View {
        Divider()
            .frame(maxHeight: 12)
    }
}

// MARK: - Nerd Font Glyph Detection

private extension String {
    /// Whether this string contains any Nerd Fonts Private Use Area glyphs.
    /// Nerd Fonts use PUA ranges: U+E000–U+F8FF (BMP PUA),
    /// U+F0000–U+FFFFD (Supplementary PUA-A), U+100000–U+10FFFD (Supplementary PUA-B).
    var containsNerdIconGlyph: Bool {
        unicodeScalars.contains { scalar in
            (0xE000...0xF8FF).contains(scalar.value) ||
            (0xF0000...0xFFFFD).contains(scalar.value) ||
            (0x100000...0x10FFFD).contains(scalar.value)
        }
    }
}
