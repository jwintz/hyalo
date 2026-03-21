// GodModeView.swift - Toolbar pill showing god-mode state
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Displays the current god-mode editing state as a compact toolbar
// pill.  When god-mode is active, shows a state label (C, M, CM, SPC,
// ##, u) with a tinted background.  When inactive or hidden, the pill
// collapses to a dimmed icon.
//
// This custom toolbar item owns a single outer Liquid Glass capsule.
// It is a standalone item (not in a group) placed before the keycast
// segment in trailingToolbarItems.

import SwiftUI

@available(macOS 26.0, *)
public struct GodModeView: View {
    @Bindable public var viewModel: ToolbarViewModel

    @Environment(\.controlActiveState)
    private var activeState
    @Environment(\.colorTheme)
    private var theme

    private var state: GodModeState {
        viewModel.godModeState
    }

    public var body: some View {
        if viewModel.godModeVisible {
            Group {
                if state.isActive {
                    HStack(spacing: 4) {
                        Image(systemName: state.icon)
                            .font(.system(size: 11, weight: .semibold))

                        Text(state.label)
                            .font(.system(size: 11, weight: .bold, design: .monospaced))
                    }
                    .foregroundStyle(activeState == .inactive ? AnyShapeStyle(.tertiary) : AnyShapeStyle(tintColor))
                } else {
                    Image(systemName: "command.square")
                        .font(.body)
                        .foregroundStyle(.tertiary)
                }
            }
            .padding(.horizontal, 6)
            .padding(.vertical, 6)
            .padding(5)
            .clipShape(Capsule())
            .glassEffect(in: .capsule)
            .fixedSize()
            .contentTransition(.opacity)
            .animation(.easeInOut(duration: 0.15), value: state)
        }
    }

    public init(viewModel: ToolbarViewModel) {
        self.viewModel = viewModel
    }

    /// Tint color per state — provides at-a-glance differentiation.
    private var tintColor: Color {
        switch state {
        case .inactive:          return theme.foregroundDim
        case .control:           return theme.accent
        case .literal:           return theme.string
        case .meta:              return theme.accentSecondary
        case .controlMeta:       return theme.link
        case .digitArgument:     return theme.warning
        case .universalArgument: return theme.success
        }
    }
}
