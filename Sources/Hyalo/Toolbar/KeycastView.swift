// KeycastView.swift - Toolbar pill showing current key binding and command
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Displays the last key binding (monospaced, dimmed) and command name
// in a compact toolbar pill. Auto-fades after a configurable delay.
// Lower display priority — collapses first when the toolbar is narrow.

import SwiftUI

@available(macOS 26.0, *)
struct KeycastView: View {
    @Bindable var viewModel: ToolbarViewModel

    @Environment(\.controlActiveState)
    private var activeState

    /// Fade delay in seconds (matches hyalo-keycast-fade-delay on lisp side)
    private let fadeDelay: TimeInterval = 3.0

    /// Tracks whether the pill content is visible (fades after delay)
    @State private var isContentVisible = false

    /// Timer for auto-fade
    @State private var fadeTimer: Timer?

    var body: some View {
        Group {
            if viewModel.keycastVisible {
                HStack(spacing: 6) {
                    // Key binding — monospaced, secondary
                    Text(viewModel.keycastKey)
                        .font(.system(size: 11, weight: .medium, design: .monospaced))
                        .foregroundStyle(.secondary)

                    // Command name — regular weight
                    Text(viewModel.keycastCommand)
                        .font(.system(size: 11))
                        .foregroundStyle(activeState == .inactive ? .tertiary : .primary)
                }
                .opacity(isContentVisible ? 1.0 : 0.0)
                .animation(.easeInOut(duration: 0.2), value: isContentVisible)
                .onChange(of: viewModel.keycastKey) { _, _ in
                    showAndScheduleFade()
                }
                .onChange(of: viewModel.keycastCommand) { _, _ in
                    showAndScheduleFade()
                }
            }
        }
    }

    private func showAndScheduleFade() {
        isContentVisible = true
        fadeTimer?.invalidate()
        fadeTimer = Timer.scheduledTimer(withTimeInterval: fadeDelay, repeats: false) { _ in
            DispatchQueue.main.async {
                withAnimation {
                    isContentVisible = false
                }
            }
        }
    }
}
