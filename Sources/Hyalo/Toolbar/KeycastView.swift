// KeycastView.swift - Toolbar pill showing current key binding and command
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Displays the last key binding (monospaced, semibold) and command name
// (monospaced, secondary) in a compact toolbar pill. Auto-fades after a
// configurable delay, revealing a keyboard icon placeholder.
//
// When idle, only the placeholder icon is in the view tree so the pill
// shrinks to its minimal size.  When active, the HStack with key and
// command replaces the placeholder.  The ControlGroup wrapper in the
// toolbar provides the Liquid Glass pill shape and clips content.

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

    private var hasContent: Bool {
        !viewModel.keycastKey.isEmpty || !viewModel.keycastCommand.isEmpty
    }

    /// Whether to show the key/command text (not the placeholder)
    private var showContent: Bool {
        isContentVisible && hasContent
    }

    var body: some View {
        if viewModel.keycastVisible {
            Group {
                if showContent {
                    HStack(spacing: 6) {
                        Text(viewModel.keycastKey)
                            .font(.system(size: 11, weight: .semibold, design: .monospaced))
                            .foregroundStyle(activeState == .inactive ? .tertiary : .primary)

                        Text(viewModel.keycastCommand)
                            .font(.system(size: 11, design: .monospaced))
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                    }
                    .padding(.horizontal, 8)
                } else {
                    Image(systemName: "keyboard")
                        .font(.system(size: 11))
                        .foregroundStyle(.tertiary)
                        .frame(minWidth: 20, minHeight: 20)
                        .padding(.horizontal, 8)
                }
            }
            .contentTransition(.opacity)
            .animation(.easeInOut(duration: 0.2), value: showContent)
            .clipped()
            .onChange(of: viewModel.keycastKey) { _, _ in
                showAndScheduleFade()
            }
            .onChange(of: viewModel.keycastCommand) { _, _ in
                showAndScheduleFade()
            }
            .onAppear {
                if hasContent { showAndScheduleFade() }
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
