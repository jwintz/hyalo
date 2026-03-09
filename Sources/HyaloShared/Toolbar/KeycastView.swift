// KeycastView.swift - Toolbar pill showing current key binding and command
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Displays the last key binding (monospaced, semibold) and command name
// (monospaced, secondary) in a compact toolbar pill. Auto-fades after a
// configurable delay, revealing a keyboard icon placeholder.
//
// When idle, only the placeholder icon is in the view tree so the pill
// shrinks to its minimal size.  When active, the HStack with key and
// command replaces the placeholder.
//
// The pill shape and clipping are owned by this view. The enclosing toolbar
// must NOT wrap this in a ControlGroup — that would create an
// NSToolbarItemGroup at the AppKit layer with compression/collapse behaviour.

import SwiftUI

@available(macOS 26.0, *)
public struct KeycastView: View {
    @Bindable public var viewModel: ToolbarViewModel

    @Environment(\.controlActiveState)
    private var activeState

    /// Fade delay in seconds (matches hyalo-keycast-fade-delay on lisp side)
    private let fadeDelay: TimeInterval = 3.0

    /// Tracks whether the pill content is visible (fades after delay)
    @State private var isContentVisible = false

    /// Task handle for auto-fade cancellation
    @State private var fadeTask: Task<Void, Never>?

    private var hasContent: Bool {
        !viewModel.keycastKey.isEmpty || !viewModel.keycastCommand.isEmpty
    }

    /// Whether to show the key/command text (not the placeholder)
    private var showContent: Bool {
        isContentVisible && hasContent
    }

    public var body: some View {
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
                    .padding(.horizontal, 6)
                } else {
                    Image(systemName: "keyboard")
                        .font(.system(size: 11))
                        .foregroundStyle(.tertiary)
                        .padding(.horizontal, 6)
                }
            }
            .padding(.vertical, 5)
            .contentTransition(.opacity)
            .animation(.easeInOut(duration: 0.2), value: showContent)
            // The pill background and clipping live here, not in a ControlGroup
            // wrapper in the toolbar. ControlGroup bridges to NSToolbarItemGroup
            // which applies compression/collapse under space pressure.
            .glassEffect(in: .capsule)
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

    public init(viewModel: ToolbarViewModel) {
        self.viewModel = viewModel
    }

    private func showAndScheduleFade() {
        isContentVisible = true
        fadeTask?.cancel()
        fadeTask = Task { @MainActor in
            try? await Task.sleep(for: .seconds(fadeDelay))
            guard !Task.isCancelled else { return }
            withAnimation {
                isContentVisible = false
            }
        }
    }
}
