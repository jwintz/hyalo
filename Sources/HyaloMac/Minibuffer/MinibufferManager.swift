// MinibufferManager.swift - Manager for the native minibuffer overlay
// Candidate-list overlay only; text editing happens in the Emacs minibuffer.
// The overlay is rendered inside KelyphosShellView via the client overlays
// mechanism, replacing the previous standalone SearchPanel NSPanel.

import AppKit
import SwiftUI
import HyaloShared
import KelyphosKit

@available(macOS 26.0, *)
@MainActor
final class MinibufferManager {
    static let shared = MinibufferManager()

    let viewModel = MinibufferViewModel()

    // Callbacks to Emacs (wired by Module+Minibuffer.swift channel setup)
    var onCandidateSelected: ((String) -> Void)?
    var onAbort: (() -> Void)?

    private var isShowing = false

    private init() {}

    // MARK: - Show

    func show(from data: Data) {
        viewModel.show(from: data)

        if isShowing { return }
        isShowing = true

        // Wire view model callbacks to Emacs channel
        viewModel.onCandidateSelected = { [weak self] index in
            self?.onCandidateSelected?(String(index))
        }
        viewModel.onAbort = { [weak self] in
            self?.onAbort?()
        }

        // Toggle overlay on active frame's shell state
        if let state = HyaloModule.activeShellState {
            state.showMinibufferOverlay = true
        }
    }

    // MARK: - Update

    func update(from data: Data) {
        viewModel.update(from: data)
    }

    // MARK: - Hide

    func hide() {
        viewModel.hide()
        isShowing = false
        // Hide overlay on all frames
        for state in HyaloModule.allShellStates {
            state.showMinibufferOverlay = false
        }
    }
}
