// MinibufferManager.swift - Manager for the native minibuffer overlay panel
// Candidate-list overlay only; text editing happens in the Emacs minibuffer.

import AppKit
import SwiftUI
import HyaloShared

@available(macOS 26.0, *)
final class MinibufferManager {
    static let shared = MinibufferManager()

    private var panel: SearchPanel?

    let viewModel = MinibufferViewModel()

    // Callbacks to Emacs (wired by Module+Minibuffer.swift channel setup)
    var onCandidateSelected: ((String) -> Void)?
    var onAbort: (() -> Void)?

    private init() {}

    // MARK: - Show

    func show(from data: Data) {
        viewModel.show(from: data)

        if panel != nil {
            return
        }

        guard let parentWindow = findParentWindow() else {
            NSLog("[Hyalo:Minibuffer] no parent window found")
            return
        }

        let searchPanel = SearchPanel()
        panel = searchPanel

        // Wire view model callbacks to Emacs channel
        viewModel.onCandidateSelected = { [weak self] index in
            self?.onCandidateSelected?(String(index))
        }
        viewModel.onAbort = { [weak self] in
            self?.onAbort?()
        }

        let contentView = MinibufferView(viewModel: viewModel)
        searchPanel.contentView = NSHostingView(rootView: contentView)

        // Set panel to proper size before positioning
        let panelWidth: CGFloat = 680
        let hasInitialCandidates = !viewModel.candidates.isEmpty || viewModel.totalCandidates > 0
        let panelHeight: CGFloat = hasInitialCandidates ? 400 : 60
        searchPanel.setContentSize(NSSize(width: panelWidth, height: panelHeight))

        searchPanel.positionRelativeToParent(parentWindow)
        parentWindow.addChildWindow(searchPanel, ordered: .above)
        searchPanel.orderFront(nil)
    }

    // MARK: - Update

    func update(from data: Data) {
        let hadCandidates = !viewModel.candidates.isEmpty
        viewModel.update(from: data)
        let hasCandidates = !viewModel.candidates.isEmpty

        // Re-center panel when candidates appear/disappear (height changes)
        if hadCandidates != hasCandidates, let searchPanel = panel,
           let parentWindow = searchPanel.parent {
            let panelWidth: CGFloat = 680
            let panelHeight: CGFloat = hasCandidates ? 400 : 60
            searchPanel.setContentSize(NSSize(width: panelWidth, height: panelHeight))
            searchPanel.positionRelativeToParent(parentWindow)
        }
    }

    // MARK: - Hide

    func hide() {
        viewModel.hide()
        dismissPanel()
    }

    // MARK: - Panel Lifecycle

    private func dismissPanel() {
        guard let p = panel else { return }
        panel = nil
        p.close()
    }

    // MARK: - Window Discovery

    private func findParentWindow() -> NSWindow? {
        if let window = NSApp.mainWindow, !window.isMiniaturized { return window }
        if let window = NSApp.keyWindow, !window.isMiniaturized { return window }
        return NSApp.windows.first { $0.isVisible && !$0.isMiniaturized }
    }
}
