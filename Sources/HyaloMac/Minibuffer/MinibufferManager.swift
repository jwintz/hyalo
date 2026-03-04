// MinibufferManager.swift - Manager for the native minibuffer panel
// Replaces CommandPaletteManager with a single generic panel.
// Keyboard events handled via SearchPanel's NSEvent monitor.

import AppKit
import SwiftUI
import HyaloShared

@available(macOS 26.0, *)
final class MinibufferManager {
    static let shared = MinibufferManager()

    private var panel: SearchPanel?

    let viewModel = MinibufferViewModel()

    // Callbacks to Emacs (wired by Module+Minibuffer.swift channel setup)
    var onInputChanged: ((String) -> Void)?
    var onCandidateSelected: ((String) -> Void)?
    var onAbort: (() -> Void)?

    private init() {}

    // MARK: - Show

    func show(from data: Data) {
        NSLog("[Hyalo:Minibuffer] show called, data size=%d", data.count)
        viewModel.show(from: data)

        if panel != nil {
            NSLog("[Hyalo:Minibuffer] panel already visible, updating state only")
            return
        }

        guard let parentWindow = findParentWindow() else {
            NSLog("[Hyalo:Minibuffer] no parent window found")
            return
        }

        let searchPanel = SearchPanel { [weak self] in
            self?.abort()
        }

        searchPanel.onArrowUp = { [weak self] in
            self?.viewModel.selectPrevious()
        }
        searchPanel.onArrowDown = { [weak self] in
            self?.viewModel.selectNext()
        }
        searchPanel.onConfirm = { [weak self] in
            self?.viewModel.confirm()
        }

        panel = searchPanel

        // Wire view model callbacks to Emacs channel
        viewModel.onInputChanged = { [weak self] text in
            NSLog("[Hyalo:Minibuffer] input changed: %@", text)
            self?.onInputChanged?(text)
        }
        viewModel.onCandidateSelected = { [weak self] index in
            NSLog("[Hyalo:Minibuffer] candidate selected: %d", index)
            self?.onCandidateSelected?(String(index))
        }
        viewModel.onAbort = { [weak self] in
            self?.abort()
        }

        let contentView = MinibufferView(viewModel: viewModel)
        searchPanel.contentView = NSHostingView(rootView: contentView)

        // Set panel to proper size before positioning
        let panelWidth: CGFloat = 680
        let panelHeight: CGFloat = 400
        searchPanel.setContentSize(NSSize(width: panelWidth, height: panelHeight))

        searchPanel.positionRelativeToParent(parentWindow)
        parentWindow.addChildWindow(searchPanel, ordered: .above)
        searchPanel.makeKeyAndOrderFront(nil)
        NSLog("[Hyalo:Minibuffer] panel shown, prompt=%@", viewModel.prompt)
    }

    // MARK: - Update

    func update(from data: Data) {
        NSLog("[Hyalo:Minibuffer] update called, data size=%d", data.count)
        viewModel.update(from: data)
        NSLog("[Hyalo:Minibuffer] after update: %d candidates, selectedIndex=%d",
              viewModel.candidates.count, viewModel.selectedIndex)
    }

    // MARK: - Hide

    func hide() {
        viewModel.hide()
        dismissPanel()
    }

    // MARK: - Abort (user pressed Escape in Swift panel)

    private func abort() {
        onAbort?()
        // Don't call viewModel.hide() here — Emacs will call hyalo-minibuffer-hide
        // via the minibuffer-exit-hook after abort-recursive-edit completes.
    }

    // MARK: - Panel Lifecycle

    private func dismissPanel() {
        guard let p = panel else { return }
        panel = nil
        p.close()
        restoreEmacsFirstResponder()
    }

    // MARK: - Window Discovery

    private func findParentWindow() -> NSWindow? {
        if let window = NSApp.mainWindow, !window.isMiniaturized { return window }
        if let window = NSApp.keyWindow, !window.isMiniaturized { return window }
        return NSApp.windows.first { $0.isVisible && !$0.isMiniaturized }
    }

    private func restoreEmacsFirstResponder() {
        guard let window = findParentWindow() else { return }
        DispatchQueue.main.async {
            window.makeKeyAndOrderFront(nil)
            func findEmacsView(in view: NSView) -> NSView? {
                let className = String(describing: type(of: view))
                if className.contains("EmacsView") { return view }
                for subview in view.subviews {
                    if let found = findEmacsView(in: subview) { return found }
                }
                return nil
            }
            if let contentView = window.contentView,
               let emacsView = findEmacsView(in: contentView) {
                window.makeFirstResponder(emacsView)
            }
        }
    }
}
