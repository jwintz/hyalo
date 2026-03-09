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
    var onHistoryPrev: (() -> Void)?
    var onHistoryNext: (() -> Void)?
    var onTabComplete: (() -> Void)?

    private init() {}

    // MARK: - Show

    func show(from data: Data) {
        #if DEBUG
        NSLog("[Hyalo:Minibuffer] show called, data size=%d", data.count)
        #endif
        viewModel.show(from: data)

        if panel != nil {
            #if DEBUG
            NSLog("[Hyalo:Minibuffer] panel already visible, updating state only")
            #endif
            return
        }

        guard let parentWindow = findParentWindow() else {
            NSLog("[Hyalo:Minibuffer] no parent window found")
            return
        }

        let searchPanel = SearchPanel { [weak self] in
            self?.abort()
        }

        searchPanel.historyMode = viewModel.historyMode
        searchPanel.onArrowUp = { [weak self] in
            self?.viewModel.selectPrevious()
        }
        searchPanel.onArrowDown = { [weak self] in
            self?.viewModel.selectNext()
        }
        searchPanel.onConfirm = { [weak self] in
            self?.viewModel.confirm()
        }
        searchPanel.onHistoryPrev = { [weak self] in
            self?.viewModel.historyPrev()
        }
        searchPanel.onHistoryNext = { [weak self] in
            self?.viewModel.historyNext()
        }
        searchPanel.onTabComplete = { [weak self] in
            self?.viewModel.tabComplete()
        }

        panel = searchPanel

        // Wire view model callbacks to Emacs channel
        viewModel.onInputChanged = { [weak self] text in
            #if DEBUG
            NSLog("[Hyalo:Minibuffer] input changed: %@", text)
            #endif
            self?.onInputChanged?(text)
        }
        viewModel.onCandidateSelected = { [weak self] index in
            #if DEBUG
            NSLog("[Hyalo:Minibuffer] candidate selected: %d", index)
            #endif
            self?.onCandidateSelected?(String(index))
        }
        viewModel.onAbort = { [weak self] in
            self?.abort()
        }
        viewModel.onHistoryPrev = { [weak self] in
            #if DEBUG
            NSLog("[Hyalo:Minibuffer] history prev")
            #endif
            self?.onHistoryPrev?()
        }
        viewModel.onHistoryNext = { [weak self] in
            #if DEBUG
            NSLog("[Hyalo:Minibuffer] history next")
            #endif
            self?.onHistoryNext?()
        }
        viewModel.onTabComplete = { [weak self] in
            #if DEBUG
            NSLog("[Hyalo:Minibuffer] tab complete")
            #endif
            self?.onTabComplete?()
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
        searchPanel.makeKeyAndOrderFront(nil)
        #if DEBUG
        NSLog("[Hyalo:Minibuffer] panel shown, prompt=%@", viewModel.prompt)
        #endif
    }

    // MARK: - Update

    func update(from data: Data) {
        #if DEBUG
        NSLog("[Hyalo:Minibuffer] update called, data size=%d", data.count)
        #endif
        viewModel.update(from: data)
        #if DEBUG
        NSLog("[Hyalo:Minibuffer] after update: %d candidates, selectedIndex=%d",
              viewModel.candidates.count, viewModel.selectedIndex)
        #endif
    }

    // MARK: - Hide

    func hide() {
        #if DEBUG
        NSLog("[Hyalo:Minibuffer] hide called, panel=%@", panel != nil ? "yes" : "nil")
        #endif
        viewModel.hide()
        dismissPanel()
    }

    // MARK: - Abort (user pressed Escape in Swift panel)

    private func abort() {
        #if DEBUG
        NSLog("[Hyalo:Minibuffer] abort called")
        #endif
        onAbort?()
        // Don't call viewModel.hide() here — Emacs will call hyalo-minibuffer-hide
        // via the minibuffer-exit-hook after abort-recursive-edit completes.
    }

    // MARK: - Panel Lifecycle

    private func dismissPanel() {
        guard let p = panel else { return }
        panel = nil
        // Detach delegate BEFORE close to prevent windowDidResignKey → abort.
        // Without this, p.close() triggers windowDidResignKey which sends a
        // spurious abort through the Emacs channel, killing the next minibuffer
        // session (e.g., M-x compile → compile's read-shell-command).
        p.delegate = nil
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
        guard let window = findParentWindow() else {
            #if DEBUG
            NSLog("[Hyalo:Minibuffer] restoreEmacsFirstResponder: no parent window")
            #endif
            return
        }
        DispatchQueue.main.async { [weak self] in
            guard self?.panel == nil else {
                #if DEBUG
                NSLog("[Hyalo:Minibuffer] restoreEmacsFirstResponder: SKIPPED (new panel active)")
                #endif
                return
            }
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
