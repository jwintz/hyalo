// SearchPanel.swift - Floating search panel base
// Target: macOS 26 Tahoe with Liquid Glass design
// Handles Escape/Arrow/Enter via NSEvent local monitor (not SwiftUI keyboard shortcuts)

import AppKit
import SwiftUI

@available(macOS 26.0, *)
final class SearchPanel: NSPanel, NSWindowDelegate {
    private var onClose: (() -> Void)?
    var onArrowUp: (() -> Void)?
    var onArrowDown: (() -> Void)?
    var onConfirm: (() -> Void)?

    private var eventMonitor: Any?

    init(onClose: (() -> Void)? = nil) {
        self.onClose = onClose

        super.init(
            contentRect: NSRect(x: 0, y: 0, width: 600, height: 48),
            styleMask: [.fullSizeContentView, .titled, .resizable],
            backing: .buffered,
            defer: false
        )

        self.delegate = self
        self.center()
        self.titlebarAppearsTransparent = true
        self.isMovableByWindowBackground = true

        installEventMonitor()
    }

    deinit {
        removeEventMonitor()
    }

    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }

    override func standardWindowButton(_ button: NSWindow.ButtonType) -> NSButton? {
        let button = super.standardWindowButton(button)
        button?.isHidden = true
        return button
    }

    func windowDidResignKey(_ notification: Notification) {
        onClose?()
    }

    /// Position like Spotlight/Xcode Quick Open: horizontally centered, top quarter of parent
    func positionRelativeToParent(_ parent: NSWindow) {
        let parentFrame = parent.frame
        let panelSize = self.frame.size
        let x = parentFrame.midX - panelSize.width / 2
        let y = parentFrame.origin.y + parentFrame.height * 0.70 - panelSize.height / 2
        self.setFrameOrigin(NSPoint(x: x, y: y))
    }

    // MARK: - Event Monitor

    private func installEventMonitor() {
        eventMonitor = NSEvent.addLocalMonitorForEvents(matching: .keyDown) { [weak self] event in
            guard let self, self.isKeyWindow else { return event }

            switch Int(event.keyCode) {
            case 53: // Escape
                self.onClose?()
                return nil
            case 126: // Up arrow
                self.onArrowUp?()
                return nil
            case 125: // Down arrow
                self.onArrowDown?()
                return nil
            case 36: // Return/Enter
                self.onConfirm?()
                return nil
            default:
                return event
            }
        }
    }

    private func removeEventMonitor() {
        if let monitor = eventMonitor {
            NSEvent.removeMonitor(monitor)
            eventMonitor = nil
        }
    }
}
