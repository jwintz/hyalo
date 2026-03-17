// SearchPanel.swift - Floating overlay panel for minibuffer candidates
// Non-activating: keystrokes go to the Emacs window; this panel is display-only.

import AppKit

@available(macOS 26.0, *)
final class SearchPanel: NSPanel {

    init() {
        super.init(
            contentRect: NSRect(x: 0, y: 0, width: 600, height: 48),
            styleMask: [.fullSizeContentView, .titled, .resizable, .nonactivatingPanel],
            backing: .buffered,
            defer: false
        )

        self.titlebarAppearsTransparent = true
        self.isMovableByWindowBackground = true
        self.level = .floating
        self.hidesOnDeactivate = false
    }

    override var canBecomeKey: Bool { false }
    override var canBecomeMain: Bool { false }

    // Accept first mouse so clicks work without requiring activation
    override var acceptsMouseMovedEvents: Bool {
        get { true }
        set { super.acceptsMouseMovedEvents = newValue }
    }

    override func standardWindowButton(_ button: NSWindow.ButtonType) -> NSButton? {
        let button = super.standardWindowButton(button)
        button?.isHidden = true
        return button
    }

    /// Position centered horizontally and vertically in parent window
    func positionRelativeToParent(_ parent: NSWindow) {
        let parentFrame = parent.frame
        let panelSize = self.frame.size
        let x = parentFrame.midX - panelSize.width / 2
        let y = parentFrame.midY - panelSize.height / 2
        self.setFrameOrigin(NSPoint(x: x, y: y))
    }
}

