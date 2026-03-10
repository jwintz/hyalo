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
    }

    override var canBecomeKey: Bool { false }
    override var canBecomeMain: Bool { false }

    override func standardWindowButton(_ button: NSWindow.ButtonType) -> NSButton? {
        let button = super.standardWindowButton(button)
        button?.isHidden = true
        return button
    }

    /// Position like Spotlight/Xcode Quick Open: horizontally centered, top quarter of parent
    func positionRelativeToParent(_ parent: NSWindow) {
        let parentFrame = parent.frame
        let panelSize = self.frame.size
        let x = parentFrame.midX - panelSize.width / 2
        let y = parentFrame.origin.y + parentFrame.height * 0.70 - panelSize.height / 2
        self.setFrameOrigin(NSPoint(x: x, y: y))
    }
}
