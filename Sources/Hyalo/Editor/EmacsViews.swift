// EmacsViews.swift - NSViewRepresentable wrapper for Emacs NSView
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Emacs Container View

/// Custom container view that manages Emacs keyboard focus.
///
/// Key design: Emacs gets focus when you click in its area.
/// SwiftUI chrome (buttons, tab bars) must NOT have focus stolen.
///
/// CRITICAL: The hitTest override clips to bounds. Without this,
/// the EmacsView intercepts all mouse events in the main content
/// split view item (status bar, utility area, editor tab bar),
/// because SwiftUI's NSViewRepresentable bridge does not clip
/// the NSView's hit-testing region to its SwiftUI layout frame.
@available(macOS 26.0, *)
class EmacsContainerView: NSView {
    weak var emacsView: NSView?

    override var acceptsFirstResponder: Bool { true }

    override var isFlipped: Bool { true }

    override func becomeFirstResponder() -> Bool {
        if let emacs = emacsView {
            return window?.makeFirstResponder(emacs) ?? false
        }
        return false
    }

    override func hitTest(_ point: NSPoint) -> NSView? {
        // point is in superview's coordinate system.
        // Only accept hits within our actual frame.
        // Without this guard, hits in the status bar, utility area,
        // and editor tab bar regions are intercepted by EmacsView.
        guard frame.contains(point) else { return nil }
        return super.hitTest(point)
    }

    override func mouseDown(with event: NSEvent) {
        // Click in Emacs area: make Emacs first responder, then forward
        if let emacs = emacsView {
            window?.makeFirstResponder(emacs)
        }
        super.mouseDown(with: event)
    }

    override func layout() {
        super.layout()
        guard let ev = emacsView else { return }
        guard bounds.width > 0 && bounds.height > 0 else { return }
        if ev.frame.size != bounds.size {
            // Defer the Emacs resize notification.  layout() can fire during
            // makeKeyAndOrderFront (ns_raise_frame holds block_input),
            // and change_frame_size must NOT be called with input blocked.
            // Keep EmacsView at its current size (glyph matrices stay valid)
            // and schedule the resize for the next event-loop iteration
            // (fires safely during the next sit-for).
            DispatchQueue.main.async { [weak self] in
                guard let self, let ev = self.emacsView else { return }
                guard self.bounds.width > 0 && self.bounds.height > 0 else { return }
                if ev.frame.size != self.bounds.size {
                    ev.resize(withOldSuperviewSize: ev.frame.size)
                }
            }
        }
    }
}

// MARK: - Emacs NSViewRepresentable

@available(macOS 26.0, *)
struct EmacsNSViewRepresentable: NSViewRepresentable {
    let emacsView: NSView

    func makeNSView(context: Context) -> EmacsContainerView {
        let container = EmacsContainerView()
        container.emacsView = emacsView
        container.wantsLayer = true
        container.layer?.masksToBounds = true

        emacsView.removeFromSuperview()

        // Keep translatesAutoresizingMaskIntoConstraints = true (default).
        // Do NOT activate NSLayoutConstraints — constraint activation
        // triggers resizeWithOldSuperviewSize: on EmacsView while the
        // container still has zero bounds (SwiftUI hasn't laid it out),
        // which calls change_frame_size(f, 0, 0, ...) and corrupts
        // Emacs's glyph matrices.  EmacsContainerView.layout() handles
        // sizing once the container has valid dimensions.
        container.addSubview(emacsView)

        // Give Emacs initial focus once the view is in a window
        DispatchQueue.main.async {
            if let window = container.window, window.isKeyWindow {
                window.makeFirstResponder(emacsView)
            }
        }

        return container
    }

    func updateNSView(_ nsView: EmacsContainerView, context: Context) {
        // Do NOT restore Emacs focus here. This method is called on
        // every SwiftUI state change, which would steal focus from
        // buttons, tab bars, text fields, and other SwiftUI chrome.
    }
}
