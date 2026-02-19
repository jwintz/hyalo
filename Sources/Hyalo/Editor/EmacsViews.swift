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
        emacsView.translatesAutoresizingMaskIntoConstraints = false
        emacsView.constraints.forEach { $0.isActive = false }

        container.addSubview(emacsView)

        let constraints = [
            emacsView.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            emacsView.trailingAnchor.constraint(equalTo: container.trailingAnchor),
            emacsView.topAnchor.constraint(equalTo: container.topAnchor),
            emacsView.bottomAnchor.constraint(equalTo: container.bottomAnchor)
        ]
        constraints.forEach { $0.priority = .defaultHigh }
        NSLayoutConstraint.activate(constraints)

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
