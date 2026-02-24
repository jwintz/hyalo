#if canImport(UIKit)
// EmacsViewsiOS.swift - UIViewRepresentable wrapper for the Emacs rendering surface
// Hosts the UIView created by iosterm.m inside the SwiftUI view hierarchy.

import SwiftUI
import UIKit

/// Container UIView that clips and manages the Emacs rendering view.
///
/// Responsibilities:
///  - Size the EmacsView to fill the container (synchronously in
///    layoutSubviews so the feedstock's EmacsView.layoutSubviews can
///    call ios_request_frame_resize with the correct dimensions).
///  - Forward touches to the EmacsView via hitTest.
///  - Claim first responder status for hardware keyboard routing once
///    the view is embedded in a window with non-zero bounds.
class EmacsContainerViewiOS: UIView {
    weak var emacsView: UIView?
    override var canBecomeFirstResponder: Bool { true }

    override func layoutSubviews() {
        super.layoutSubviews()
        // Set the EmacsView frame synchronously so that the
        // feedstock's EmacsView.layoutSubviews fires in the same
        // layout pass and can call ios_request_frame_resize with the
        // actual window dimensions.  The resize goes through the
        // feedstock event queue, so Emacs processes it asynchronously
        // on its own thread — no risk of glyph matrix corruption.
        emacsView?.frame = bounds
    }

    override func hitTest(_ point: CGPoint, with event: UIEvent?) -> UIView? {
        guard bounds.contains(point) else { return nil }
        return emacsView ?? super.hitTest(point, with: event)
    }

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard window != nil, let emacsView else { return }
        // Defer becomeFirstResponder to the next run-loop iteration so
        // the layout pass has completed and the view has non-zero bounds.
        // becomeFirstResponder fails on a zero-size view.
        DispatchQueue.main.async { [weak emacsView] in
            emacsView?.becomeFirstResponder()
        }
    }
}

/// SwiftUI bridge for the Emacs UIView.
@available(iOS 26.0, *)
struct EmacsUIViewRepresentable: UIViewRepresentable {
    let emacsView: UIView?

    func makeUIView(context: Context) -> EmacsContainerViewiOS {
        let container = EmacsContainerViewiOS()
        container.clipsToBounds = true
        if let emacsView {
            emacsView.removeFromSuperview()
            container.addSubview(emacsView)
            container.emacsView = emacsView
            // Frame will be set by layoutSubviews after SwiftUI
            // determines the container's bounds.
        }
        return container
    }

    func updateUIView(_ uiView: EmacsContainerViewiOS, context: Context) {
        if let emacsView, uiView.emacsView !== emacsView {
            // EmacsView instance changed — re-parent.
            uiView.emacsView?.removeFromSuperview()
            emacsView.removeFromSuperview()
            uiView.addSubview(emacsView)
            uiView.emacsView = emacsView
            uiView.setNeedsLayout()
            // First responder will be asserted by didMoveToWindow or
            // the next layout cycle.
        }
        // Frame updates are handled by layoutSubviews — no need to
        // set the frame here explicitly.
    }
}

#endif // canImport(UIKit)
