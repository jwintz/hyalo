// EmacsViewsiOS.swift - UIViewRepresentable wrapper for the Emacs rendering surface
// Hosts the UIView created by iosterm.m inside the SwiftUI view hierarchy.

import SwiftUI
import UIKit

/// Container UIView that clips and manages the Emacs rendering view.
class EmacsContainerViewiOS: UIView {
    weak var emacsView: UIView?

    override func layoutSubviews() {
        super.layoutSubviews()
        // Defer resize to avoid glyph matrix corruption during animation
        let targetFrame = bounds
        DispatchQueue.main.async { [weak self] in
            self?.emacsView?.frame = targetFrame
        }
    }

    override func hitTest(_ point: CGPoint, with event: UIEvent?) -> UIView? {
        // Ensure touches reach the Emacs view
        guard bounds.contains(point) else { return nil }
        return emacsView ?? super.hitTest(point, with: event)
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
        }
        return container
    }

    func updateUIView(_ uiView: EmacsContainerViewiOS, context: Context) {
        // If the emacs view changed, re-parent it
        if let emacsView, uiView.emacsView !== emacsView {
            uiView.emacsView?.removeFromSuperview()
            emacsView.removeFromSuperview()
            uiView.addSubview(emacsView)
            uiView.emacsView = emacsView
            uiView.setNeedsLayout()
        }
    }
}
