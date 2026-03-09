// HyaloShared.swift - Shared utilities, constants, and components
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI
import HyaloShared

// MARK: - Design Constants

/// Design system constants for Hyalo UI components
/* HyaloDesign moved to HyaloDesign.swift in HyaloShared */

// MARK: - Geometry Tracking

 

// MARK: - Vibrancy Background

/// NSVisualEffectView wrapper for behind-window blur.
/// Placed behind the Emacs view in the editor content area.
/// Emacs renders with alpha-background 0.0, so this view provides
/// the actual blur effect that shows through the transparent EmacsView.
@available(macOS 26.0, *)
struct HyaloVibrancyBackground: NSViewRepresentable {
    let material: NSVisualEffectView.Material
    let blendingMode: NSVisualEffectView.BlendingMode

    func makeNSView(context: Context) -> NSVisualEffectView {
        let view = NSVisualEffectView()
        view.material = material
        view.blendingMode = blendingMode
        view.state = .active
        return view
    }

    func updateNSView(_ nsView: NSVisualEffectView, context: Context) {
        nsView.material = material
        nsView.blendingMode = blendingMode
    }
}

// MARK: - Content Unavailable Placeholder

 
