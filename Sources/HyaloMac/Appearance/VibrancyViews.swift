// VibrancyViews.swift - Additional vibrancy helpers
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Gradient Fade View

/// A gradient overlay that fades content at edges
@available(macOS 26.0, *)
struct GradientFadeView: View {
    let edge: Edge
    let length: CGFloat

    init(edge: Edge = .bottom, length: CGFloat = 20) {
        self.edge = edge
        self.length = length
    }

    var body: some View {
        LinearGradient(
            gradient: Gradient(colors: [.clear, .black]),
            startPoint: startPoint,
            endPoint: endPoint
        )
        .frame(width: isHorizontal ? length : nil, height: isVertical ? length : nil)
        .allowsHitTesting(false)
    }

    private var isHorizontal: Bool { edge == .leading || edge == .trailing }
    private var isVertical: Bool { edge == .top || edge == .bottom }

    private var startPoint: UnitPoint {
        switch edge {
        case .top: return .bottom
        case .bottom: return .top
        case .leading: return .trailing
        case .trailing: return .leading
        }
    }

    private var endPoint: UnitPoint {
        switch edge {
        case .top: return .top
        case .bottom: return .bottom
        case .leading: return .leading
        case .trailing: return .trailing
        }
    }
}

// MARK: - Vibrancy Background View

/// NSVisualEffectView wrapper for fine-grained blur control
@available(macOS 26.0, *)
struct VibrancyBackgroundView: NSViewRepresentable {
    var material: NSVisualEffectView.Material
    var blendingMode: NSVisualEffectView.BlendingMode
    var isActive: Bool

    func makeNSView(context: Context) -> NSVisualEffectView {
        let view = NSVisualEffectView()
        view.material = material
        view.blendingMode = blendingMode
        view.state = isActive ? .active : .inactive
        view.isEmphasized = true
        view.wantsLayer = true
        return view
    }

    func updateNSView(_ nsView: NSVisualEffectView, context: Context) {
        nsView.material = material
        nsView.blendingMode = blendingMode
        nsView.state = isActive ? .active : .inactive
    }
}

// MARK: - Effect View (Background)

@available(macOS 26.0, *)
struct EffectView: NSViewRepresentable {
    let material: NSVisualEffectView.Material
    let blendingMode: NSVisualEffectView.BlendingMode

    init(_ material: NSVisualEffectView.Material, blendingMode: NSVisualEffectView.BlendingMode = .behindWindow) {
        self.material = material
        self.blendingMode = blendingMode
    }

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
