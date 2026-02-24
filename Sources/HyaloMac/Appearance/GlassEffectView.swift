// GlassEffectView.swift - Glass effect wrappers (Liquid Glass)
// Target: macOS 26 Tahoe

import AppKit
import SwiftUI
import HyaloShared

// MARK: - Flat Glass Effect (panel backgrounds, tab bars)

/// Flat glass effect for panel backgrounds. Uses NSVisualEffectView with
/// behindWindow blending for translucent sidebar/inspector backgrounds.
/// cornerRadius = 0 for flush panel edges.
@available(macOS 26.0, *)
struct GlassEffectView: NSViewRepresentable {
    var tintColor: NSColor?

    func makeNSView(context: Context) -> NSVisualEffectView {
        let view = NSVisualEffectView()
        view.material = .sidebar
        view.blendingMode = .behindWindow
        view.state = .active
        if let tintColor {
            view.wantsLayer = true
            view.layer?.backgroundColor = tintColor.withAlphaComponent(0.08).cgColor
        }
        return view
    }

    func updateNSView(_ nsView: NSVisualEffectView, context: Context) {
        nsView.material = .sidebar
        if let tintColor {
            nsView.wantsLayer = true
            nsView.layer?.backgroundColor = tintColor.withAlphaComponent(0.08).cgColor
        } else {
            nsView.layer?.backgroundColor = nil
        }
    }
}

// MARK: - Glass Effect Container (floating panels)

/// Floating panel container (e.g., minibuffer, command palette).
/// Uses the Liquid Glass `.glassEffect()` modifier with a rounded-rect shape.
@available(macOS 26.0, *)
struct GlassEffectContainer<Content: View>: View {
    let title: String?
    let content: Content

    init(title: String? = nil, @ViewBuilder content: () -> Content) {
        self.title = title
        self.content = content()
    }

    var body: some View {
        VStack(spacing: 0) {
            if let title {
                HStack {
                    Text(title)
                        .font(.system(size: HyaloDesign.FontSize.caption, weight: .medium))
                        .foregroundStyle(.secondary)
                    Spacer()
                }
                .padding(.horizontal, HyaloDesign.Padding.horizontal)
                .padding(.vertical, 6)
            }

            content
                .padding(HyaloDesign.Padding.compact)
        }
        .glassEffect(in: .rect(cornerRadius: HyaloDesign.CornerRadius.glass))
        .shadow(color: .black.opacity(0.15), radius: 20, y: 10)
        .padding(HyaloDesign.Padding.outer)
    }
}
