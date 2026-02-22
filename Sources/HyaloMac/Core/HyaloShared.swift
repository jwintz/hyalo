// HyaloShared.swift - Shared utilities, constants, and components
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Design Constants

/// Design system constants for Hyalo UI components
/* HyaloDesign moved to HyaloDesign.swift in HyaloShared */

// MARK: - Geometry Tracking

// Geometry tracking moved to HyaloShared

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

@available(macOS 26.0, *)
struct HyaloContentUnavailableView<Actions: View>: View {
    let label: String
    let description: String?
    let systemImage: String?
    let actions: Actions?

    init(
        _ label: String,
        description: String? = nil,
        systemImage: String? = nil
    ) where Actions == EmptyView {
        self.label = label
        self.description = description
        self.systemImage = systemImage
        self.actions = nil
    }

    init(
        _ label: String,
        description: String? = nil,
        systemImage: String? = nil,
        @ViewBuilder actions: () -> Actions
    ) {
        self.label = label
        self.description = description
        self.systemImage = systemImage
        self.actions = actions()
    }

    var body: some View {
        VStack(spacing: 14) {
            if let systemImage {
                Image(systemName: systemImage)
                    .font(.system(size: 28))
                    .foregroundStyle(.tertiary)
                    .padding(.bottom, 8)
            }

            VStack(spacing: 5) {
                Text(label)
                    .font(.system(size: 16.5, weight: systemImage != nil ? .bold : .regular))

                if let description {
                    Text(description)
                        .font(.system(size: 10))
                        .multilineTextAlignment(.center)
                }
            }

            if let actions {
                HStack { actions }
            }
        }
        .foregroundStyle(.secondary)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .contentShape(Rectangle())
    }
}
