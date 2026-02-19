// HyaloShared.swift - Shared utilities, constants, and components
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Design Constants

/// Design system constants for Hyalo UI components
enum HyaloDesign {

    enum CornerRadius {
        static let capsule: CGFloat = 14
        static let glass: CGFloat = 18
        static let content: CGFloat = 14
        static let menu: CGFloat = 12
        static let small: CGFloat = 6
    }

    enum Padding {
        static let horizontal: CGFloat = 12
        static let outer: CGFloat = 16
        static let sidebar: CGFloat = 14
        static let compact: CGFloat = 8
        static let section: CGFloat = 16
    }

    enum Height {
        static let modeLine: CGFloat = 24
        static let headerLine: CGFloat = 22
        static let toolbar: CGFloat = 28
        static let tabBar: CGFloat = 27
        static let statusBar: CGFloat = 28
    }

    enum Width {
        static let sidebarToggle: CGFloat = 47
        static let sidebarMin: CGFloat = 200
        static let sidebarIdeal: CGFloat = 280
        static let sidebarMax: CGFloat = 400
        static let inspectorMin: CGFloat = 300
        static let inspectorIdeal: CGFloat = 400
        static let inspectorMax: CGFloat = 500
    }

    enum Spacing {
        static let tight: CGFloat = 4
        static let compact: CGFloat = 8
        static let standard: CGFloat = 12
        static let comfortable: CGFloat = 16
        static let generous: CGFloat = 24
    }

    enum FontSize {
        static let small: CGFloat = 9
        static let caption: CGFloat = 10
        static let body: CGFloat = 11
        static let emphasized: CGFloat = 12
        static let large: CGFloat = 13
    }

    enum IconSize {
        static let small: CGFloat = 10
        static let medium: CGFloat = 12
        static let standard: CGFloat = 14
        static let large: CGFloat = 28
    }

    enum Animation {
        static let instant: Double = 0.0
        static let quick: Double = 0.1
        static let standard: Double = 0.25
        static let slow: Double = 0.3
    }
}

// MARK: - Geometry Tracking

/// Tracks geometry changes and reports them via a callback
struct GeometrySizeTracker: View {
    let onChange: (CGFloat) -> Void
    let dimension: Dimension
    let onUpdate: (() -> Void)?

    enum Dimension {
        case width
        case height
    }

    init(dimension: Dimension = .width, onChange: @escaping (CGFloat) -> Void, onUpdate: (() -> Void)? = nil) {
        self.dimension = dimension
        self.onChange = onChange
        self.onUpdate = onUpdate
    }

    init(dimension: Dimension = .width, onChange: @escaping (CGFloat) -> Void) {
        self.init(dimension: dimension, onChange: onChange, onUpdate: nil)
    }

    var body: some View {
        GeometryReader { geometry in
            Color.clear
                .onAppear {
                    let size = dimension == .width ? geometry.size.width : geometry.size.height
                    onChange(size)
                }
                .onChange(of: dimension == .width ? geometry.size.width : geometry.size.height) { _, newSize in
                    onChange(newSize)
                    onUpdate?()
                }
        }
    }
}

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
