// ToolbarVisibilityPriorityModifier.swift - Sets NSToolbarItem.visibilityPriority
// via an NSViewRepresentable background placed on the ToolbarItem's content.
//
// SwiftUI's .toolbar {} API has no public visibilityPriority surface. The only
// way to set it is to reach through to the underlying NSToolbarItem after layout.
//
// How it works:
//   1. A zero-size NSViewRepresentable is embedded as a .background on the item's
//      content view. SwiftUI renders it inside the same NSHostingView that
//      NSToolbar uses as the item's view property.
//   2. On the next run loop pass (after the window and toolbar are live), we walk
//      toolbar.items and find the one whose .view is an ancestor of our NSView.
//   3. We set visibilityPriority on that NSToolbarItem.
//
// Usage — place inside a ToolbarItem's content:
//   Button { ... } label: { ... }
//       .toolbarItemVisibilityPriority(.required)

import AppKit
import SwiftUI

// MARK: - Public modifier

@available(macOS 26.0, *)
extension View {
    /// Configures the NSToolbarItem.visibilityPriority for the enclosing toolbar item.
    /// Apply this modifier to content that is a direct child of a ToolbarItem.
    func toolbarItemVisibilityPriority(
        _ priority: NSToolbarItem.VisibilityPriority
    ) -> some View {
        background(
            _ToolbarPriorityAnchor(priority: priority)
                .frame(width: 0, height: 0)
                .allowsHitTesting(false)
        )
    }
}

// MARK: - NSViewRepresentable anchor

@available(macOS 26.0, *)
private struct _ToolbarPriorityAnchor: NSViewRepresentable {
    let priority: NSToolbarItem.VisibilityPriority

    final class Coordinator {
        var applied = false
    }

    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeNSView(context: Context) -> NSView {
        let view = NSView()
        view.setContentHuggingPriority(.defaultLow, for: .horizontal)
        view.setContentHuggingPriority(.defaultLow, for: .vertical)
        // First attempt: deferred one run loop to let the window wire up.
        DispatchQueue.main.async {
            if !context.coordinator.applied {
                context.coordinator.applied = applyPriority(from: view)
            }
        }
        return view
    }

    func updateNSView(_ nsView: NSView, context: Context) {
        // Retry on every SwiftUI update until we succeed (e.g. if the toolbar
        // was not yet available during makeNSView's deferred block).
        guard !context.coordinator.applied else { return }
        DispatchQueue.main.async {
            if !context.coordinator.applied {
                context.coordinator.applied = applyPriority(from: nsView)
            }
        }
    }

    /// Walks UP the superview chain to find the NSToolbarItem that hosts this
    /// view and sets its visibilityPriority. Returns true on success.
    @discardableResult
    private func applyPriority(from view: NSView) -> Bool {
        guard let toolbar = view.window?.toolbar else { return false }
        var candidate: NSView? = view.superview
        while let v = candidate {
            for item in toolbar.items {
                if item.view === v {
                    item.visibilityPriority = priority
                    return true
                }
            }
            candidate = v.superview
        }
        return false
    }
}
