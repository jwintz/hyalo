// HyaloToolbar.swift - NSToolbar subclass with EmacsToolbar compatibility stubs
// Target: macOS 26 Tahoe
//
// Emacs's C code (nsmenu.m update_frame_tool_bar_1) casts [window toolbar]
// to EmacsToolbar* and calls methods like clearActive, changed, and
// addDisplayItemWithImage:idx:tag:labelText:helpText:enabled: during redisplay.
//
// IMPORTANT: SwiftUI's NavigationSplitView with .toolbar {} may replace the
// window's toolbar with its own plain NSToolbar at any time. The stubs must
// therefore live on NSToolbar itself (via extension), not just on our subclass.
// The HyaloToolbar subclass is kept for explicit installation as a named stub.

import AppKit

@available(macOS 26.0, *)
final class HyaloToolbar: NSToolbar {
    // All Emacs compat stubs are on NSToolbar extension below,
    // so they work regardless of which toolbar SwiftUI installs.
}

// MARK: - EmacsToolbar compatibility stubs (on NSToolbar)
//
// These must be on NSToolbar, not just HyaloToolbar, because SwiftUI's
// NavigationSplitView replaces the window's toolbar with a plain NSToolbar.
// Emacs C code then calls these selectors on that plain NSToolbar instance.

extension NSToolbar {

    /// Called by update_frame_tool_bar_1 at the start of toolbar reconciliation.
    @objc func clearActive() {
        // no-op: Hyalo manages its own toolbar items
    }

    /// Called by update_frame_tool_bar_1 to check if the set of active items changed.
    /// Always return false so Emacs never tries to reconfigure our toolbar.
    @objc func changed() -> Bool {
        return false
    }

    /// Called by update_frame_tool_bar_1 for each Emacs tool-bar item.
    /// We ignore all Emacs tool-bar items since Hyalo provides its own.
    @objc(addDisplayItemWithImage:idx:tag:labelText:helpText:enabled:)
    func addDisplayItem(
        withImage image: AnyObject,
        idx: Int32,
        tag: Int32,
        labelText: NSString,
        helpText: NSString,
        enabled: Bool
    ) {
        // no-op: Hyalo toolbar items are managed by SwiftUI
    }

    /// Called by update_frame_tool_bar_1 after reconciliation.
    @objc func clearAll() {
        // no-op
    }
}
