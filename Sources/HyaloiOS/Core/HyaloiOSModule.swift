#if canImport(UIKit)
// HyaloiOSModule.swift - Top-level coordinator for Hyalo on iOS
// Connects EmacsLifecycle, ChannelBridge, and shared view models.

import UIKit
import SwiftUI
import HyaloShared
import KelyphosKit

/// Main coordinator for Hyalo on iOS.
/// Owns the lifecycle, workspace state, and wires up the channel bridge.
@available(iOS 26.0, *)
@MainActor
@Observable
public final class HyaloiOSModule {

    public static let shared = HyaloiOSModule()

    let lifecycle = EmacsLifecycle()
    let workspace = HyaloWorkspaceState()
    let shellState = KelyphosShellState(persistencePrefix: "hyalo")

    /// The EmacsView handed off from iosterm.m via ios_set_main_emacs_view.
    var emacsView: UIView?
    let editorTabViewModel = EditorTabViewModel()
    let utilityAreaViewModel = UtilityAreaViewModel()
    let commandPaletteViewModel = CommandPaletteViewModel()
    let openQuicklyViewModel = OpenQuicklyViewModel()

    // Channel callbacks wired by ChannelBridge when Emacs sets up channels.
    var onOpenFile: ((String) -> Void)?
    var onExecuteCommand: ((String) -> Void)?

    // Presentation state for Cmd+P / Cmd+O sheets (driven by Emacs or toolbar)
    var showCommandPalette = false
    var showOpenQuickly = false

    private init() {
        // Register the iOS wakeEmacs implementation
        platformWakeEmacs = {
            ios_signal_event_available()
        }
        // Wire appearance callback → DispatchRouter
        if #available(iOS 26.0, *) {
            InspectorAppearanceCallbacks.onAppearanceModeChanged = { mode in
                DispatchRouter.shared.sendCommand(.appearanceChange, payload: ["mode": mode])
            }
        }
    }

    /// Start Emacs and set up channels.
    public func start() {
        lifecycle.start()
    }

}

// MARK: - C bridge

/// Override of the weak symbol in iosterm.m.
/// Called on the Emacs thread immediately after the EmacsView is created.
/// Ownership stays with the feedstock (takeUnretainedValue).
@_cdecl("ios_set_main_emacs_view")
func bridgeSetMainEmacsView(_ viewPtr: UnsafeMutableRawPointer) {
    let view = Unmanaged<UIView>.fromOpaque(viewPtr).takeUnretainedValue()
    DispatchQueue.main.async {
        HyaloiOSModule.shared.emacsView = view
        // Transition lifecycle to .running now that the Emacs UIView is ready.
        // ios_emacs_init never returns while Emacs is alive, so this C callback
        // is the correct trigger point for the SwiftUI shell to appear.
        HyaloiOSModule.shared.lifecycle.markRunning()
    }
}

/// Weak-symbol override checked by ios_connect_frame_to_window in the
/// feedstock.  When this returns true, the feedstock skips its default
/// behavior (replacing rootViewController + delayed becomeFirstResponder)
/// because SwiftUI manages the window hierarchy.  The view hand-off
/// already happened via ios_set_main_emacs_view; SwiftUI embeds the
/// view through EmacsUIViewRepresentable, and
/// EmacsContainerViewiOS.didMoveToWindow asserts first responder once
/// the view has proper bounds.
@_cdecl("ios_has_swiftui_host")
func bridgeHasSwiftUIHost() -> Bool {
    return true
}

#endif // canImport(UIKit)
