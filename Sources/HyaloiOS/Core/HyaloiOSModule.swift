#if canImport(UIKit)
// HyaloiOSModule.swift - Top-level coordinator for Hyalo on iOS
// Connects EmacsLifecycle, ChannelBridge, and shared view models.

import UIKit
import SwiftUI
import HyaloShared

/// Main coordinator for Hyalo on iOS.
/// Owns the lifecycle, workspace state, and wires up the channel bridge.
@available(iOS 26.0, *)
@MainActor
@Observable
public final class HyaloiOSModule {

    public static let shared = HyaloiOSModule()

    let lifecycle = EmacsLifecycle()
    let workspace = HyaloWorkspaceState()

    /// The EmacsView handed off from iosterm.m via ios_set_main_emacs_view.
    var emacsView: UIView?
    let editorTabViewModel = EditorTabViewModel()
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
    }
}

#endif // canImport(UIKit)
