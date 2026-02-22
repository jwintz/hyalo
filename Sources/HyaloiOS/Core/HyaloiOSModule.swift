// HyaloiOSModule.swift - Top-level coordinator for Hyalo on iOS
// Connects EmacsLifecycle, ChannelBridge, and shared view models.

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

    private init() {
        // Register the iOS wakeEmacs implementation
        platformWakeEmacs = {
            // TODO: Post event to iOS terminal event loop
            // This will be connected when iosterm.m exposes the wake function
        }
    }

    /// Start Emacs and set up channels.
    public func start() {
        lifecycle.start()
    }
}
