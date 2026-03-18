// Hyalo - Emacs Dynamic Module for macOS IDE Shell
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Module bindings are split across domain-specific extension files:
//   EmacsFFI.swift          — dlsym wrappers, window finders, createModule()
//   Module+WindowSetup.swift — core info, loading proxy, keycast, window setup
//   Module+Navigator.swift   — navigator panel, tabs, data, channels, search, source control
//   Module+Panels.swift      — inspector, utility area, diagnostics
//   Module+Editor.swift      — editor tabs and channel
//   Module+StatusBar.swift   — status bar, toolbar, activity, environment, command palette, packages
//   Module+Appearance.swift  — appearance, theme, terminal palette
//   Module+Build.swift       — build watcher, async build process, build channel

import AppKit
import HyaloShared
import KelyphosKit
import CoreServices
import EmacsSwiftModule
import SwiftUI

/// Hyalo module — macOS IDE shell around Emacs.
///
/// Architecture: NavigationSplitView (3-panel) with SwiftUI .toolbar {}.
/// Channels provide bidirectional Swift↔Emacs Lisp communication.
final class HyaloModule: Module {
    let isGPLCompatible = true
    let version = "2.0.0"

    // Base directory of the Hyalo lisp/ folder.
    // Set once via hyalo-set-base-dir called from hyalo-window--early-setup.
    // Used by LoadingView to locate hyalo-splash.svg.
    static var baseLispDir: String = ""

    // Observable state for the loading proxy window.
    // Decoupled from HyaloWorkspaceState so the proxy can exist independently.
    @available(macOS 26.0, *)
    @MainActor static var loadingState = LoadingState()

    // The standalone proxy window shown while the Emacs window stays invisible.
    // Created in decorateWindow, closed in hyalo-loading-done.
    static var loadingProxyWindow: NSWindow?

    // When true, decorateWindow creates the loading proxy window.
    // Set from Lisp via hyalo-set-needs-bootstrap before decoration.
    // Default false: the proxy is only shown when a bootstrap will occur
    // (e.g., .local or .local/elpa does not exist).
    static var needsBootstrap: Bool = false

    // Channel references (prevent deallocation)
    static var navigatorChannel: Any?
    static var editorTabChannel: Any?
    static var statusChannel: Any?
    static var toolbarChannel: Any?
    static var minibufferChannel: Any?
    static var appearanceChannel: Any?
    static var diagnosticsChannel: Any?
    static var packageChannel: Any?
    static var moduleReloadChannel: Any?
    static var environmentChannel: Any?
    static var buildChannel: Any?

    // Window controllers keyed by Emacs frame window-id (desc_ctr)
    static var controllers: [Int: HyaloWindowController] = [:]

    // Reverse map: ObjectIdentifier(NSWindow) -> Emacs window-id
    // Uses object identity (stable) instead of windowNumber (changes
    // when invisible windows become visible via makeKeyAndOrderFront).
    static var windowToFrameId: [ObjectIdentifier: Int] = [:]

    // Convenience: the controller for the current key/main window
    @available(macOS 26.0, *)
    static var activeController: HyaloWindowController? {
        if let key = NSApp.keyWindow,
           let frameId = windowToFrameId[ObjectIdentifier(key)],
           let c = controllers[frameId] { return c }
        if let main = NSApp.mainWindow,
           let frameId = windowToFrameId[ObjectIdentifier(main)],
           let c = controllers[frameId] { return c }
        return controllers.values.first
    }

    // Convenience: the workspace state for the current key/main window
    @available(macOS 26.0, *)
    static var activeWorkspace: HyaloWorkspaceState? {
        activeController?.workspace
    }

    // All workspaces (for global state propagation)
    @available(macOS 26.0, *)
    static var allWorkspaces: [HyaloWorkspaceState] {
        controllers.values.map(\.workspace)
    }

    // All shell states (for appearance propagation)
    @available(macOS 26.0, *)
    static var allShellStates: [KelyphosShellState] {
        controllers.values.map(\.shellState)
    }

    // Active shell state for the current key/main window
    @available(macOS 26.0, *)
    static var activeShellState: KelyphosShellState? {
        activeController?.shellState
    }

    // All controllers (for global operations)
    @available(macOS 26.0, *)
    static var allControllers: [HyaloWindowController] {
        Array(controllers.values)
    }

    // Legacy compatibility — returns first controller if any
    @available(macOS 26.0, *)
    static var windowController: HyaloWindowController? {
        activeController
    }

    // Search callbacks
    static var onSearchExecute: ((String) -> Void)?
    static var onSearchNavigate: ((String, Int, Int) -> Void)?

    // Appearance callback
    static var onAppearanceModeChanged: ((String) -> Void)?
    static var onOpacityChanged: ((Double) -> Void)?

    // Editor tab channel callbacks (stored globally, wired to each controller)
    static var editorTabSelectCallback: (@MainActor (String) -> Void)?
    static var editorTabCloseCallback: ((String) -> Void)?
    static var editorNavigateBackCallback: (() -> Void)?
    static var editorNavigateForwardCallback: (() -> Void)?

    // Diagnostics channel callback (stored globally, wired to each controller)
    static var diagnosticsNavigateCallback: ((String) -> Void)?

    // Package channel callbacks (stored globally)
    static var packageRefreshCallback: (() -> Void)?
    static var packageUpgradeAllCallback: (() -> Void)?
    static var packageUpgradeSingleCallback: ((String) -> Void)?
    static var packageListCallback: (() -> Void)?

    // Build channel callbacks — forward async build output to Emacs compilation buffer
    static var buildStartCallback: ((String) -> Void)?
    static var buildLogLineCallback: ((String) -> Void)?
    static var buildFinishCallback: ((Bool) -> Void)?

    /// Wire channel callbacks to a controller's view models.
    /// Called at decoration time and after channel setup.
    @available(macOS 26.0, *)
    @MainActor
    static func wireCallbacks(to controller: HyaloWindowController) {
        // Wire up editor tab callbacks if already set up
        // The pre-sync wrapper is created in hyalo-setup-editor-tab-channel
        if let cb = editorTabSelectCallback {
            controller.editorTabViewModel.onTabSelect = cb
        }
        if let cb = editorTabCloseCallback {
            controller.editorTabViewModel.onTabClose = cb
        }
        if let cb = editorNavigateBackCallback {
            controller.editorTabViewModel.onNavigateBack = cb
        }
        if let cb = editorNavigateForwardCallback {
            controller.editorTabViewModel.onNavigateForward = cb
        }
        if let navCb = diagnosticsNavigateCallback {
            controller.utilityAreaViewModel.diagnosticsViewModel.onNavigate = { file, line, col in
                navCb("\(file):\(line):\(col)")
            }
        }
    }

    /// Offset added to opacity for fringe alpha: min(opacity + offset, 1.0).
    /// Must match `hyalo-appearance-fringe-alpha-offset` on the lisp side.
    static let fringeAlphaOffset: Double = 0.10

    /// Set fringe alpha directly via the Emacs C API and force redisplay.
    ///
    /// This bypasses the async channel / lisp event loop entirely,
    /// allowing live updates during SwiftUI slider tracking where the
    /// NS run loop is in event-tracking mode and pipe process filters
    /// cannot run.
    ///
    /// Must be called from the main thread.
    static func setFringeAlpha(_ alpha: Double) {
        if let fn = ns_set_fringe_alpha_override {
            fn(alpha)
        }
    }

    /// Force a full redisplay of all Emacs frames, bypassing the
    /// NSView display pipeline.  Renders directly into the IOSurface
    /// and flushes to VRAM.  Safe during event tracking.
    static func forceRedisplay() {
        if let fn = ns_force_redisplay {
            fn()
        }
    }

    /// Wake the Emacs event loop so it processes pending channel data.
    /// Posts an NSApplicationDefined event to the NSApp queue.
    static func wakeEmacs() {
        if let fn = ns_wake_emacs {
            fn()
        }
    }

    func Init(_ env: EmacsSwiftModule.Environment) throws {
        platformWakeEmacs = { ns_wake_emacs?() }
        Task { @MainActor in
            InspectorAppearanceCallbackWirer.wire()
        }

        try setupProfilingBindings(env)
        try setupCoreBindings(env)
        try setupLoadingBindings(env)
        try setupKeycastBindings(env)
        try setupWindowBindings(env)
        try setupNavigatorBindings(env)
        try setupPanelBindings(env)
        try setupEditorBindings(env)
        try setupStatusBarBindings(env)
        try setupAppearanceBindings(env)
        try setupBuildBindings(env)
        try setupMinibufferBindings(env)
    }
}
