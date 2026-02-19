// Hyalo - Emacs Dynamic Module for macOS IDE Shell
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import CoreServices
import EmacsSwiftModule
import SwiftUI

// C functions in Emacs binary — resolved via dlsym since Hyalo is
// loaded as a dynamic module into the Emacs process.

// Sets fringe alpha override and forces full redisplay of all NS frames.
private let ns_set_fringe_alpha_override: (@convention(c) (Double) -> Void)? = {
    guard let sym = dlsym(dlopen(nil, RTLD_LAZY), "ns_set_fringe_alpha_override") else {
        return nil
    }
    return unsafeBitCast(sym, to: (@convention(c) (Double) -> Void).self)
}()

// Forces full redisplay of all NS frames (renders into IOSurface
// and flushes to VRAM).  Used when changes must be visible during
// event tracking (e.g. appearance mode switch via picker click).
private let ns_force_redisplay: (@convention(c) () -> Void)? = {
    guard let sym = dlsym(dlopen(nil, RTLD_LAZY), "ns_force_redisplay") else {
        return nil
    }
    return unsafeBitCast(sym, to: (@convention(c) () -> Void).self)
}()

// Wakes the Emacs event loop by posting an NSApplicationDefined
// event.  Causes [NSApp run] to return and process pending I/O,
// including pipe process data from Swift channels.
private let ns_wake_emacs: (@convention(c) () -> Void)? = {
    guard let sym = dlsym(dlopen(nil, RTLD_LAZY), "ns_wake_emacs") else {
        return nil
    }
    return unsafeBitCast(sym, to: (@convention(c) () -> Void).self)
}()

/// Find the main Emacs window (not child-frames).
/// Child-frames are positioned off-screen at x < -5000.
func findEmacsWindow() -> NSWindow? {
    func isOnScreen(_ window: NSWindow) -> Bool {
        window.frame.origin.x > -5000
    }

    if let window = NSApp.mainWindow, isOnScreen(window) { return window }
    if let window = NSApp.keyWindow, isOnScreen(window) { return window }

    for window in NSApp.windows where window.isVisible && !window.isMiniaturized && isOnScreen(window) {
        let className = String(describing: type(of: window))
        if className.contains("EmacsWindow") { return window }
    }

    for window in NSApp.windows where isOnScreen(window) {
        let className = String(describing: type(of: window))
        if className.contains("EmacsWindow") { return window }
    }

    return nil
}

/// Find all on-screen EmacsWindows (not child-frames).
func findAllEmacsWindows() -> [NSWindow] {
    NSApp.windows.filter { window in
        window.frame.origin.x > -5000
        && String(describing: type(of: window)).contains("EmacsWindow")
    }
}

/// Find the first undecorated EmacsWindow (not yet in the reverse map).
@available(macOS 26.0, *)
func findUndecoratedEmacsWindow() -> NSWindow? {
    for window in findAllEmacsWindows() {
        if HyaloModule.windowToFrameId[ObjectIdentifier(window)] == nil {
            return window
        }
    }
    return nil
}

/// Extract the Emacs NSView from the window's content view hierarchy.
func extractEmacsView(from contentView: NSView) -> NSView? {
    let className = String(describing: type(of: contentView))
    if className.contains("EmacsView") { return contentView }

    for subview in contentView.subviews {
        let subClassName = String(describing: type(of: subview))
        if subClassName.contains("EmacsView") || subview.acceptsFirstResponder {
            return subview
        }
        if let found = extractEmacsView(from: subview) {
            return found
        }
    }

    return nil
}

/// Hyalo module — macOS IDE shell around Emacs.
///
/// Architecture: NavigationSplitView (3-panel) with SwiftUI .toolbar {}.
/// Channels provide bidirectional Swift↔Emacs Lisp communication.
final class HyaloModule: Module {
    let isGPLCompatible = true
    private let version = "2.0.0"

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
    static var commandPaletteChannel: Any?
    static var appearanceChannel: Any?
    static var diagnosticsChannel: Any?
    static var packageChannel: Any?
    static var sourceControlChannel: Any?
    static var moduleReloadChannel: Any?

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

    // Source control channel callbacks (stored globally)
    static var sourceControlShowCommitCallback: ((String) -> Void)?
    static var sourceControlShowDiffCallback: ((String) -> Void)?

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

        // MARK: - Info

        try env.defun("hyalo-version") { [version] in version }

        // MARK: - Direct Rendering

        try env.defun("hyalo-force-redisplay",
            with: """
            Force immediate redisplay of all NS frames.
            Bypasses the NSView display pipeline, rendering directly into
            the IOSurface and flushing to VRAM.  Use after changes that
            must be visible even when the Emacs view is not the key window.
            """
        ) { () -> Bool in
            HyaloModule.forceRedisplay()
            return true
        }

        try env.defun("hyalo-corner-radius") {
            Double(HyaloManager.cornerRadius)
        }

        try env.defun("hyalo-set-project-name",
            with: "Set the project name displayed in the toolbar."
        ) { (env: EmacsSwiftModule.Environment, name: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.projectName = name
                    }
                    ToolbarManager.shared.viewModel.projectName = name
                }
                return true
            }
            return false
        }

        // MARK: - Loading Proxy

        try env.defun("hyalo-set-needs-bootstrap",
            with: "Set whether a package bootstrap is needed. When true, the loading proxy window is shown during init. Call before hyalo-navigation-setup."
        ) { (env: EmacsSwiftModule.Environment, needed: Bool) throws -> Bool in
            HyaloModule.needsBootstrap = needed
            return true
        }

        try env.defun("hyalo-set-base-dir",
            with: "Set the Hyalo lisp/ directory path. Called once during early setup so LoadingView can locate hyalo-splash.svg."
        ) { (env: EmacsSwiftModule.Environment, path: String) throws -> Bool in
            HyaloModule.baseLispDir = path
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    HyaloModule.loadingState.lispDir = path
                }
            }
            return true
        }

        try env.defun("hyalo-set-loading-message",
            with: "Update the proxy window message shown while init.el runs. MSG is a short string describing the current init step."
        ) { (env: EmacsSwiftModule.Environment, msg: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    HyaloModule.loadingState.message = msg
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-loading-done",
            with: """
            Close the loading proxy window.  Called at the end of hyalo-window--post-setup.
            Does NOT call makeKeyAndOrderFront or NSApp.activate — those must be done by
            the Lisp caller via (make-frame-visible) so Emacs can update its internal
            frame visibility state before AppKit triggers display callbacks.
            """
        ) { () -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    NSLog("[Hyalo:Nav] hyalo-loading-done: closing proxy, %d workspaces",
                          HyaloModule.allWorkspaces.count)
                    // Mark all workspaces as initialized
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.isLoading = false
                    }
                    // Remove the proxy from screen immediately but do NOT
                    // close() it yet.  close() deallocates the window while
                    // CoreAnimation still holds references to its transition
                    // animations (_NSWindowTransformAnimation).  When CA
                    // flushes the transaction seconds later, the animation
                    // dealloc accesses the freed window → SIGSEGV.
                    //
                    // orderOut removes it from the screen instantly (no
                    // visible delay), then we nil the reference after 1s
                    // to let CA finish its transaction.
                    if let proxy = HyaloModule.loadingProxyWindow {
                        proxy.orderOut(nil)
                        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) {
                            // Reference was kept alive by loadingProxyWindow;
                            // dropping it now lets ARC deallocate cleanly
                            // after CA is done.
                            HyaloModule.loadingProxyWindow = nil
                        }
                    }
                    NSLog("[Hyalo:Nav] hyalo-loading-done: proxy closed")
                    // Frame reveal intentionally omitted.
                    // makeKeyAndOrderFront from Swift (bypassing Emacs's
                    // ns_make_frame_visible) crashes because AppKit fires
                    // display callbacks before Emacs's internal frame state
                    // is updated.  The Lisp caller must call (make-frame-visible).
                }
                return true
            }
            return false
        }

        // MARK: - Keycast

        try env.defun("hyalo-update-keycast",
            with: """
            Update the keycast toolbar pill with the current key binding and command.
            KEY is the key description string (e.g. \"C-x C-f\").
            COMMAND is the command name string (e.g. \"find-file\").
            """
        ) { (env: EmacsSwiftModule.Environment, key: String, command: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    let vm = ToolbarManager.shared.viewModel
                    vm.keycastKey = key
                    vm.keycastCommand = command
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-keycast-visible",
            with: "Set keycast toolbar pill visibility. VISIBLE is t or nil."
        ) { (env: EmacsSwiftModule.Environment, visible: Bool) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ToolbarManager.shared.viewModel.keycastVisible = visible
                }
                return true
            }
            return false
        }

        // MARK: - Appearance

        try env.defun(
            "hyalo-get-system-appearance",
            with: "Get system appearance: \"light\" or \"dark\"."
        ) { () -> String in
            HyaloManager.shared.getSystemAppearance()
        }

        try env.defun(
            "hyalo-set-window-appearance",
            with: "Set window appearance: \"light\", \"dark\", or \"auto\"."
        ) { (env: EmacsSwiftModule.Environment, appearance: String) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.setWindowAppearance(appearance, for: window)
            }
            return true
        }

        // MARK: - IDE Window Setup

        try env.defun(
            "hyalo-navigation-setup",
            with: """
            Setup the IDE shell with NavigationSplitView on the initial frame.
            FRAME-ID is the Emacs frame window-id (from frame parameter 'window-id').
            Creates a 3-panel layout: Navigator | Editor | Inspector.
            Returns t on success, nil on failure.
            """
        ) { (env: EmacsSwiftModule.Environment, frameId: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                // Called from the Emacs main thread.  Run decoration synchronously
                // rather than via DispatchQueue.main.async so the window hierarchy
                // is fully set up before this function returns to Lisp.
                // Async dispatch caused decorateWindow to run during a subsequent
                // sit-for, interleaving with Emacs drawing code and causing a segfault.
                MainActor.assumeIsolated {
                    guard let window = findEmacsWindow() else { return }
                    HyaloModule.decorateWindow(window, frameId: frameId)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-decorate-frame",
            with: """
            Decorate an Emacs frame with the Hyalo IDE shell.
            FRAME-ID is the Emacs frame window-id (from frame parameter 'window-id').
            Finds the undecorated EmacsWindow and decorates it.
            Returns t on success, nil on failure.
            """
        ) { (env: EmacsSwiftModule.Environment, frameId: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    if HyaloModule.controllers[frameId] != nil {
                        NSLog("[Hyalo:Nav] decorate-frame: frame %d already decorated", frameId)
                        return
                    }
                    guard let window = findUndecoratedEmacsWindow() else {
                        NSLog("[Hyalo:Nav] decorate-frame: no undecorated window for frame %d", frameId)
                        return
                    }
                    HyaloModule.decorateWindow(window, frameId: frameId)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-undecorate-frame",
            with: """
            Remove Hyalo decoration from an Emacs frame.
            FRAME-ID is the Emacs frame window-id (from frame parameter 'window-id').
            """
        ) { (env: EmacsSwiftModule.Environment, frameId: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    HyaloModule.undecorateWindow(frameId)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-window-controller-ready-p",
            with: "Return t if any window controller has been created, nil otherwise."
        ) { () -> Bool in
            if #available(macOS 26.0, *) {
                return !HyaloModule.controllers.isEmpty
            }
            return false
        }

        try env.defun(
            "hyalo-frame-decorated-p",
            with: """
            Return t if the frame with FRAME-ID has been decorated.
            FRAME-ID is the Emacs frame window-id (from frame parameter 'window-id').
            """
        ) { (env: EmacsSwiftModule.Environment, frameId: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                return HyaloModule.controllers[frameId] != nil
            }
            return false
        }

        try env.defun(
            "hyalo-frame-controller-ready-p",
            with: """
            Return t if the frame with FRAME-ID has a fully set up controller.
            FRAME-ID is the Emacs frame window-id (from frame parameter 'window-id').
            Returns nil if the controller does not exist or setup() has not completed.
            """
        ) { (env: EmacsSwiftModule.Environment, frameId: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                return HyaloModule.controllers[frameId]?.isSetUp == true
            }
            return false
        }

        try env.defun(
            "hyalo-decorated-frame-ids",
            with: "Return a list of window IDs for all decorated frames."
        ) { (env: EmacsSwiftModule.Environment) throws -> [Int] in
            if #available(macOS 26.0, *) {
                return Array(HyaloModule.controllers.keys)
            }
            return []
        }

        // MARK: - Navigator Panel

        try env.defun("hyalo-navigator-toggle",
            with: "Toggle the navigator (left sidebar) visibility."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.toggleNavigator()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-show",
            with: "Show the navigator (left sidebar)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.setNavigatorVisible(true)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-hide",
            with: "Hide the navigator (left sidebar)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.setNavigatorVisible(false)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-visible-p",
            with: "Return t if navigator is visible, nil otherwise."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let controller = HyaloModule.activeController else { return false }
                return controller.isNavigatorVisible
            }
            return false
        }

        // MARK: - Navigator Tab Selection

        try env.defun("hyalo-navigator-select-tab",
            with: """
            Select navigator tab by 1-based INDEX.
            If already on that tab and navigator is visible, toggle navigator off.
            This is the Xcode Cmd-1/2/3/4 behavior.
            """
        ) { (env: EmacsSwiftModule.Environment, index: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.selectNavigatorTab(index)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-tab-index",
            with: "Return the current navigator tab index (1-based), or 0 if none."
        ) { (env: EmacsSwiftModule.Environment) throws -> Int in
            if #available(macOS 26.0, *) {
                guard let controller = HyaloModule.activeController else { return 0 }
                return controller.navigatorTabIndex
            }
            return 0
        }

        // MARK: - Inspector Panel

        try env.defun("hyalo-inspector-toggle",
            with: "Toggle the inspector (right sidebar) visibility."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.toggleInspector()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-inspector-show",
            with: "Show the inspector (right sidebar)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.setInspectorVisible(true)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-inspector-hide",
            with: "Hide the inspector (right sidebar)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.setInspectorVisible(false)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-inspector-visible-p",
            with: "Return t if inspector is visible, nil otherwise."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let controller = HyaloModule.activeController else { return false }
                return controller.isInspectorVisible
            }
            return false
        }

        // MARK: - Inspector Tab Selection

        try env.defun("hyalo-inspector-select-tab",
            with: """
            Select inspector tab by 1-based INDEX.
            If already on that tab and inspector is visible, toggle inspector off.
            This is the Xcode Cmd-Option-1/2/3 behavior.
            """
        ) { (env: EmacsSwiftModule.Environment, index: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.selectInspectorTab(index)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-inspector-tab-index",
            with: "Return the current inspector tab index (1-based), or 0 if none."
        ) { (env: EmacsSwiftModule.Environment) throws -> Int in
            if #available(macOS 26.0, *) {
                guard let controller = HyaloModule.activeController else { return 0 }
                return controller.inspectorTabIndex
            }
            return 0
        }

        // MARK: - Utility Area

        try env.defun("hyalo-utility-area-toggle",
            with: """
            Toggle the utility area (bottom panel) visibility.
            When shown, the terminal gains keyboard focus.
            When hidden, the Emacs view regains focus.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    guard let controller = HyaloModule.activeController else { return }
                    let workspace = controller.workspace
                    let willShow = !workspace.utilityAreaVisible
                    withAnimation(.easeInOut(duration: 0.15)) {
                        workspace.utilityAreaVisible = willShow
                    }
                    if willShow {
                        // Defer focus until the view is laid out
                        DispatchQueue.main.async { controller.focusTerminal() }
                    } else {
                        controller.focusEmacs()
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-utility-area-show",
            with: """
            Show the utility area (bottom panel).
            The terminal gains keyboard focus.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    guard let controller = HyaloModule.activeController else { return }
                    withAnimation(.easeInOut(duration: 0.15)) {
                        controller.workspace.utilityAreaVisible = true
                    }
                    DispatchQueue.main.async { controller.focusTerminal() }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-utility-area-hide",
            with: """
            Hide the utility area (bottom panel).
            The Emacs view regains keyboard focus.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    guard let controller = HyaloModule.activeController else { return }
                    withAnimation(.easeInOut(duration: 0.15)) {
                        controller.workspace.utilityAreaVisible = false
                    }
                    controller.focusEmacs()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-utility-area-select-tab",
            with: """
            Select utility area tab by 1-based INDEX.
            If already on that tab and utility area is visible, toggle it off.
            This is the Cmd-Option-Shift-1/2 behavior.
            """
        ) { (env: EmacsSwiftModule.Environment, index: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.selectUtilityAreaTab(index)
                }
                return true
            }
            return false
        }

        // MARK: - Status Bar

        try env.defun("hyalo-status-update",
            with: """
            Update the status bar with current buffer information.
            Includes modeline segments and minor modes.
            """
        ) { (env: EmacsSwiftModule.Environment,
             line: Int, column: Int, mode: String,
             encoding: String?, lineEnding: String?,
             indentStyle: String?, indentWidth: Int?,
             fileType: String?, fileSize: String?,
             minorModesJson: String?,
             modelineLHS: String?,
             modelineRHS: String?) throws -> Bool in
            if #available(macOS 26.0, *) {
                // Parse minor modes JSON array if provided
                var minorModes: [String]?
                if let json = minorModesJson, let data = json.data(using: .utf8) {
                    minorModes = try? JSONDecoder().decode([String].self, from: data)
                }
                DispatchQueue.main.async {
                    StatusBarManager.shared.updateStatus(
                        line: line, column: column, mode: mode,
                        encoding: encoding, lineEnding: lineEnding,
                        indentStyle: indentStyle, indentWidth: indentWidth,
                        fileType: fileType, fileSize: fileSize,
                        minorModes: minorModes,
                        modelineLHS: modelineLHS,
                        modelineRHS: modelineRHS
                    )
                }
                return true
            }
            return false
        }

        // MARK: - Navigator Data

        try env.defun("hyalo-navigator-update-buffers",
            with: "Update the navigator buffer list from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                do {
                    let buffers = try JSONDecoder().decode([BufferInfo].self, from: data)
                    DispatchQueue.main.async {
                        NavigatorManager.shared.updateBufferList(buffers)
                    }
                    return true
                } catch { return false }
            }
            return false
        }

        try env.defun("hyalo-navigator-update-file-tree",
            with: "Update the navigator file tree from JSON. (Legacy — prefer hyalo-navigator-set-project-root)"
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            // Legacy path: kept for backward compatibility during transition
            if #available(macOS 26.0, *) {
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-set-project-root",
            with: "Set the project root directory. Rebuilds file tree in Swift."
        ) { (env: EmacsSwiftModule.Environment, rootPath: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.setProjectRoot(rootPath)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-refresh-file-tree",
            with: "Refresh the file tree from the current project root."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.refreshFileTree()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-set-active-buffer",
            with: "Set the currently active buffer in the navigator."
        ) { (env: EmacsSwiftModule.Environment, bufferName: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.setActiveBuffer(bufferName)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-set-active-file",
            with: "Set the currently active file in the navigator."
        ) { (env: EmacsSwiftModule.Environment, filePath: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.setActiveFile(filePath)
                }
                return true
            }
            return false
        }

        // MARK: - Navigator Channel

        try env.defun("hyalo-setup-navigator-channel",
            with: "Setup the async channel for navigator callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-navigator")
                HyaloModule.navigatorChannel = channel

                let bufferSelectCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, bufferName: String) in
                    // Use wrapper that pushes state to all UI components
                    try env.funcall("hyalo-channels--handle-switch-buffer", with: bufferName)
                }

                let bufferCloseCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, bufferName: String) in
                    try env.funcall("kill-buffer", with: bufferName)
                }

                let fileSelectCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, filePath: String) in
                    // Use wrapper that pushes state to all UI components
                    try env.funcall("hyalo-channels--handle-find-file", with: filePath)
                }

                DispatchQueue.main.async {
                    NavigatorManager.shared.onBufferSelect = bufferSelectCallback
                    NavigatorManager.shared.onBufferClose = bufferCloseCallback
                    NavigatorManager.shared.onFileSelect = fileSelectCallback
                }

                return true
            }
            return false
        }

        // MARK: - Editor Tabs

        try env.defun("hyalo-update-editor-tabs",
            with: "Update the editor tab bar from JSON. Pushes to all frames."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                do {
                    let tabs = try JSONDecoder().decode([EditorTab].self, from: data)
                    MainActor.assumeIsolated {
                        for controller in HyaloModule.allControllers {
                            controller.editorTabViewModel.updateTabs(tabs)
                        }
                    }
                    return true
                } catch { return false }
            }
            return false
        }

        try env.defun("hyalo-select-editor-tab",
            with: "Select an editor tab by buffer name in the active frame. Called from Emacs when file opens."
        ) { (env: EmacsSwiftModule.Environment, bufferName: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    if let wc = HyaloModule.activeController {
                        // Use onTabSelected which checks pendingTabId to skip stale callbacks
                        wc.editorTabViewModel.onTabSelected(bufferName)
                    }
                }
                return true
            }
            return false
        }

        // MARK: - Editor Tab Channel

        try env.defun("hyalo-setup-editor-tab-channel",
            with: "Setup the async channel for editor tab callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-editor-tabs")
                HyaloModule.editorTabChannel = channel

                // window-buffer-change-functions is the single sync point.
                // Swift clicks send commands to Emacs; the buffer switch
                // triggers the hook which pushes state back via
                // hyalo-sync--push.  wakeEmacs() is required because the
                // click goes to SwiftUI, not the Emacs view — without it,
                // Emacs never reads the channel pipe (see FINDINGS.md).

                let tabSelectCallback: @MainActor (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, bufferName: String) in
                    try env.funcall("hyalo-channels--handle-switch-buffer", with: bufferName)
                }

                let tabCloseCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, bufferName: String) in
                    try env.funcall("hyalo-channels--handle-close-tab", with: bufferName)
                }

                let navigateBackCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("previous-buffer")
                    try env.funcall("hyalo-sync--push")
                }

                let navigateForwardCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("next-buffer")
                    try env.funcall("hyalo-sync--push")
                }

                // Store callbacks globally
                HyaloModule.editorTabSelectCallback = tabSelectCallback
                HyaloModule.editorTabCloseCallback = tabCloseCallback
                HyaloModule.editorNavigateBackCallback = navigateBackCallback
                HyaloModule.editorNavigateForwardCallback = navigateForwardCallback

                // Wire to all existing controllers
                MainActor.assumeIsolated {
                    for controller in HyaloModule.allControllers {
                        controller.editorTabViewModel.onTabSelect = tabSelectCallback
                        controller.editorTabViewModel.onTabClose = tabCloseCallback
                        controller.editorTabViewModel.onNavigateBack = navigateBackCallback
                        controller.editorTabViewModel.onNavigateForward = navigateForwardCallback
                    }
                }

                return true
            }
            return false
        }

        // MARK: - Status Channel

        try env.defun("hyalo-setup-status-channel",
            with: "Setup the async channel for status bar callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-status")
                HyaloModule.statusChannel = channel

                let encodingCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, encoding: String) in
                    try env.funcall("hyalo-status--set-encoding", with: encoding)
                }

                let lineEndingCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, lineEnding: String) in
                    try env.funcall("hyalo-status--set-line-ending", with: lineEnding)
                }

                let indentStyleCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, indentJson: String) in
                    try env.funcall("hyalo-status--set-indent", with: indentJson)
                }

                DispatchQueue.main.async {
                    let vm = StatusBarManager.shared.viewModel
                    vm.onEncodingChange = encodingCallback
                    vm.onLineEndingChange = lineEndingCallback
                    vm.onIndentStyleChange = { style, width in
                        indentStyleCallback("\(style):\(width)")
                    }
                }

                return true
            }
            return false
        }

        // MARK: - Toolbar Data

        try env.defun("hyalo-update-branch-info",
            with: "Update the toolbar branch picker from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    ToolbarManager.shared.updateBranchInfo(from: data)
                }
                return true
            }
            return false
        }

        // MARK: - Activity System

        try env.defun("hyalo-activity-upsert",
            with: """
            Create or update an activity in the toolbar activity viewer.
            ID is a unique string key for this activity.
            KIND is one of: \"native-compilation\", \"module-compilation\", \"package-installation\".
            TITLE is the primary text shown inline.
            MESSAGE is optional secondary text shown in the popover.
            PROGRESS is a float 0.0-1.0, or nil for indeterminate.
            IS-ACTIVE is t for in-progress, nil for finished.
            """
        ) { (env: EmacsSwiftModule.Environment, id: String, kind: String,
             title: String, message: String?, progress: Double?,
             isActive: Bool) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    guard let activityKind = ActivityKind(rawValue: kind) else { return }
                    ActivityManager.shared.upsert(
                        id: id, kind: activityKind, title: title,
                        message: message ?? "", progress: progress,
                        isActive: isActive
                    )
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-finish",
            with: """
            Mark an activity as finished.
            ID is the activity key.  MESSAGE is an optional completion message.
            """
        ) { (env: EmacsSwiftModule.Environment, id: String, message: String?) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.finish(id: id, message: message ?? "")
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-remove",
            with: "Remove an activity from the activity viewer."
        ) { (env: EmacsSwiftModule.Environment, id: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.remove(id: id)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-remove-after-delay",
            with: """
            Remove an activity after DELAY seconds.
            """
        ) { (env: EmacsSwiftModule.Environment, id: String, delay: Double) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.removeAfterDelay(id: id, delay: delay)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-append-log",
            with: """
            Append a log line to an activity's detail log.
            ID is the activity key.  LINE is the text to append.
            """
        ) { (env: EmacsSwiftModule.Environment, id: String, line: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.appendLog(id: id, line: line)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-clear-log",
            with: "Clear the log lines for an activity."
        ) { (env: EmacsSwiftModule.Environment, id: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.clearLog(id: id)
                }
                return true
            }
            return false
        }

        // MARK: - Module Build Watcher

        try env.defun("hyalo-start-build-watcher",
            with: """
            Start watching the .build/ directory for swift build completions.
            BASE-DIR is the project root (where Package.swift lives).
            Uses kqueue file system events — no polling.
            """
        ) { (env: EmacsSwiftModule.Environment, baseDir: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                HyaloModule.startBuildWatcher(baseDir: baseDir)
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-set-module-reload",
            with: """
            Set the module reload callback for the activity viewer.
            When the user clicks the reload button after a module build,
            this function is called on the Emacs side.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-module-reload")
                HyaloModule.moduleReloadChannel = channel
                let reloadCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-rebuild-and-reload")
                }
                MainActor.assumeIsolated {
                    ActivityManager.shared.onModuleReload = reloadCallback
                }
                return true
            }
            return false
        }

        // MARK: - Async Build Process

        try env.defun("hyalo-async-build",
            with: """
            Run `swift build` asynchronously with full lifecycle tracking.
            BASE-DIR is the project root.  CONFIG is "debug" or "release".
            The activity viewer shows start, real-time build logs, and
            completion with a reload button.  No polling — uses Process
            pipes for stdout/stderr streaming.
            Returns t immediately (build runs in background).
            """
        ) { (env: EmacsSwiftModule.Environment, baseDir: String, config: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                HyaloModule.runAsyncBuild(baseDir: baseDir, config: config)
                return true
            }
            return false
        }

        // MARK: - Toolbar Channel

        try env.defun("hyalo-setup-toolbar-channel",
            with: "Setup the async channel for toolbar callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-toolbar")
                HyaloModule.toolbarChannel = channel

                let branchSwitchCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, branch: String) in
                    try env.funcall("hyalo-channels--handle-branch-switch", with: branch)
                }

                DispatchQueue.main.async {
                    ToolbarManager.shared.viewModel.onBranchSwitch = branchSwitchCallback
                }

                return true
            }
            return false
        }

        // MARK: - Package Management

        try env.defun("hyalo-update-package-status",
            with: """
            Update the package management toolbar from JSON.
            JSON-DATA has keys: status ("idle"/"refreshing"/"upgrading"),
            upgradable (array of {name, installed, available}).
            """
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    ToolbarManager.shared.updatePackageStatus(from: data)
                }
                return true
            }
            return false
        }

        // MARK: - Package Channel

        try env.defun("hyalo-setup-package-channel",
            with: "Setup the async channel for package management callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-package")
                HyaloModule.packageChannel = channel

                let refreshCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-channels--handle-package-refresh")
                }

                let upgradeAllCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-channels--handle-package-upgrade-all")
                }

                let upgradeSingleCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, name: String) in
                    try env.funcall("hyalo-channels--handle-package-upgrade-single", with: name)
                }

                let listCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-channels--handle-package-list")
                }

                // Store globally
                HyaloModule.packageRefreshCallback = refreshCallback
                HyaloModule.packageUpgradeAllCallback = upgradeAllCallback
                HyaloModule.packageUpgradeSingleCallback = upgradeSingleCallback
                HyaloModule.packageListCallback = listCallback

                // Wire to toolbar view model
                DispatchQueue.main.async {
                    let vm = ToolbarManager.shared.viewModel
                    vm.onPackageRefresh = refreshCallback
                    vm.onPackageUpgradeAll = upgradeAllCallback
                    vm.onPackageUpgradeSingle = upgradeSingleCallback
                    vm.onPackageList = listCallback
                }

                return true
            }
            return false
        }

        // MARK: - Appearance

        try env.defun("hyalo-set-vibrancy-material",
            with: "Set the vibrancy material level for the main content area."
        ) { (env: EmacsSwiftModule.Environment, material: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let mat = VibrancyMaterial(rawValue: material) else { return }
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.vibrancyMaterial = mat
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-background-color",
            with: "Set the workspace background color. Alpha is preserved from UserDefaults."
        ) { (env: EmacsSwiftModule.Environment, hexColor: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let color = NSColor(hexString: hexColor) else { return }
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.backgroundColor = color
                    }
                    for controller in HyaloModule.allControllers {
                        controller.updateLayerBackgrounds()
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-workspace-appearance",
            with: """
            Apply NSWindow appearance (light/dark chrome) without
            changing the mode selector preference.  The picker value
            is only changed by explicit user interaction.
            """
        ) { (env: EmacsSwiftModule.Environment, appearance: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    let nsAppearance: NSAppearance?
                    switch appearance {
                    case "light": nsAppearance = NSAppearance(named: .aqua)
                    case "dark": nsAppearance = NSAppearance(named: .darkAqua)
                    default: nsAppearance = nil
                    }
                    for controller in HyaloModule.allControllers {
                        controller.applyAppearance(nsAppearance)
                    }
                }
                return true
            }
            return false
        }

        // MARK: - Appearance Panel

        try env.defun("hyalo-show-appearance-panel",
            with: "Show the appearance settings in the inspector (tab 3)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    if let wc = HyaloModule.activeController {
                        wc.selectInspectorTab(3)
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-current-theme-name",
            with: "Set the current theme name displayed in the appearance panel."
        ) { (env: EmacsSwiftModule.Environment, name: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.currentThemeName = name
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-color-theme",
            with: """
            Set the color theme for a variant (\"light\" or \"dark\").
            VARIANT is \"light\" or \"dark\".
            JSON-COLORS is a JSON object with keys: background, backgroundDim,
            foreground, foregroundDim, accent, accentSecondary, error, warning,
            success, link, string, comment, constant, border, selection.
            """
        ) { (env: EmacsSwiftModule.Environment, variant: String, jsonColors: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let data = jsonColors.data(using: .utf8),
                          let dict = try? JSONSerialization.jsonObject(with: data) as? [String: String]
                    else { return }
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.colorTheme.update(variant: variant, from: dict)
                    }
                }
                return true
            }
            return false
        }

        // MARK: - Terminal Palette

        try env.defun("hyalo-set-terminal-palette",
            with: """
            Set the terminal ANSI palette from JSON.
            JSON is a JSON object with keys: foreground, background, cursor,
            ansi (array of 16 hex color strings).
            """
        ) { (env: EmacsSwiftModule.Environment, json: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let data = json.data(using: .utf8),
                          let dict = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
                    else { return }
                    let palette = TerminalPalette.shared
                    if let fg = dict["foreground"] as? String { palette.foreground = fg }
                    if let bg = dict["background"] as? String { palette.background = bg }
                    if let cursor = dict["cursor"] as? String { palette.cursor = cursor }
                    if let ansi = dict["ansi"] as? [String], ansi.count == 16 {
                        palette.ansiColors = ansi
                    }
                    palette.version += 1
                }
                return true
            }
            return false
        }

        // MARK: - Command Palette Channel

        try env.defun("hyalo-setup-command-palette-channel",
            with: "Setup the async channel for command palette callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-command-palette")
                HyaloModule.commandPaletteChannel = channel

                let openFileCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, path: String) in
                    try env.funcall("hyalo-channels--handle-open-file", with: path)
                }

                let executeCommandCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, command: String) in
                    try env.funcall("hyalo-channels--handle-execute-command", with: command)
                }

                MainActor.assumeIsolated {
                    let manager = CommandPaletteManager.shared
                    manager.onOpenFile = openFileCallback
                    manager.onExecuteCommand = executeCommandCallback
                }

                return true
            }
            return false
        }

        // MARK: - Command Palette Data

        try env.defun("hyalo-update-open-quickly-items",
            with: """
            Update the Open Quickly file list.
            JSON-DATA is a JSON array of file objects with keys: name, path, icon, relativePath.
            """
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    CommandPaletteManager.shared.updateOpenQuicklyItems(from: data)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-update-command-list",
            with: """
            Update the Command Palette command list.
            JSON-DATA is a JSON array of command objects with keys: name, description, icon, keybinding, category.
            """
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    CommandPaletteManager.shared.updateCommandList(from: data)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-show-open-quickly",
            with: "Show the Open Quickly panel."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    CommandPaletteManager.shared.showOpenQuickly()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-show-command-palette",
            with: "Show the Command Palette panel."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    CommandPaletteManager.shared.showCommandPalette()
                }
                return true
            }
            return false
        }

        // MARK: - Inspector Data

        try env.defun("hyalo-update-file-info",
            with: "Update the inspector file info panel from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    InspectorManager.shared.updateFileInfo(from: data)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-update-git-history",
            with: "Update the inspector git history panel from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    InspectorManager.shared.updateGitHistory(from: data)
                }
                return true
            }
            return false
        }

        // MARK: - Search

        try env.defun("hyalo-update-search-results",
            with: "Update search results in the navigator from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else {
                    NSLog("[Hyalo:Search] hyalo-update-search-results: failed to convert JSON to data")
                    return false
                }
                NSLog("[Hyalo:Search] hyalo-update-search-results: received %d bytes", data.count)
                do {
                    let results = try JSONDecoder().decode([SearchResult].self, from: data)
                    NSLog("[Hyalo:Search] hyalo-update-search-results: decoded %d results", results.count)
                    DispatchQueue.main.async {
                        NavigatorManager.shared.updateSearchResults(results)
                    }
                    return true
                } catch {
                    NSLog("[Hyalo:Search] hyalo-update-search-results: decode error: %@", error.localizedDescription)
                    return false
                }
            }
            return false
        }

        try env.defun("hyalo-setup-search-channel",
            with: "Setup the async channel for search callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-search")

                let searchCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, query: String) in
                    try env.funcall("hyalo-channels--handle-search", with: query)
                }

                let navigateCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, location: String) in
                    try env.funcall("hyalo-channels--handle-search-navigate", with: location)
                }

                HyaloModule.onSearchExecute = searchCallback
                HyaloModule.onSearchNavigate = { file, line, col in
                    navigateCallback("\(file):\(line):\(col)")
                }

                DispatchQueue.main.async {
                    NavigatorManager.shared.onSearchExecute = searchCallback
                    NavigatorManager.shared.onSearchResultSelect = { file, line, col in
                        navigateCallback("\(file):\(line):\(col)")
                    }
                }

                return true
            }
            return false
        }

        try env.defun("hyalo-update-search-status",
            with: "Update search status counts in the find navigator."
        ) { (env: EmacsSwiftModule.Environment, resultCount: Int, fileCount: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                NSLog("[Hyalo:Search] hyalo-update-search-status: %d results, %d files", resultCount, fileCount)
                DispatchQueue.main.async {
                    NavigatorManager.shared.updateSearchStatus(resultCount: resultCount, fileCount: fileCount)
                }
                return true
            }
            return false
        }

        // MARK: - Appearance Channel

        try env.defun("hyalo-setup-appearance-channel",
            with: "Setup the async channel for appearance mode and opacity callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-appearance")

                let modeChangedCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, mode: String) in
                    try env.funcall("hyalo-channels--handle-appearance-mode", with: mode)
                }

                let opacityChangedCallback: (Double) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, opacity: Double) in
                    try env.funcall("hyalo-channels--handle-opacity-change", with: opacity)
                }

                HyaloModule.appearanceChannel = channel
                HyaloModule.onAppearanceModeChanged = modeChangedCallback
                HyaloModule.onOpacityChanged = opacityChangedCallback

                // Push the persisted opacity so Emacs derives the
                // correct fringe alpha at startup.
                MainActor.assumeIsolated {
                    if let workspace = HyaloModule.activeWorkspace {
                        let opacity = Double(workspace.backgroundAlpha)
                        opacityChangedCallback(opacity)
                        let fringeAlpha = min(opacity + HyaloModule.fringeAlphaOffset, 1.0)
                        HyaloModule.setFringeAlpha(fringeAlpha)
                    }
                }

                return true
            }
            return false
        }

        // MARK: - Diagnostics

        try env.defun("hyalo-update-diagnostics",
            with: """
            Update the diagnostics panel from JSON.
            JSON-DATA is a JSON array of diagnostic objects with keys:
            id, file, line, column, severity, message, source.
            """
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    if let wc = HyaloModule.activeController {
                        wc.utilityAreaViewModel.diagnosticsViewModel.update(from: data)
                    }
                }
                return true
            }
            return false
        }

        // MARK: - Diagnostics Channel

        try env.defun("hyalo-setup-diagnostics-channel",
            with: "Setup the async channel for diagnostics navigation callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-diagnostics")
                HyaloModule.diagnosticsChannel = channel

                let navigateCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, location: String) in
                    try env.funcall("hyalo-channels--handle-diagnostic-navigate", with: location)
                }

                // Store globally and wire to all existing controllers
                HyaloModule.diagnosticsNavigateCallback = navigateCallback

                MainActor.assumeIsolated {
                    for controller in HyaloModule.allControllers {
                        controller.utilityAreaViewModel.diagnosticsViewModel.onNavigate = { file, line, col in
                            navigateCallback("\(file):\(line):\(col)")
                        }
                    }
                }

                return true
            }
            return false
        }

        // MARK: - Source Control Data

        try env.defun("hyalo-update-changed-files",
            with: "Update the source control changed files list from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                do {
                    let files = try JSONDecoder().decode([GitChangedFile].self, from: data)
                    DispatchQueue.main.async {
                        NavigatorManager.shared.updateChangedFiles(files)
                    }
                    return true
                } catch { return false }
            }
            return false
        }

        try env.defun("hyalo-update-commit-history",
            with: "Update the source control commit history from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                do {
                    let commits = try JSONDecoder().decode([GitCommitEntry].self, from: data)
                    DispatchQueue.main.async {
                        NavigatorManager.shared.updateCommitHistory(commits)
                    }
                    return true
                } catch { return false }
            }
            return false
        }

        // MARK: - Source Control Channel

        try env.defun("hyalo-setup-source-control-channel",
            with: "Setup the async channel for source control navigation callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-source-control")
                HyaloModule.sourceControlChannel = channel

                let showCommitCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, hash: String) in
                    try env.funcall("hyalo-channels--handle-show-commit", with: hash)
                }

                let showDiffCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, path: String) in
                    try env.funcall("hyalo-channels--handle-show-diff", with: path)
                }

                HyaloModule.sourceControlShowCommitCallback = showCommitCallback
                HyaloModule.sourceControlShowDiffCallback = showDiffCallback

                DispatchQueue.main.async {
                    NavigatorManager.shared.onCommitSelect = showCommitCallback
                    NavigatorManager.shared.onChangedFileSelect = showDiffCallback
                    InspectorManager.shared.onCommitSelect = showCommitCallback
                }

                return true
            }
            return false
        }
    }

    // MARK: - Module Build File Watcher

    /// Single FSEventStream watching `.build/` for build activity.
    private static var buildWatcherStream: FSEventStreamRef?

    /// Dylib paths to monitor (debug and release).
    private static var watchedDylibPaths: [String] = []

    /// Last known modification times keyed by path (dylibs + build.db).
    private static var watchedModTimes: [String: Date] = [:]

    /// Path to `.build/build.db` — always updated on every build, even
    /// incremental no-ops where the dylib is not relinked.
    private static var buildDbPath: String = ""

    /// Whether an external build is currently detected as in-progress.
    private static var externalBuildInProgress = false

    /// Timer for stale build detection (used by internal async builds).
    private static var buildStaleTimer: DispatchSourceTimer?

    /// Project root directory (set by startBuildWatcher).
    static var baseDir: String = ""

    /// Convenience: path to whichever dylib was most recently written.
    /// Used by the async build process to update the watcher after build.
    static var watchedDylibPath: String = ""

    /// Convenience: last mod time of the most recently written dylib.
    static var lastDylibModTime: Date? {
        get { watchedModTimes.values.max() }
        set {
            if let nv = newValue {
                watchedModTimes[watchedDylibPath] = nv
            }
        }
    }

    /// Start watching `.build/` for swift build activity.
    /// A single FSEventStream covers both debug and release subdirectories.
    /// Called once after module load.  Uses FSEvents — no polling.
    @available(macOS 26.0, *)
    static func startBuildWatcher(baseDir: String) {
        stopBuildWatcher()
        self.baseDir = baseDir

        let buildDir = (baseDir as NSString).appendingPathComponent(".build")
        let debugDylib = (buildDir as NSString).appendingPathComponent("debug/libHyalo.dylib")
        let releaseDylib = (buildDir as NSString).appendingPathComponent("release/libHyalo.dylib")

        watchedDylibPaths = [debugDylib, releaseDylib]
        watchedDylibPath = debugDylib
        buildDbPath = (buildDir as NSString).appendingPathComponent("build.db")

        // Record initial mod times
        for path in watchedDylibPaths + [buildDbPath] {
            if let attrs = try? FileManager.default.attributesOfItem(atPath: path),
               let modTime = attrs[.modificationDate] as? Date {
                watchedModTimes[path] = modTime
            }
        }

        // Ensure .build/ exists so FSEvents has something to watch
        try? FileManager.default.createDirectory(
            atPath: buildDir, withIntermediateDirectories: true)

        var context = FSEventStreamContext()
        let paths = [buildDir as CFString] as CFArray

        guard let stream = FSEventStreamCreate(
            nil,
            { _, _, numEvents, eventPaths, _, _ in
                // eventPaths is a C array of const char* (no UseCFTypes flag)
                let cPaths = eventPaths.assumingMemoryBound(
                    to: UnsafePointer<CChar>.self)
                var paths: [String] = []
                paths.reserveCapacity(numEvents)
                for i in 0..<numEvents {
                    paths.append(String(cString: cPaths[i]))
                }
                HyaloModule.handleBuildDirectoryChange(eventPaths: paths)
            },
            &context,
            paths,
            FSEventStreamEventId(kFSEventStreamEventIdSinceNow),
            1.0,  // 1s latency — quick feedback for build start
            FSEventStreamCreateFlags(kFSEventStreamCreateFlagFileEvents)
        ) else {
            NSLog("[Hyalo:BuildWatcher] Failed to create FSEventStream for %@", buildDir)
            return
        }

        FSEventStreamScheduleWithRunLoop(
            stream, CFRunLoopGetMain(),
            CFRunLoopMode.defaultMode.rawValue)
        FSEventStreamStart(stream)
        buildWatcherStream = stream

        NSLog("[Hyalo:BuildWatcher] Watching %@ for build activity", buildDir)
    }

    /// Handle file system changes in `.build/`.
    ///
    /// Lifecycle detection:
    /// - **Start**: FSEvents activity in build-relevant paths (not index
    ///   store) without `build.db` mod-time change.
    /// - **Completion**: `build.db` mod-time changes (SPM always updates it,
    ///   even for incremental builds that skip relinking).
    /// - **Dylib changed**: additionally check dylib mod-times so the
    ///   "Reload" button is only offered when the binary actually changed.
    /// - **Stalled**: 30s of FSEvents silence → assume failure.
    ///
    /// Paths containing `/index/` or `/IndexStore/` are ignored for build
    /// start detection — sourcekit-lsp writes to these directories when
    /// opening Swift files, which would otherwise cause false positives.
    @available(macOS 26.0, *)
    private static func handleBuildDirectoryChange(eventPaths: [String]) {
        // Check build.db — the reliable completion signal
        var buildDbChanged = false
        if FileManager.default.fileExists(atPath: buildDbPath),
           let attrs = try? FileManager.default.attributesOfItem(atPath: buildDbPath),
           let modTime = attrs[.modificationDate] as? Date {
            let last = watchedModTimes[buildDbPath]
            if last == nil || modTime > last! {
                watchedModTimes[buildDbPath] = modTime
                buildDbChanged = true
            }
        }

        // Check if any dylib was relinked (for the reload button)
        var dylibChanged = false
        for path in watchedDylibPaths {
            guard FileManager.default.fileExists(atPath: path) else { continue }
            guard let attrs = try? FileManager.default.attributesOfItem(atPath: path),
                  let modTime = attrs[.modificationDate] as? Date else { continue }
            let last = watchedModTimes[path]
            if let l = last, modTime <= l { continue }
            watchedModTimes[path] = modTime
            dylibChanged = true
            break
        }

        if buildDbChanged {
            // build.db updated — build finished (success: SPM wrote the db)
            cancelBuildStaleTimer()
            externalBuildInProgress = false
            DispatchQueue.main.async {
                ActivityManager.shared.finishModuleBuild(
                    success: true, dylibChanged: dylibChanged)
                NSLog("[Hyalo:BuildWatcher] Build completed (dylibChanged=%d)",
                      dylibChanged ? 1 : 0)
            }
        }
        // No external build start detection.  sourcekit-lsp and other
        // tools write to .build/ (index store, module cache, etc.) and
        // reliably distinguishing real builds from LSP activity is not
        // feasible with path filtering alone.  Internal builds are
        // tracked explicitly via hyalo-async-build / startModuleBuild().
    }

    private static func cancelBuildStaleTimer() {
        buildStaleTimer?.cancel()
        buildStaleTimer = nil
    }

    /// Stop the build watcher.
    static func stopBuildWatcher() {
        if let stream = buildWatcherStream {
            FSEventStreamStop(stream)
            FSEventStreamInvalidate(stream)
            FSEventStreamRelease(stream)
            buildWatcherStream = nil
        }
        cancelBuildStaleTimer()
        watchedDylibPaths.removeAll()
        watchedModTimes.removeAll()
        externalBuildInProgress = false
    }

    // MARK: - Async Build Process

    /// Currently running build process, if any.
    static var buildProcess: Process?

    /// Run `swift build` asynchronously with full lifecycle tracking.
    /// Activity shows start → real-time log lines → finish with reload button.
    /// Uses Process pipes for stdout/stderr — no polling.
    @available(macOS 26.0, *)
    static func runAsyncBuild(baseDir: String, config: String) {
        // Cancel any in-flight build
        if let existing = buildProcess, existing.isRunning {
            existing.terminate()
        }

        let mgr = ActivityManager.shared
        let activityID = ActivityManager.moduleBuildID

        // Push start activity
        DispatchQueue.main.async {
            mgr.clearLog(id: activityID)
            mgr.startModuleBuild()
        }

        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/swift")
        process.arguments = ["build", "-c", config]
        process.currentDirectoryURL = URL(fileURLWithPath: baseDir)

        // Merge stdout and stderr into a single pipe for log streaming
        let pipe = Pipe()
        process.standardOutput = pipe
        process.standardError = pipe

        // Stream log lines via readabilityHandler — fires on data arrival.
        // No polling — the run loop dispatches when the pipe has data.
        let handle = pipe.fileHandleForReading
        var lineBuffer = Data()
        handle.readabilityHandler = { fh in
            let data = fh.availableData
            guard !data.isEmpty else { return }
            lineBuffer.append(data)

            // Split on newlines and push complete lines
            while let range = lineBuffer.range(of: Data([0x0A])) {
                let lineData = lineBuffer.subdata(in: lineBuffer.startIndex..<range.lowerBound)
                lineBuffer.removeSubrange(lineBuffer.startIndex...range.lowerBound)
                if let line = String(data: lineData, encoding: .utf8), !line.isEmpty {
                    DispatchQueue.main.async {
                        mgr.appendLog(id: activityID, line: line)
                        // Update the activity message with the last meaningful line
                        let trimmed = line.trimmingCharacters(in: .whitespaces)
                        if trimmed.hasPrefix("[") || trimmed.hasPrefix("Build") || trimmed.hasPrefix("Compiling") ||
                           trimmed.hasPrefix("Linking") || trimmed.hasPrefix("error:") || trimmed.hasPrefix("warning:") {
                            mgr.updateModuleBuild(message: trimmed)
                        }
                    }
                }
            }
        }

        // Completion handler — fires when process exits.
        process.terminationHandler = { proc in
            // Flush remaining buffer
            handle.readabilityHandler = nil
            if !lineBuffer.isEmpty, let line = String(data: lineBuffer, encoding: .utf8) {
                DispatchQueue.main.async {
                    mgr.appendLog(id: activityID, line: line)
                }
            }

            let success = proc.terminationStatus == 0
            DispatchQueue.main.async {
                mgr.finishModuleBuild(success: success, dylibChanged: success)
                // Update the FSEvents watcher's known mod times so it doesn't
                // re-trigger for this build's artifacts.
                if success {
                    let debugDylib = (HyaloModule.baseDir as NSString).appendingPathComponent(".build/debug/libHyalo.dylib")
                    let releaseDylib = (HyaloModule.baseDir as NSString).appendingPathComponent(".build/release/libHyalo.dylib")
                    let dylibPath = config == "release" ? releaseDylib : debugDylib
                    if let attrs = try? FileManager.default.attributesOfItem(atPath: dylibPath),
                       let modTime = attrs[.modificationDate] as? Date {
                        HyaloModule.watchedModTimes[dylibPath] = modTime
                    }
                }
                // Update build.db mod time
                if let attrs = try? FileManager.default.attributesOfItem(atPath: HyaloModule.buildDbPath),
                   let modTime = attrs[.modificationDate] as? Date {
                    HyaloModule.watchedModTimes[HyaloModule.buildDbPath] = modTime
                }
                HyaloModule.externalBuildInProgress = false
                NSLog("[Hyalo:Build] swift build %@ (exit %d)", success ? "succeeded" : "failed", proc.terminationStatus)
            }

            HyaloModule.buildProcess = nil
        }

        do {
            try process.run()
            buildProcess = process
            NSLog("[Hyalo:Build] Started swift build -c %@ in %@", config, baseDir)
        } catch {
            DispatchQueue.main.async {
                mgr.appendLog(id: activityID, line: "Failed to start: \(error.localizedDescription)")
                mgr.finishModuleBuild(success: false)
            }
            NSLog("[Hyalo:Build] Failed to start: %@", error.localizedDescription)
        }
    }

    // MARK: - Multi-Frame Helpers

    /// Decorate a window with the Hyalo IDE shell.
    /// - Parameters:
    ///   - window: The NSWindow to decorate
    ///   - frameId: The Emacs frame window-id (desc_ctr), used as dictionary key
    /// Must be called on the main thread.
    @available(macOS 26.0, *)
    @MainActor
    static func decorateWindow(_ window: NSWindow, frameId: Int) {
        guard controllers[frameId] == nil else {
            NSLog("[Hyalo:Nav] decorateWindow: frame %d already decorated", frameId)
            return
        }
        guard let contentView = window.contentView else { return }
        guard let emacsView = extractEmacsView(from: contentView) else { return }

        // Drain autoreleased objects from the view hierarchy teardown
        // immediately.  removeFromSuperview and clearing the content
        // view controller autorelease internal AppKit objects (layers,
        // layout engines).  If left in the outer pool, Emacs's
        // ns_read_socket_1 pops them later when they may already be
        // invalid → SIGSEGV in AutoreleasePoolPage::releaseUntil.
        autoreleasepool {
            emacsView.removeFromSuperview()

            if window.contentViewController != nil {
                window.contentViewController = nil
            }
        }

        let workspace = HyaloWorkspaceState()
        workspace.navigatorVisible = false
        workspace.inspectorVisible = false

        // Set the project name from the base directory immediately so
        // the toolbar shows the correct title from the first frame.
        // Without this, BranchPickerView falls back to "Emacs" and the
        // toolbar pills resize when the real name arrives via channels.
        if !HyaloModule.baseDir.isEmpty {
            let baseName = (HyaloModule.baseDir as NSString).lastPathComponent
            workspace.projectName = baseName
            ToolbarManager.shared.viewModel.projectName = baseName
        }

        let controller = HyaloWindowController(
            window: window,
            workspace: workspace,
            emacsView: emacsView
        )

        controllers[frameId] = controller
        windowToFrameId[ObjectIdentifier(window)] = frameId

        // Wire channel callbacks to the new controller
        wireCallbacks(to: controller)

        // Show a standalone loading proxy window at the same frame position.
        // The Emacs window itself stays invisible (visibility . nil) until
        // hyalo-loading-done is called.  The proxy is created only for the
        // first (primary) frame, and only when a package bootstrap will
        // occur (no .local/elpa).  Normal startups skip the proxy entirely.
        if loadingProxyWindow == nil && needsBootstrap {
            showLoadingProxy(matching: window)
        }

        NSLog("[Hyalo:Nav] decorateWindow: frame %d (window %@) decorated (%d total)",
              frameId, window, controllers.count)

        // Diagnostic: verify EmacsView is properly in the window hierarchy
        // after HyaloWindowController.setup() replaced the contentView.
        if let ev = controller.emacsView {
            NSLog("[Hyalo:Nav] decorateWindow: emacsView.window=%@, superview=%@, frame=%@",
                  String(describing: ev.window),
                  String(describing: ev.superview),
                  NSStringFromRect(ev.frame))
        }
    }

    /// Create and show a vibrancy proxy window at the same screen position as
    /// `emacsWindow`.  The proxy hosts LoadingView and stays visible until
    /// `hyalo-loading-done` closes it and reveals the Emacs window.
    @available(macOS 26.0, *)
    @MainActor
    private static func showLoadingProxy(matching emacsWindow: NSWindow) {
        // Wrap in autoreleasepool: creating an NSWindow and calling
        // makeKeyAndOrderFront autoreleases window transition animation
        // objects.  Drain them immediately so they do not linger in
        // Emacs's outer autorelease pool.
        autoreleasepool {
            let proxy = NSWindow(
                contentRect: emacsWindow.frame,
                styleMask: [.titled, .fullSizeContentView],
                backing: .buffered,
                defer: false
            )
            proxy.titleVisibility = .hidden
            proxy.titlebarAppearsTransparent = true
            proxy.isOpaque = false
            proxy.backgroundColor = .clear
            proxy.isMovableByWindowBackground = true
            proxy.level = emacsWindow.level

            // Vibrancy background matching the default workspace material
            let vibrancy = NSVisualEffectView(frame: proxy.contentView?.bounds ?? .zero)
            vibrancy.autoresizingMask = [.width, .height]
            vibrancy.material = .hudWindow
            vibrancy.blendingMode = .behindWindow
            vibrancy.state = .active
            proxy.contentView?.addSubview(vibrancy)

            // SwiftUI LoadingView hosted on top
            let hostView = NSHostingView(
                rootView: LoadingView(state: loadingState)
            )
            hostView.frame = vibrancy.bounds
            hostView.autoresizingMask = [.width, .height]
            vibrancy.addSubview(hostView)

            proxy.makeKeyAndOrderFront(nil)
            loadingProxyWindow = proxy
        }
    }

    /// Remove Hyalo decoration from a frame.
    /// - Parameter frameId: The Emacs frame window-id (desc_ctr)
    /// Must be called on the main thread.
    @available(macOS 26.0, *)
    static func undecorateWindow(_ frameId: Int) {
        guard let controller = controllers.removeValue(forKey: frameId) else {
            NSLog("[Hyalo:Nav] undecorateWindow: frame %d not found", frameId)
            return
        }
        // Clean up reverse map
        if let window = controller.window {
            windowToFrameId.removeValue(forKey: ObjectIdentifier(window))
        }
        NSLog("[Hyalo:Nav] undecorateWindow: frame %d removed (%d remaining)", frameId, controllers.count)
    }
}

// MARK: - Module Factory

func createModule() -> Module {
    HyaloModule()
}

// MARK: - NSColor Hex Extension

extension NSColor {
    convenience init?(hexString: String) {
        var hex = hexString.trimmingCharacters(in: .whitespacesAndNewlines)
        if hex.hasPrefix("#") { hex.removeFirst() }
        guard hex.count == 6 else { return nil }
        var rgb: UInt64 = 0
        guard Scanner(string: hex).scanHexInt64(&rgb) else { return nil }
        self.init(
            red: CGFloat((rgb >> 16) & 0xFF) / 255.0,
            green: CGFloat((rgb >> 8) & 0xFF) / 255.0,
            blue: CGFloat(rgb & 0xFF) / 255.0,
            alpha: 1.0
        )
    }
}
