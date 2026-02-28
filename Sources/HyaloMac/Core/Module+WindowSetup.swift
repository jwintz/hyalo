// Module+WindowSetup.swift - IDE window setup, loading proxy, keycast
// Target: macOS 26 Tahoe

import AppKit
import HyaloShared
import EmacsSwiftModule
import SwiftUI

extension HyaloModule {

    // MARK: - Core & Window Setup Bindings

    func setupCoreBindings(_ env: EmacsSwiftModule.Environment) throws {

        try env.defun("hyalo-version") { [version] in version }

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
    }

    func setupLoadingBindings(_ env: EmacsSwiftModule.Environment) throws {

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
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.isLoading = false
                    }
                    if let proxy = HyaloModule.loadingProxyWindow {
                        proxy.orderOut(nil)
                        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) {
                            HyaloModule.loadingProxyWindow = nil
                        }
                    }
                }
                return true
            }
            return false
        }
    }

    func setupKeycastBindings(_ env: EmacsSwiftModule.Environment) throws {

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
    }

    func setupWindowBindings(_ env: EmacsSwiftModule.Environment) throws {

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
                        return
                    }
                    guard let window = findUndecoratedEmacsWindow() else {
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
    }

    // MARK: - Multi-Frame Helpers

    /// Decorate a window with the Hyalo IDE shell.
    @available(macOS 26.0, *)
    @MainActor
    static func decorateWindow(_ window: NSWindow, frameId: Int) {
        guard controllers[frameId] == nil else {
            return
        }
        guard let contentView = window.contentView else { return }
        guard let emacsView = extractEmacsView(from: contentView) else { return }

        autoreleasepool {
            emacsView.removeFromSuperview()

            if window.contentViewController != nil {
                window.contentViewController = nil
            }
        }

        let workspace = HyaloWorkspaceState()
        workspace.navigatorVisible = false
        workspace.inspectorVisible = false

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

        wireCallbacks(to: controller)

        if loadingProxyWindow == nil && needsBootstrap {
            showLoadingProxy(matching: window)
        }

        if let _ = controller.emacsView {
        }
    }

    /// Create and show a vibrancy proxy window at the same screen position.
    @available(macOS 26.0, *)
    @MainActor
    private static func showLoadingProxy(matching emacsWindow: NSWindow) {
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

            let vibrancy = NSVisualEffectView(frame: proxy.contentView?.bounds ?? .zero)
            vibrancy.autoresizingMask = [.width, .height]
            vibrancy.material = .hudWindow
            vibrancy.blendingMode = .behindWindow
            vibrancy.state = .active
            proxy.contentView?.addSubview(vibrancy)

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
    @available(macOS 26.0, *)
    static func undecorateWindow(_ frameId: Int) {
        guard let controller = controllers.removeValue(forKey: frameId) else {
            return
        }
        if let window = controller.window {
            windowToFrameId.removeValue(forKey: ObjectIdentifier(window))
        }
    }
}
