// HyaloWindowController.swift - Window controller hosting NavigationSplitView
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Hosts HyaloNavigationLayout (SwiftUI NavigationSplitView) as the window's
// content view. Toolbar items are managed by SwiftUI .toolbar {} on the
// NavigationSplitView. The HyaloToolbar is installed as a stub (no delegate)
// for Emacs C code compatibility — the NSToolbar extension in HyaloToolbar.swift
// ensures any toolbar SwiftUI creates also responds to Emacs selectors.

import AppKit
import SwiftUI

@available(macOS 26.0, *)
final class HyaloWindowController: NSWindowController {

    // MARK: - Properties

    private(set) var workspace: HyaloWorkspaceState
    private var emacsView: NSView?
    private var observers: [NSKeyValueObservation] = []
    private var titleObservation: NSKeyValueObservation?
    let editorTabViewModel = EditorTabViewModel()
    let utilityAreaViewModel = UtilityAreaViewModel()

    /// True once `setup()` has installed the SwiftUI hosting view.
    private(set) var isSetUp = false

    // MARK: - Initialization

    init(window: NSWindow?, workspace: HyaloWorkspaceState, emacsView: NSView? = nil) {
        self.workspace = workspace
        self.emacsView = emacsView
        super.init(window: window)

        if let win = window, win.delegate == nil {
            win.delegate = self
        }

        DispatchQueue.main.async { [weak self] in
            self?.setup()
        }
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    // MARK: - Setup

    private func setup() {
        guard let window else { return }
        guard let emacsView else { return }

        NSLog("[Hyalo:Nav] setup: window=%@ emacsView=%@", window, emacsView)

        // Save the window's position before reconfiguring.
        // Emacs placed the window at a specific origin; we must preserve
        // it to prevent a visible position jump when the chrome is installed.
        let savedFrame = window.frame

        // Window configuration
        window.styleMask.insert(.fullSizeContentView)
        window.titleVisibility = .hidden
        window.titlebarAppearsTransparent = true
        window.titlebarSeparatorStyle = .automatic
        window.title = ""
        window.representedURL = nil

        // Remove Emacs's toolbar if present
        if window.toolbar != nil {
            window.toolbar = nil
        }

        // Window configuration - use .unified toolbar style
        window.toolbarStyle = .unified

        // Create SwiftUI layout and host it.
        // NavigationSplitView's .toolbar {} manages all toolbar items.
        // The NSToolbar extension stubs (in HyaloToolbar.swift) ensure
        // any toolbar SwiftUI creates responds to Emacs C selectors.
        let layout = HyaloNavigationLayout(
            workspace: workspace,
            emacsView: emacsView,
            editorTabViewModel: editorTabViewModel,
            utilityAreaViewModel: utilityAreaViewModel
        )
        // Inject view models via environment (AUDIT.md #4)
        .environment(\.projectNavigatorViewModel, NavigatorManager.shared.projectNavigatorViewModel)
        .environment(\.inspectorViewModel, InspectorManager.shared.viewModel)
        .environment(\.colorTheme, workspace.colorTheme)
        let hosting = NSHostingView(rootView: layout)
        hosting.frame = window.contentView?.bounds ?? .zero
        hosting.autoresizingMask = [NSView.AutoresizingMask.width, NSView.AutoresizingMask.height]

        window.contentView = hosting
        isSetUp = true

        // Restore the window position.  Setting contentView and styleMask
        // can shift the origin (AppKit adjusts for toolbar height changes).
        // Keep the same top-left corner by computing from the saved frame.
        let newOrigin = NSPoint(
            x: savedFrame.origin.x,
            y: savedFrame.origin.y + savedFrame.size.height - window.frame.size.height
        )
        window.setFrameOrigin(newOrigin)

        // Reveal the window now that the chrome is installed.
        // Always call makeKeyAndOrderFront + activate to ensure the window
        // appears even when Emacs's (make-frame-visible) hasn't run yet.
        window.makeKeyAndOrderFront(nil)
        NSApp.activate()

        NSLog("[Hyalo:Nav] hosting view installed: frame=%@", NSStringFromRect(window.frame))

        // Observe window title to prevent Emacs from displaying geometry
        titleObservation = window.observe(\.title, options: [.new]) { window, _ in
            if window.title != "" {
                window.title = ""
            }
        }
    }

    // MARK: - Public API

    private static let panelAnimation: Animation = .easeInOut(duration: 0.15)

    func toggleNavigator() {
        withAnimation(Self.panelAnimation) {
            workspace.navigatorVisible.toggle()
        }
    }

    func toggleInspector() {
        withAnimation(Self.panelAnimation) {
            workspace.inspectorVisible.toggle()
        }
    }

    func setNavigatorVisible(_ visible: Bool) {
        withAnimation(Self.panelAnimation) {
            workspace.navigatorVisible = visible
        }
    }

    func setInspectorVisible(_ visible: Bool) {
        withAnimation(Self.panelAnimation) {
            workspace.inspectorVisible = visible
        }
    }

    var isNavigatorVisible: Bool {
        workspace.navigatorVisible
    }

    var isInspectorVisible: Bool {
        workspace.inspectorVisible
    }

    // MARK: - Tab Selection (Xcode-like toggle behavior)

    /// Select navigator tab by 1-based index. If already on that tab and visible, toggle off.
    func selectNavigatorTab(_ index: Int) {
        let vm = NavigatorManager.shared.viewModel
        let tabs = vm.tabItems
        guard index >= 1, index <= tabs.count else { return }
        let tab = tabs[index - 1]

        withAnimation(Self.panelAnimation) {
            if workspace.navigatorVisible && vm.selectedTab == tab {
                workspace.navigatorVisible = false
            } else {
                vm.selectedTab = tab
                workspace.navigatorVisible = true
            }
        }
    }

    /// Select inspector tab by 1-based index. If already on that tab and visible, toggle off.
    func selectInspectorTab(_ index: Int) {
        let vm = InspectorManager.shared.viewModel
        let tabs = vm.tabItems
        guard index >= 1, index <= tabs.count else { return }
        let tab = tabs[index - 1]

        withAnimation(Self.panelAnimation) {
            if workspace.inspectorVisible && vm.selectedTab == tab {
                workspace.inspectorVisible = false
            } else {
                vm.selectedTab = tab
                workspace.inspectorVisible = true
            }
        }
    }

    /// Select utility area tab by 1-based index. If already on that tab and visible, toggle off.
    func selectUtilityAreaTab(_ index: Int) {
        let tabs = UtilityAreaTab.allCases
        guard index >= 1, index <= tabs.count else { return }
        let tab = tabs[index - 1]

        withAnimation(Self.panelAnimation) {
            if workspace.utilityAreaVisible && utilityAreaViewModel.selectedTab == tab {
                workspace.utilityAreaVisible = false
            } else {
                utilityAreaViewModel.selectedTab = tab
                workspace.utilityAreaVisible = true
            }
        }
    }

    /// Get current navigator tab index (1-based), or 0 if none.
    var navigatorTabIndex: Int {
        let vm = NavigatorManager.shared.viewModel
        guard let tab = vm.selectedTab,
              let index = vm.tabItems.firstIndex(of: tab) else { return 0 }
        return index + 1
    }

    /// Get current inspector tab index (1-based), or 0 if none.
    var inspectorTabIndex: Int {
        let vm = InspectorManager.shared.viewModel
        guard let tab = vm.selectedTab,
              let index = vm.tabItems.firstIndex(of: tab) else { return 0 }
        return index + 1
    }

    func applyAppearance(_ appearance: NSAppearance?) {
        window?.appearance = appearance
    }

    func updateLayerBackgrounds() {
        // No-op: workspace state changes trigger SwiftUI re-render automatically
    }

}

// MARK: - NSWindowDelegate

@available(macOS 26.0, *)
extension HyaloWindowController: NSWindowDelegate {
    func windowWillClose(_ notification: Foundation.Notification) {
        titleObservation?.invalidate()
        titleObservation = nil
        observers.forEach { $0.invalidate() }
        observers.removeAll()
    }
}


