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
    /// The original Emacs NSView, retained for focus restoration.
    private(set) var emacsView: NSView?
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

        // Call setup() synchronously.  decorateWindow is already called from
        // the Emacs main thread (via MainActor.assumeIsolated in the defun),
        // so setup can safely manipulate the window hierarchy here.
        // Async dispatch was removed because it caused setup to fire during a
        // subsequent sit-for, interleaving with Emacs drawing and segfaulting.
        setup()
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    // MARK: - Setup

    private func setup() {
        guard let window else { return }
        guard let emacsView else { return }


        // Wrap the entire view hierarchy manipulation in an autoreleasepool.
        // Replacing window.contentView autoreleases the old content view and
        // its internal AppKit objects (layers, constraints, layout engines).
        // Without an explicit drain, these objects linger in the outer pool
        // managed by Emacs's C code (ns_read_socket_1).  When the next
        // (sit-for 0) triggers ns_read_socket_1 and pops its pool, some of
        // those objects have already been invalidated by the hierarchy swap,
        // causing a use-after-free → SIGSEGV in AutoreleasePoolPage::releaseUntil.
        // Draining here ensures all temporaries are released deterministically
        // before control returns to the Emacs event loop.
        autoreleasepool {

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

        // Force SwiftUI to evaluate its body NOW so that makeNSView fires
        // synchronously.  Without this, SwiftUI may defer makeNSView for
        // invisible windows, leaving EmacsView orphaned (no window, no
        // superview).  When make-frame-visible later calls
        // makeKeyAndOrderFront, windowDidBecomeKey fires on an orphaned
        // EmacsView and triggers gui_update_cursor on corrupted glyph
        // matrices → segfault.
        hosting.layoutSubtreeIfNeeded()

        // Restore the window position.  Setting contentView and styleMask
        // can shift the origin (AppKit adjusts for toolbar height changes).
        // Keep the same top-left corner by computing from the saved frame.
        let newOrigin = NSPoint(
            x: savedFrame.origin.x,
            y: savedFrame.origin.y + savedFrame.size.height - window.frame.size.height
        )
        window.setFrameOrigin(newOrigin)

        // The Emacs window is intentionally NOT revealed here.
        // A standalone loading proxy window (created in Module.decorateWindow)
        // is shown instead while init.el runs.  The Emacs window is revealed
        // by hyalo-loading-done once the IDE shell is fully initialized.


        } // autoreleasepool — drain all temporaries from view hierarchy swap

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
    /// Manages focus: terminal gets focus when shown, Emacs when hidden.
    func selectUtilityAreaTab(_ index: Int) {
        let tabs = UtilityAreaTab.allCases
        guard index >= 1, index <= tabs.count else { return }
        let tab = tabs[index - 1]

        let willHide = workspace.utilityAreaVisible && utilityAreaViewModel.selectedTab == tab

        withAnimation(Self.panelAnimation) {
            if willHide {
                workspace.utilityAreaVisible = false
            } else {
                utilityAreaViewModel.selectedTab = tab
                workspace.utilityAreaVisible = true
            }
        }

        if willHide {
            focusEmacs()
        } else if tab == .terminal {
            // Defer until the view is laid out
            DispatchQueue.main.async { [weak self] in self?.focusTerminal() }
        }
    }

    /// Show utility area and select tab by 1-based index. Never hides.
    func showUtilityAreaTab(_ index: Int) {
        let tabs = UtilityAreaTab.allCases
        guard index >= 1, index <= tabs.count else { return }
        let tab = tabs[index - 1]

        withAnimation(Self.panelAnimation) {
            utilityAreaViewModel.selectedTab = tab
            workspace.utilityAreaVisible = true
        }

        if tab == .terminal {
            DispatchQueue.main.async { [weak self] in self?.focusTerminal() }
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

    // MARK: - Focus Management

    /// Focus the utility area terminal view.
    func focusTerminal() {
        guard let tv = utilityAreaViewModel.terminalHolder.container?.terminalView else { return }
        window?.makeFirstResponder(tv)
    }

    /// Focus the Emacs main view (restore keyboard input to Emacs).
    func focusEmacs() {
        guard let ev = emacsView else { return }
        window?.makeFirstResponder(ev)
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


