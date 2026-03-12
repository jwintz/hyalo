// HyaloWindowController.swift - Window controller hosting KelyphosShellView
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI
import HyaloShared
import KelyphosKit

@available(macOS 26.0, *)
final class HyaloWindowController: NSWindowController {

    // MARK: - Properties

    private(set) var workspace: HyaloWorkspaceState
    private(set) var shellState: KelyphosShellState
    /// The original Emacs NSView, retained for focus restoration.
    private(set) var emacsView: NSView?
    private var observers: [NSKeyValueObservation] = []
    private var titleObservation: NSKeyValueObservation?
    private var isRestoringTitle = false
    let editorTabViewModel = EditorTabViewModel()
    let utilityAreaViewModel = UtilityAreaViewModel()

    /// The terminal palette for this frame.
    private let terminalPalette = TerminalPalette.shared

    /// True once `setup()` has installed the SwiftUI hosting view.
    private(set) var isSetUp = false

    // MARK: - Initialization

    init(window: NSWindow?, workspace: HyaloWorkspaceState, shellState: KelyphosShellState, emacsView: NSView? = nil) {
        self.workspace = workspace
        self.shellState = shellState
        self.emacsView = emacsView
        super.init(window: window)

        if let win = window, win.delegate == nil {
            win.delegate = self
        }

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

        // Wire appearance callbacks now that shellState exists.
        shellState.colorTheme = workspace.colorTheme

        // Wrap hierarchy changes in autoreleasepool to prevent use-after-free
        // when Emacs's C event loop pops its pool after this returns.
        autoreleasepool {

        let savedFrame = window.frame

        window.styleMask.insert(.fullSizeContentView)
        window.titleVisibility = .visible
        window.titlebarAppearsTransparent = true
        window.titlebarSeparatorStyle = .automatic
        window.title = ""
        window.representedURL = nil

        if window.toolbar != nil { window.toolbar = nil }
        window.toolbarStyle = .unified

        let toolbarVM = ToolbarManager.shared.viewModel

        // Seed shell title; subtitle is kept in sync by ShellTitleBridgeView inside the content.
        shellState.title = workspace.projectName.isEmpty ? "Emacs" : workspace.projectName

        let shell = KelyphosShellView(
            state: shellState,
            configuration: KelyphosShellConfiguration(
                navigatorTabs: NavigatorTab.allCases.map { $0 },
                inspectorTabs: InspectorTab.allCases.map { $0 },
                utilityTabs: UtilityAreaTab.allCases.map { $0 },
                scrollable: false,
                principalToolbar: { [workspace] in
                    AnyView(EnvironmentPillView(workspace: workspace))
                },
                trailingToolbarItems: [
                    {
                        AnyView(KeycastView(viewModel: toolbarVM))
                    },
                    {
                        AnyView(PackageManagerView(viewModel: toolbarVM))
                    },
                ],
                detail: { [emacsView, editorTabViewModel, terminalPalette, shellState] in
                    AnyView(MainContentView(
                        emacsView: emacsView,
                        terminalPalette: terminalPalette,
                        editorTabViewModel: editorTabViewModel
                    )
                    .background(ShellTitleBridgeView(toolbarVM: toolbarVM, shellState: shellState))
                    )
                }
            )
        )
        .environment(workspace)
        .environment(\.projectNavigatorViewModel, NavigatorManager.shared.projectNavigatorViewModel)
        .environment(\.inspectorViewModel, InspectorManager.shared.viewModel)
        .environment(\.inspectorManager, InspectorManager.shared)
        .environment(\.navigatorManager, NavigatorManager.shared)
        .environment(\.searchViewModel, NavigatorManager.shared.searchViewModel)
        .environment(\.bufferListViewModel, NavigatorManager.shared.bufferListViewModel)
        .environment(\.utilityAreaViewModel, utilityAreaViewModel)
        .environment(\.colorTheme, workspace.colorTheme)
        .environment(\.terminalContent, AnyView(UtilityAreaTerminalView(
            holder: utilityAreaViewModel.terminalHolder,
            palette: terminalPalette
        )))

        let hosting = NSHostingView(rootView: shell)
        hosting.frame = window.contentView?.bounds ?? .zero
        hosting.autoresizingMask = [.width, .height]

        window.contentView = hosting
        isSetUp = true

        hosting.layoutSubtreeIfNeeded()

        let newOrigin = NSPoint(
            x: savedFrame.origin.x,
            y: savedFrame.origin.y + savedFrame.size.height - window.frame.size.height
        )
        window.setFrameOrigin(newOrigin)

        } // autoreleasepool

        // Emacs sets window.title to geometry strings like "emacs — (134 × 61)".
        // Reset any such overwrite back to shellState.title so .navigationTitle stays correct.
        // Guard: only act when we have a real title; skip while shellState.title is still empty.
        // Async: setting win.title inside its own KVO observation causes re-entrancy on macOS.
        titleObservation = window.observe(\.title, options: [.new]) { [weak self] win, change in
            MainActor.assumeIsolated {
                guard let self, !self.isRestoringTitle else { return }
                guard let newTitle = change.newValue else { return }
                let desired = self.shellState.title
                guard !desired.isEmpty, newTitle != desired else { return }
                self.isRestoringTitle = true
                win.title = desired
                self.isRestoringTitle = false
            }
        }
    }

    // MARK: - Public API

    private static let panelAnimation: Animation = .easeInOut(duration: 0.15)

    func toggleNavigator() {
        withAnimation(Self.panelAnimation) { shellState.navigatorVisible.toggle() }
    }

    func toggleInspector() {
        withAnimation(Self.panelAnimation) { shellState.inspectorVisible.toggle() }
    }

    func setNavigatorVisible(_ visible: Bool) {
        withAnimation(Self.panelAnimation) { shellState.navigatorVisible = visible }
    }

    func setInspectorVisible(_ visible: Bool) {
        withAnimation(Self.panelAnimation) { shellState.inspectorVisible = visible }
    }

    var isNavigatorVisible: Bool { shellState.navigatorVisible }
    var isInspectorVisible: Bool { shellState.inspectorVisible }

    // MARK: - Tab Selection (Xcode-like toggle behavior)

    func selectNavigatorTab(_ index: Int) {
        let tabs = NavigatorTab.allCases
        guard index >= 1, index <= tabs.count else { return }
        withAnimation(Self.panelAnimation) {
            if shellState.navigatorVisible && shellState.selectedNavigatorIndex == index - 1 {
                shellState.navigatorVisible = false
            } else {
                shellState.selectedNavigatorIndex = index - 1
                shellState.navigatorVisible = true
            }
        }
    }

    func selectInspectorTab(_ index: Int) {
        let tabs = InspectorTab.allCases
        guard index >= 1, index <= tabs.count else { return }
        withAnimation(Self.panelAnimation) {
            if shellState.inspectorVisible && shellState.selectedInspectorIndex == index - 1 {
                shellState.inspectorVisible = false
            } else {
                shellState.selectedInspectorIndex = index - 1
                shellState.inspectorVisible = true
            }
        }
    }

    func selectUtilityAreaTab(_ index: Int) {
        let tabs = UtilityAreaTab.allCases
        guard index >= 1, index <= tabs.count else { return }
        let tab = tabs[index - 1]
        let willHide = shellState.utilityAreaVisible && shellState.selectedUtilityIndex == index - 1
        withAnimation(Self.panelAnimation) {
            if willHide {
                shellState.utilityAreaVisible = false
            } else {
                shellState.selectedUtilityIndex = index - 1
                shellState.utilityAreaVisible = true
            }
        }
        if willHide {
            focusEmacs()
        } else if tab == .terminal {
            DispatchQueue.main.async { [weak self] in self?.focusTerminal() }
        }
    }

    func showUtilityAreaTab(_ index: Int) {
        let tabs = UtilityAreaTab.allCases
        guard index >= 1, index <= tabs.count else { return }
        let tab = tabs[index - 1]
        withAnimation(Self.panelAnimation) {
            shellState.selectedUtilityIndex = index - 1
            shellState.utilityAreaVisible = true
        }
        if tab == .terminal {
            DispatchQueue.main.async { [weak self] in self?.focusTerminal() }
        }
    }

    var navigatorTabIndex: Int { (shellState.selectedNavigatorIndex ?? -1) + 1 }
    var inspectorTabIndex: Int { (shellState.selectedInspectorIndex ?? -1) + 1 }

    // MARK: - Focus Management

    func focusTerminal() {
        guard let tv = utilityAreaViewModel.terminalHolder.container?.terminalView else { return }
        window?.makeFirstResponder(tv)
    }

    func focusEmacs() {
        guard let ev = emacsView else { return }
        window?.makeFirstResponder(ev)
    }

    func applyAppearance(_ appearance: NSAppearance?) {
        window?.appearance = appearance
    }

    func updateLayerBackgrounds() {}
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
