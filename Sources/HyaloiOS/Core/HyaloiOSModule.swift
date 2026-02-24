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
    @Published var emacsView: UIView?
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

    #if DEBUG
    /// Load mock data for simulator visual testing.
    /// Called when no real Emacs is running.
    public func loadMockData() {
        // Populate NavigatorManager with sample buffers
        let mockBuffers: [BufferInfo] = [
            BufferInfo(
                id: "scratch",
                name: "*scratch*",
                path: nil,
                modified: false,
                icon: "doc.text"
            ),
            BufferInfo(
                id: "main.swift",
                name: "main.swift",
                path: "/Users/jwintz/Syntropment/hyalo-unified/Sources/Hyalo/main.swift",
                modified: true,
                icon: "swift"
            ),
            BufferInfo(
                id: "README.md",
                name: "README.md",
                path: "/Users/jwintz/Syntropment/hyalo-unified/README.md",
                modified: false,
                icon: "doc.richtext"
            ),
            BufferInfo(
                id: "HyaloRootView.swift",
                name: "HyaloRootView.swift",
                path: "/Users/jwintz/Syntropment/hyalo-unified/Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift",
                modified: false,
                icon: "swift"
            )
        ]
        NavigatorManager.shared.updateBufferList(mockBuffers)
        NavigatorManager.shared.setActiveBuffer("main.swift")

        // Populate EditorTabViewModel with sample tabs
        let mockTabs: [EditorTab] = [
            EditorTab(
                id: "main.swift",
                name: "main.swift",
                icon: "swift",
                isModified: true,
                isTemporary: false,
                filePath: "/Users/jwintz/Syntropment/hyalo-unified/Sources/Hyalo/main.swift"
            ),
            EditorTab(
                id: "README.md",
                name: "README.md",
                icon: "doc.richtext",
                isModified: false,
                isTemporary: false,
                filePath: "/Users/jwintz/Syntropment/hyalo-unified/README.md"
            ),
            EditorTab(
                id: "HyaloRootView.swift",
                name: "HyaloRootView.swift",
                icon: "swift",
                isModified: false,
                isTemporary: false,
                filePath: "/Users/jwintz/Syntropment/hyalo-unified/Sources/HyaloiOS/Window/HyaloiOSNavigationLayout.swift"
            )
        ]
        editorTabViewModel.updateTabs(mockTabs)
        editorTabViewModel.onTabSelected("main.swift")

        // Populate StatusBarViewModel with sample values
        let statusBarViewModel = StatusBarManager.shared.viewModel
        statusBarViewModel.line = 42
        statusBarViewModel.column = 13
        statusBarViewModel.mode = "Swift"
        statusBarViewModel.encoding = "UTF-8"
        statusBarViewModel.lineEnding = "LF"
        statusBarViewModel.indentStyle = "Spaces"
        statusBarViewModel.indentWidth = 4
        statusBarViewModel.fileType = "Swift"
        statusBarViewModel.fileSize = "12.3 KB"
    }
    #endif
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
