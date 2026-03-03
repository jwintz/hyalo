// MainContentView.swift - Editor area: tab bar + Emacs view + status bar
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Utility area is managed by KelyphosContentArea (in KelyphosShellView).
// Background vibrancy is managed by KelyphosShellView.

import AppKit
import SwiftUI
import HyaloShared

@available(macOS 26.0, *)
struct MainContentView: View {
    let emacsView: NSView
    let terminalPalette: TerminalPalette

    var editorTabViewModel: EditorTabViewModel?

    @State private var statusBarViewModel = StatusBarManager.shared.viewModel
    @State private var localTabViewModel = EditorTabViewModel()

    init(
        emacsView: NSView,
        terminalPalette: TerminalPalette,
        editorTabViewModel: EditorTabViewModel? = nil
    ) {
        self.emacsView = emacsView
        self.terminalPalette = terminalPalette
        self.editorTabViewModel = editorTabViewModel
    }

    private var effectiveTabViewModel: EditorTabViewModel {
        editorTabViewModel ?? localTabViewModel
    }

    var body: some View {
        VStack(spacing: 0) {
            EditorTabBarView(viewModel: effectiveTabViewModel)
            EmacsNSViewRepresentable(emacsView: emacsView)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            StatusBarView(viewModel: statusBarViewModel)
        }
    }
}
