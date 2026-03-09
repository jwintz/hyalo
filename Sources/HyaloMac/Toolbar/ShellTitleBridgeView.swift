// ShellTitleBridgeView.swift - Keeps KelyphosShellState title/subtitle in sync
// with ToolbarViewModel so that KelyphosShellView's .navigationTitle /
// .navigationSubtitle always reflect the current project and branch.

import SwiftUI
import KelyphosKit
import HyaloShared

/// Zero-size view that propagates ToolbarViewModel changes into KelyphosShellState
/// title and subtitle using SwiftUI observation — no polling.
@available(macOS 26.0, *)
struct ShellTitleBridgeView: View {
    var toolbarVM: ToolbarViewModel
    var shellState: KelyphosShellState

    var body: some View {
        Color.clear
            .frame(width: 0, height: 0)
            .onChange(of: toolbarVM.currentBranch, initial: true) { _, branch in
                shellState.subtitle = branch
            }
            .onChange(of: toolbarVM.projectName, initial: true) { _, name in
                shellState.title = name.isEmpty ? "Emacs" : name
            }
    }
}
