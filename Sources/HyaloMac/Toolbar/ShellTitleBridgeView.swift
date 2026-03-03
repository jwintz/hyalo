// ShellTitleBridgeView.swift - Toolbar title views for HyaloWindowController
//
// ShellTitleView: renders project name + branch as a two-line toolbar title,
//   bypassing window.titleVisibility = .hidden which suppresses navigationTitle.
// ShellTitleBridgeView: zero-size observer that keeps shellState.title/subtitle
//   in sync with ToolbarViewModel, driving ShellTitleView reactively.

import SwiftUI
import KelyphosKit
import HyaloShared

// MARK: - Toolbar Title View

/// Two-line title rendered as a leading toolbar item.
/// Mirrors the standard macOS unified-toolbar title+subtitle appearance.
@available(macOS 26.0, *)
struct ShellTitleView: View {
    var shellState: KelyphosShellState

    var body: some View {
        VStack(alignment: .leading, spacing: 1) {
            Text(shellState.title.isEmpty ? "Emacs" : shellState.title)
                .font(.headline)
                .lineLimit(1)
            if !shellState.subtitle.isEmpty {
                Text(shellState.subtitle)
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
            }
        }
        .frame(minWidth: 60, alignment: .leading)
    }
}

// MARK: - Title Bridge

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
