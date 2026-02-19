// MainContentView.swift - Main content area with tab bar and Emacs editor
// Target: macOS 26 Tahoe with Liquid Glass design
//
// The vibrancy background stack lives at the NavigationSplitView level
// (HyaloNavigationLayout), spanning all panels.
// This view applies a theme tint overlay.
// Emacs renders with alpha-background 0.0 so the blur shows through.
// The utility area and status bar have a slightly denser tint.
//
// REFACTORED (AUDIT.md #8): Semantic opacity constants instead of magic numbers

import AppKit
import SwiftUI

// MARK: - Semantic Opacity Constants (AUDIT.md #8)

enum HyaloOpacity {
    /// Maximum opacity value
    static let maxOpacity: CGFloat = 1.0

    /// Utility area opacity boost over base
    static let utilityAreaBoost: CGFloat = 0.15

    /// Status bar opacity boost over base
    static let statusBarBoost: CGFloat = 0.10

    /// Combined utility area opacity (capped at max)
    static func utilityAreaOpacity(base: CGFloat) -> CGFloat {
        min(maxOpacity, base + utilityAreaBoost)
    }

    /// Combined status bar opacity (capped at max)
    static func statusBarOpacity(base: CGFloat) -> CGFloat {
        min(maxOpacity, base + statusBarBoost)
    }
}

@available(macOS 26.0, *)
struct MainContentView: View {
    @Bindable var workspace: HyaloWorkspaceState
    let emacsView: NSView

    var editorTabViewModel: EditorTabViewModel?
    var utilityAreaViewModel: UtilityAreaViewModel?

    @State private var statusBarViewModel = StatusBarManager.shared.viewModel
    @State private var localTabViewModel = EditorTabViewModel()
    @State private var localUtilityViewModel = UtilityAreaViewModel()

    init(
        workspace: HyaloWorkspaceState,
        emacsView: NSView,
        editorTabViewModel: EditorTabViewModel? = nil,
        utilityAreaViewModel: UtilityAreaViewModel? = nil
    ) {
        self.workspace = workspace
        self.emacsView = emacsView
        self.editorTabViewModel = editorTabViewModel
        self.utilityAreaViewModel = utilityAreaViewModel
    }

    private var effectiveTabViewModel: EditorTabViewModel {
        editorTabViewModel ?? localTabViewModel
    }

    private var effectiveUtilityViewModel: UtilityAreaViewModel {
        utilityAreaViewModel ?? localUtilityViewModel
    }

    var body: some View {
        VStack(spacing: 0) {
            if !effectiveUtilityViewModel.isMaximized {
                // Normal layout: tab bar, editor, optional utility area
                EditorTabBarView(viewModel: effectiveTabViewModel, workspace: workspace)

                editorContent
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }

            if workspace.utilityAreaVisible {
                UtilityAreaView(viewModel: effectiveUtilityViewModel, workspace: workspace)
                    .background {
                        // Utility area: slightly denser tint (AUDIT.md #8)
                        Color(nsColor: workspace.backgroundColor)
                            .opacity(HyaloOpacity.utilityAreaOpacity(base: workspace.backgroundAlpha))
                    }
                    .transition(.move(edge: .bottom))
            }

            StatusBarView(viewModel: statusBarViewModel, workspace: workspace)
                .background {
                    // Status bar: slightly denser tint (AUDIT.md #8)
                    Color(nsColor: workspace.backgroundColor)
                        .opacity(HyaloOpacity.statusBarOpacity(base: workspace.backgroundAlpha))
                }
        }
        .background {
            // Theme tint overlay at configurable opacity
            Color(nsColor: workspace.backgroundColor)
                .opacity(Double(workspace.backgroundAlpha))
                .ignoresSafeArea()
        }
    }

    // MARK: - Editor Content

    private var editorContent: some View {
        EmacsNSViewRepresentable(emacsView: emacsView)
            .clipped()
    }
}
