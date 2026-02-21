// WorkspaceDropDownView.swift - Breadcrumb segment 1: workspace / frame switcher
// Target: macOS 26 Tahoe
//
// Shows the current project name with a folder icon.
// A dropdown lists all decorated Emacs frames so the user can switch
// between open projects.  Matches the CodeEdit SchemeDropDownView pattern.

import SwiftUI

@available(macOS 26.0, *)
struct WorkspaceDropDownView: View {
    @Environment(\.colorScheme) private var colorScheme
    @Environment(\.controlActiveState) private var activeState

    var model: ActivityBreadcrumbModel
    var workspace: HyaloWorkspaceState

    @State private var isPopoverPresented = false
    @State private var isHovering = false

    private var displayName: String {
        if !workspace.projectName.isEmpty { return workspace.projectName }
        if let current = model.currentFrame { return current.name }
        return "Emacs"
    }

    var body: some View {
        HStack(spacing: 4) {
            label
            // Right chevron is the breadcrumb separator; becomes down chevron on hover
            chevronSeparator
                .opacity(isHovering || isPopoverPresented ? 0 : 1)
        }
        .background {
            if isHovering || isPopoverPresented {
                HStack {
                    Spacer()
                    chevronDown
                }
            }
        }
        .padding(.horizontal, 6)
        .padding(.vertical, 6)
        .background {
            Color(nsColor: colorScheme == .dark ? .white : .black)
                .opacity(isHovering || isPopoverPresented ? 0.05 : 0)
                .clipShape(Capsule())
        }
        .onHover { isHovering = $0 }
        .instantPopover(isPresented: $isPopoverPresented, arrowEdge: .bottom) {
            popoverContent
        }
        .onTapGesture { isPopoverPresented.toggle() }
        .opacity(activeState == .inactive ? 0.4 : 1.0)
        .accessibilityElement(children: .combine)
        .accessibilityAddTraits(.isButton)
        .accessibilityLabel("Workspace")
        .accessibilityValue(displayName)
        .accessibilityHint("Switch workspace")
    }

    // MARK: - Label

    @ViewBuilder private var label: some View {
        HStack(spacing: 6) {
            Image(systemName: "folder.badge.gearshape")
                .imageScale(.medium)
            Text(displayName)
                .font(.subheadline)
                .lineLimit(1)
                .frame(minWidth: 0)
        }
    }

    // MARK: - Chevrons

    @ViewBuilder private var chevronSeparator: some View {
        Image(systemName: "chevron.compact.right")
            .font(.system(size: 9, weight: .medium))
            .foregroundStyle(.secondary)
            .scaleEffect(x: 1.3, y: 1.0, anchor: .center)
            .imageScale(.large)
    }

    @ViewBuilder private var chevronDown: some View {
        Image(systemName: "chevron.down")
            .font(.system(size: 8, weight: .semibold))
            .padding(.top, 0.5)
            .padding(.trailing, 2)
    }

    // MARK: - Popover Content

    @ViewBuilder private var popoverContent: some View {
        // Frame list
        if model.frames.isEmpty {
            DropdownOptionView(label: displayName, isChecked: true) {}
        } else {
            ForEach(model.frames) { frame in
                DropdownOptionView(
                    label: frame.name,
                    isChecked: frame.isCurrent
                ) {
                    isPopoverPresented = false
                    model.onFrameSwitch?(frame.id)
                }
            }
        }
        Divider().padding(.vertical, 5)
        DropdownOptionView(label: "Open Folder…", isChecked: false) {
            isPopoverPresented = false
            // Sends to the Emacs minibuffer via standard find-file mechanism
            NSApp.sendAction(#selector(NSDocumentController.openDocument(_:)), to: nil, from: nil)
        }
    }
}
