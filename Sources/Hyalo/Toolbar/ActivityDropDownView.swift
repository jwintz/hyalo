// ActivityDropDownView.swift - Breadcrumb segment 2: activity / tab switcher
// Target: macOS 26 Tahoe
//
// Shows the current tab-bar tab name with an activity icon.
// A dropdown lists all tabs so the user can switch, create, or close them.
// Matches the CodeEdit TaskDropDownView pattern.

import SwiftUI

@available(macOS 26.0, *)
struct ActivityDropDownView: View {
    @Environment(\.colorScheme) private var colorScheme
    @Environment(\.controlActiveState) private var activeState

    var model: ActivityBreadcrumbModel

    @State private var isPopoverPresented = false
    @State private var isHovering = false

    private var displayName: String {
        model.currentTab?.name ?? "Default"
    }

    var body: some View {
        HStack(spacing: 4) {
            label
            chevronDown
                .opacity(isHovering || isPopoverPresented ? 1 : 0)
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
        .accessibilityLabel("Activity")
        .accessibilityValue(displayName)
        .accessibilityHint("Switch activity")
        .accessibilityAction { isPopoverPresented = true }
    }

    // MARK: - Label

    @ViewBuilder private var label: some View {
        HStack(spacing: 6) {
            Image(systemName: "macwindow.on.rectangle")
                .imageScale(.medium)
            Text(displayName)
                .font(.subheadline)
                .lineLimit(1)
                .frame(minWidth: 0)
        }
    }

    // MARK: - Chevron

    @ViewBuilder private var chevronDown: some View {
        Image(systemName: "chevron.down")
            .font(.system(size: 8, weight: .semibold))
            .padding(.top, 0.5)
            .padding(.trailing, 2)
    }

    // MARK: - Popover Content

    @ViewBuilder private var popoverContent: some View {
        // Tab list (only shown when there are multiple tabs)
        if model.tabs.count > 1 {
            ForEach(model.tabs) { tab in
                DropdownOptionView(
                    label: tab.name,
                    isChecked: tab.isCurrent
                ) {
                    isPopoverPresented = false
                    model.onTabSwitch?(tab.name)
                }
            }
            Divider().padding(.vertical, 5)
        }
        DropdownOptionView(label: "New Activity…", isChecked: false) {
            isPopoverPresented = false
            model.onTabNew?()
        }
        if model.tabs.count > 1, let current = model.currentTab {
            DropdownOptionView(label: "Close Activity", isChecked: false) {
                isPopoverPresented = false
                model.onTabClose?(current.name)
            }
        }
    }
}
