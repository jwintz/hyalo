// UserHostDropDownView.swift - Breadcrumb segment 1: user/host display
// Target: macOS 26 Tahoe
//
// Shows user@hostname with person icon.
// Dropdown provides SSH-related actions.

import SwiftUI

@available(macOS 26.0, *)
struct UserHostDropDownView: View {
    @Environment(\.colorScheme) private var colorScheme
    @Environment(\.controlActiveState) private var activeState
    
    var model: EnvironmentBreadcrumbModel
    
    @State private var isPopoverPresented = false
    @State private var isHovering = false
    
    private var displayName: String {
        model.userHost?.displayName ?? "user@host"
    }
    
    var body: some View {
        HStack(spacing: 4) {
            label
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
        .accessibilityLabel("User and Host")
        .accessibilityValue(displayName)
        .accessibilityHint("SSH actions")
    }
    
    // MARK: - Label
    
    @ViewBuilder private var label: some View {
        HStack(spacing: 6) {
            Image(systemName: "person.crop.circle")
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
        // User/Host info
        if let userHost = model.userHost {
            VStack(alignment: .leading, spacing: 4) {
                Text(userHost.username)
                    .font(.system(size: 13, weight: .semibold))
                Text(userHost.hostname)
                    .font(.system(size: 12))
                    .foregroundStyle(.secondary)
            }
            .padding(.vertical, 4)
            
            Divider().padding(.vertical, 5)
        }
        
        // SSH Actions
        DropdownOptionView(label: "SSH to this host", isChecked: false) {
            isPopoverPresented = false
            model.onOpenTerminal?()
        }
        
        DropdownOptionView(label: "Copy SSH command", isChecked: false) {
            isPopoverPresented = false
            model.onCopySSHCommand?()
        }
    }
}
