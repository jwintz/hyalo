// UserHostDropDownView.swift - Breadcrumb segment 1: user/host display
// Target: macOS 26 Tahoe
//
// Shows user@hostname with person icon.
// Dropdown provides SSH-related actions.

import SwiftUI

@available(macOS 26.0, *)
public struct UserHostDropDownView: View {
    @Environment(\.colorScheme) private var colorScheme
    @Environment(\.controlActiveState) private var activeState
    
    public var model: EnvironmentBreadcrumbModel
    public var compact: Bool = false

    @State private var isPopoverPresented = false
    @State private var isHovering = false
    
    private var displayName: String {
        model.userHost?.displayName ?? "user@host"
    }

    // Lightweight hover background to reduce type complexity in body (moved up for visibility)
    private var hoverBackground: some View {
        Capsule()
            .fill(Color.primary)
            .opacity(isHovering || isPopoverPresented ? 0.05 : 0)
    }
    
    public var body: some View {
        HStack(spacing: 4) {
            label
            chevron
        }
        .padding(.horizontal, 6)
        .padding(.vertical, 6)
        .background( hoverBackground )
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

    public init(model: EnvironmentBreadcrumbModel, compact: Bool = false) {
        self.model = model
        self.compact = compact
    }
    
    // MARK: - Label
    
    @ViewBuilder private var label: some View {
        HStack(spacing: 6) {
            Image(systemName: "person.crop.circle")
                .imageScale(.medium)
            if !compact {
                Text(displayName)
                    .font(.subheadline)
                    .lineLimit(1)
                    .frame(minWidth: 0)
            }
        }
    }
    
    // MARK: - Chevron

    @ViewBuilder private var chevron: some View {
        Image(systemName: "chevron.down")
            .font(.system(size: 8, weight: .semibold))
            .foregroundStyle(.secondary)
            .rotationEffect(
                .degrees(isHovering || isPopoverPresented ? 0 : -90),
                anchor: .center
            )
            .animation(.easeInOut(duration: 0.2), value: isHovering || isPopoverPresented)
            .frame(width: 12, height: 12)
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
            model.onSSHHost?()
        }
        
        DropdownOptionView(label: "Copy SSH command", isChecked: false) {
            isPopoverPresented = false
            model.onCopySSHCommand?()
        }
    }

    
}
