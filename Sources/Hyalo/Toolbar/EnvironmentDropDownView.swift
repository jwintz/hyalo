// EnvironmentDropDownView.swift - Breadcrumb segment 2: dev environment display
// Target: macOS 26 Tahoe
//
// Shows detected development environments (pixi, npm, swift, bun, rust, etc.)
// Summary in pill, full list with details in dropdown.

import SwiftUI

@available(macOS 26.0, *)
struct EnvironmentDropDownView: View {
    @Environment(\.colorScheme) private var colorScheme
    @Environment(\.controlActiveState) private var activeState
    
    var model: EnvironmentBreadcrumbModel
    
    @State private var isPopoverPresented = false
    @State private var isHovering = false
    
    private var summaryText: String {
        let count = model.environments.count
        if count == 0 {
            return "No env"
        } else if count == 1, let primary = model.primaryEnvironment {
            return primary.name
        } else if let primary = model.primaryEnvironment {
            return "\(primary.name) + \(count - 1)"
        } else {
            return "\(count) envs"
        }
    }
    
    private var summaryIcon: String {
        model.primaryEnvironment?.displayIcon ?? "cube"
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
        .accessibilityLabel("Development Environment")
        .accessibilityValue(summaryText)
        .accessibilityHint("View environments")
    }
    
    // MARK: - Label
    
    @ViewBuilder private var label: some View {
        HStack(spacing: 6) {
            Image(systemName: summaryIcon)
                .imageScale(.medium)
            Text(summaryText)
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
        if model.environments.isEmpty {
            Text("No development environments detected")
                .font(.system(size: 12))
                .foregroundStyle(.secondary)
                .padding(.vertical, 4)
        } else {
            // Active environments
            if !model.activeEnvironments.isEmpty {
                Text("Active")
                    .font(.system(size: 10, weight: .semibold))
                    .foregroundStyle(.secondary)
                    .padding(.bottom, 2)
                
                ForEach(model.activeEnvironments) { env in
                    EnvironmentOptionView(
                        environment: env,
                        isChecked: true
                    ) {
                        isPopoverPresented = false
                        // Already active, just show details
                    }
                }
                
                if !model.inactiveEnvironments.isEmpty {
                    Divider().padding(.vertical, 5)
                }
            }
            
            // Inactive environments
            if !model.inactiveEnvironments.isEmpty {
                if model.activeEnvironments.isEmpty {
                    Text("Environments")
                        .font(.system(size: 10, weight: .semibold))
                        .foregroundStyle(.secondary)
                        .padding(.bottom, 2)
                }
                
                ForEach(model.inactiveEnvironments) { env in
                    EnvironmentOptionView(
                        environment: env,
                        isChecked: false
                    ) {
                        isPopoverPresented = false
                        model.onEnvironmentSwitch?(env.type)
                    }
                }
            }
            
            Divider().padding(.vertical, 5)
            
            // Actions
            DropdownOptionView(label: "Open Terminal Here", isChecked: false) {
                isPopoverPresented = false
                model.onOpenTerminal?()
            }
        }
    }
}

// MARK: - Environment Option View

@available(macOS 26.0, *)
struct EnvironmentOptionView: View {
    let environment: DevEnvironment
    let isChecked: Bool
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            HStack(spacing: 8) {
                Image(systemName: environment.displayIcon)
                    .font(.system(size: 14))
                    .foregroundStyle(.primary)
                    .frame(width: 20)
                
                VStack(alignment: .leading, spacing: 1) {
                    Text(environment.name)
                        .font(.system(size: 13))
                    Text(environment.type.capitalized)
                        .font(.system(size: 10))
                        .foregroundStyle(.secondary)
                }
                
                Spacer()
                
                if isChecked {
                    Image(systemName: "checkmark")
                        .font(.system(size: 10, weight: .semibold))
                        .foregroundStyle(Color.accentColor)
                }
            }
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .padding(.horizontal, 8)
        .padding(.vertical, 4)
    }
}
