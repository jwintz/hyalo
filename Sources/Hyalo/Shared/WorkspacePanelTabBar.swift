// WorkspacePanelTabBar.swift - Icon tab bar for panel switching
// Target: macOS 26 Tahoe with Liquid Glass design

import SwiftUI

// MARK: - Tab Bar Position

enum HyaloTabBarPosition {
    case side    // Vertical icon strip on the leading edge
    case top     // Horizontal tab bar at the top
}

// MARK: - Panel Tab Bar

@available(macOS 26.0, *)
struct WorkspacePanelTabBar<Tab: HyaloPanelTab>: View {
    @Binding var items: [Tab]
    @Binding var selection: Tab?
    var position: HyaloTabBarPosition

    @State private var draggingTab: Tab?
    @State private var tabOffsets: [Tab: CGFloat] = [:]
    @State private var draggingStartLocation: CGFloat?
    @State private var draggingLastLocation: CGFloat?

    // AUDIT.md #7: Respect Reduce Motion accessibility setting
    @Environment(\.accessibilityReduceMotion) private var reduceMotion

    var body: some View {
        if position == .top {
            topBody
        } else {
            sideBody
        }
    }

    private var topBody: some View {
        GeometryReader { proxy in
            iconsView(size: proxy.size)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                // AUDIT.md #7: Respect Reduce Motion
                .animation(reduceMotion ? nil : .default, value: items)
        }
        .clipped()
        .frame(maxWidth: .infinity, idealHeight: 27)
        .fixedSize(horizontal: false, vertical: true)
    }

    private var sideBody: some View {
        GeometryReader { proxy in
            iconsView(size: proxy.size)
                .padding(.vertical, 5)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .animation(.default, value: items)
        }
        .clipped()
        .frame(idealWidth: 40, maxHeight: .infinity)
        .fixedSize(horizontal: true, vertical: false)
    }

    @ViewBuilder
    private func iconsView(size: CGSize) -> some View {
        let layout = position == .top
            ? AnyLayout(HStackLayout(spacing: 0))
            : AnyLayout(VStackLayout(spacing: 0))
        layout {
            ForEach(items) { tab in
                makeIcon(tab: tab, size: size)
                    .offset(offsetFor(tab))
                    .simultaneousGesture(dragGesture(for: tab))
            }
            if position == .side {
                Spacer()
            }
        }
    }

    // MARK: - Icon Button

    private func makeIcon(tab: Tab, size: CGSize) -> some View {
        Button {
            selection = tab
        } label: {
            Image(systemName: tab.systemImage)
                .font(.system(size: 12.5))
                .symbolVariant(tab == selection ? .fill : .none)
                .frame(
                    width: position == .side ? 40 : 24,
                    height: position == .side ? 28 : size.height
                )
                .contentShape(Rectangle())
                .help(tab.title)
        }
        .buttonStyle(
            IconButtonStyle(
                isActive: tab == selection,
                size: CGSize(
                    width: position == .side ? 40 : 24,
                    height: position == .side ? 28 : size.height
                )
            )
        )
        .focusable(false)
        .accessibilityIdentifier("PanelTab-\(tab.title)")
        .accessibilityLabel(tab.title)
    }

    // MARK: - Offset

    private func offsetFor(_ tab: Tab) -> CGSize {
        let offset = tabOffsets[tab] ?? 0
        return position == .top
            ? CGSize(width: offset, height: 0)
            : CGSize(width: 0, height: offset)
    }

    // MARK: - Drag Gesture

    private func dragGesture(for tab: Tab) -> some Gesture {
        DragGesture(minimumDistance: 2, coordinateSpace: .global)
            .onChanged { value in
                if draggingTab != tab {
                    draggingTab = tab
                    draggingStartLocation = position == .top
                        ? value.startLocation.x
                        : value.startLocation.y
                    draggingLastLocation = draggingStartLocation
                }

                guard let start = draggingStartLocation,
                      let last = draggingLastLocation,
                      let currentIndex = items.firstIndex(of: tab)
                else { return }

                let currentLocation = position == .top
                    ? value.location.x
                    : value.location.y
                let dragDifference = currentLocation - last
                tabOffsets[tab] = currentLocation - start

                checkSwap(tab: tab, currentIndex: currentIndex, dragDifference: dragDifference)

                let locationOnAxis = position == .top
                    ? value.location.x
                    : value.location.y
                if abs(locationOnAxis - last) >= 10 {
                    draggingLastLocation = locationOnAxis
                }
            }
            .onEnded { _ in
                draggingTab = nil
                draggingStartLocation = nil
                draggingLastLocation = nil
                // AUDIT.md #7: Respect Reduce Motion accessibility setting
                if reduceMotion {
                    tabOffsets = [:]
                } else {
                    withAnimation(.easeInOut(duration: 0.25)) {
                        tabOffsets = [:]
                    }
                }
            }
    }

    // MARK: - Swap Logic

    private func checkSwap(tab: Tab, currentIndex: Int, dragDifference: CGFloat) {
        // Previous tab
        if currentIndex > 0 && dragDifference < 0 {
            let prevTab = items[currentIndex - 1]
            let prevWidth = estimatedTabWidth(for: prevTab)
            let threshold = CGFloat(prevWidth) * 0.5

            if abs(tabOffsets[tab] ?? 0) > threshold {
                swapTabs(at: currentIndex, with: currentIndex - 1)
            }
        }

        // Next tab
        if currentIndex < items.count - 1 && dragDifference > 0 {
            let nextTab = items[currentIndex + 1]
            let nextWidth = estimatedTabWidth(for: nextTab)
            let threshold = CGFloat(nextWidth) * 0.5

            if (tabOffsets[tab] ?? 0) > threshold {
                swapTabs(at: currentIndex, with: currentIndex + 1)
            }
        }
    }

    private func swapTabs(at index1: Int, with index2: Int) {
        items.swapAt(index1, index2)

        let width1 = estimatedTabWidth(for: items[index2])
        let width2 = estimatedTabWidth(for: items[index1])
        let adjustment = CGFloat(width1 - width2) * (index1 < index2 ? -1 : 1)

        if let currentOffset = tabOffsets[items[index2]] {
            tabOffsets[items[index2]] = currentOffset + adjustment
        }

        draggingStartLocation? += CGFloat(adjustment)
    }

    private func estimatedTabWidth(for tab: Tab) -> CGFloat {
        position == .top ? 40 : 28
    }
}

// MARK: - Icon Button Style

/// Uses `.contentShape(Rectangle())` and `.controlAccentColor` for active state.
@available(macOS 26.0, *)
struct IconButtonStyle: ButtonStyle {
    var isActive: Bool
    var size: CGSize?

    @Environment(\.controlActiveState)
    private var controlActiveState
    @Environment(\.colorScheme)
    private var colorScheme

    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .foregroundColor(
                isActive
                    ? Color(.controlAccentColor)
                    : Color(.secondaryLabelColor)
            )
            .frame(width: size?.width, height: size?.height, alignment: .center)
            .contentShape(Rectangle())
            .brightness(
                configuration.isPressed
                    ? colorScheme == .dark
                        ? 0.5
                        : isActive ? -0.25 : -0.75
                    : 0
            )
            .opacity(controlActiveState == .inactive ? 0.5 : 1)
    }
}
