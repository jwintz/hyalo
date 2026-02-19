// ActivityViewerView.swift - Build/activity status in toolbar center
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Breadcrumb layout in a fixed-width toolbar segment:
//   [kind icon] Activity Title  [progress/badge] [chevron ▾]
//
// - The kind icon animates (e.g., gearshape rotates for native compilation)
// - Text is clipped to the pill bounds (no overflow during transitions)
// - Clicking the chevron opens a menu to switch between activities
// - Clicking the pill body opens a detail popover
// - Fixed width prevents toolbar layout shifts when text changes

import SwiftUI

// MARK: - Activity Viewer View

@available(macOS 26.0, *)
struct ActivityViewerView: View {
    @Bindable var workspace: HyaloWorkspaceState

    var body: some View {
        ActivityToolbarContent()
    }
}

// MARK: - Toolbar Content

@available(macOS 26.0, *)
private struct ActivityToolbarContent: View {
    var activityManager = ActivityManager.shared

    @Environment(\.controlActiveState)
    private var activeState

    @State private var isHovering = false
    @State private var showPopover = false

    /// The notification currently displayed inline.
    private var displayed: ActivityItem? {
        activityManager.activities.first(where: { $0.isActive })
            ?? activityManager.activities.first
    }

    var body: some View {
        HStack(spacing: 6) {
            // Activity content (breadcrumb)
            activityBreadcrumb
                .clipped()
                .contentShape(Rectangle())
                .onTapGesture { showPopover.toggle() }

            // Switcher chevron (when multiple activities)
            if activityManager.activities.count > 1 {
                activitySwitcherMenu
            }
        }
        .padding(.horizontal, 8)
        .opacity(activeState == .inactive ? 0.4 : 1.0)
        .onHover { isHovering = $0 }
        .popover(isPresented: $showPopover) {
            ActivityPopoverView()
        }
    }

    // MARK: - Breadcrumb

    @ViewBuilder
    private var activityBreadcrumb: some View {
        if let activity = displayed {
            HStack(spacing: 6) {
                // Kind icon with animation
                kindIcon(activity)

                // Title
                Text(activity.title)
                    .font(.subheadline)
                    .lineLimit(1)
                    .truncationMode(.tail)
                    .id("ActivityTitle-" + activity.id + activity.title)
                    .transition(
                        .asymmetric(
                            insertion: .move(edge: .top),
                            removal: .move(edge: .bottom)
                        )
                        .combined(with: .opacity)
                    )

                // Progress bar (determinate only)
                if activity.isActive, let progress = activity.progress, progress > 0 {
                    ProgressView(value: progress)
                        .progressViewStyle(.linear)
                        .frame(width: 40)
                }

                // Indicator
                trailingIndicator(activity)
            }
            .animation(.easeInOut(duration: 0.25), value: activity.id)
            .animation(.easeInOut(duration: 0.25), value: activity.title)
        } else {
            HStack(spacing: 6) {
                Image(systemName: "checkmark.circle.fill")
                    .foregroundStyle(.green)
                    .font(.system(size: 11))
                Text("Ready")
                    .font(.subheadline)
            }
        }
    }

    // MARK: - Kind Icon

    @ViewBuilder
    private func kindIcon(_ activity: ActivityItem) -> some View {
        Image(systemName: activity.kind.systemImage)
            .font(.system(size: 11))
            .foregroundStyle(activity.isActive ? .primary : .secondary)
            .symbolEffect(.rotate, isActive: activity.isActive && activity.kind == .nativeCompilation)
            .symbolEffect(.pulse, isActive: activity.isActive && activity.kind == .packageInstallation)
    }

    // MARK: - Trailing Indicator

    @ViewBuilder
    private func trailingIndicator(_ activity: ActivityItem) -> some View {
        if activity.isActive {
            ActivityCircularProgressView(
                progress: activity.progress,
                taskCount: activityManager.activeCount
            )
            .frame(width: 16, height: 16)
        } else if activity.kind == .moduleCompilation,
                  let onReload = activityManager.onModuleReload {
            Button {
                onReload()
            } label: {
                Image(systemName: "arrow.clockwise")
                    .font(.system(size: 10, weight: .semibold))
            }
            .buttonStyle(.plain)
            .help("Reload Hyalo module")
        }
    }

    // MARK: - Activity Switcher Menu

    private var activitySwitcherMenu: some View {
        Menu {
            ForEach(activityManager.activities) { activity in
                Button {
                    // Move this activity to the front so it becomes displayed
                    activityManager.bringToFront(id: activity.id)
                } label: {
                    Label {
                        Text(activity.title)
                    } icon: {
                        Image(systemName: activity.isActive
                              ? "circle.fill"
                              : "checkmark.circle.fill")
                    }
                }
            }
        } label: {
            Image(systemName: "chevron.down")
                .font(.system(size: 8, weight: .semibold))
                .foregroundStyle(.tertiary)
                .frame(width: 16, height: 16)
                .contentShape(Rectangle())
        }
        .menuStyle(.borderlessButton)
        .menuIndicator(.hidden)
        .fixedSize()
        .help("Switch activity")
    }
}

// MARK: - Circular Progress View

@available(macOS 26.0, *)
struct ActivityCircularProgressView: View {
    @State private var isAnimating = false
    @State private var previousValue = false

    var progress: Double?
    var taskCount: Int = 1

    private let lineWidth: CGFloat = 2

    var body: some View {
        Circle()
            .stroke(style: StrokeStyle(lineWidth: lineWidth))
            .foregroundStyle(.tertiary)
            .overlay {
                if let progress {
                    Circle()
                        .trim(from: 0, to: progress)
                        .stroke(Color.accentColor, style: StrokeStyle(
                            lineWidth: lineWidth, lineCap: .round
                        ))
                        .animation(.easeInOut, value: progress)
                } else {
                    Circle()
                        .trim(from: 0, to: 0.5)
                        .stroke(Color.accentColor, style: StrokeStyle(
                            lineWidth: lineWidth, lineCap: .round
                        ))
                        .rotationEffect(
                            previousValue
                                ? .degrees(isAnimating ? 0 : -360)
                                : .degrees(isAnimating ? 360 : 0)
                        )
                        .animation(
                            .linear(duration: 1).repeatForever(autoreverses: false),
                            value: isAnimating
                        )
                        .onAppear {
                            previousValue = isAnimating
                            isAnimating.toggle()
                        }
                }
            }
            .rotationEffect(.degrees(-90))
            .padding(lineWidth / 2)
            .overlay {
                if taskCount > 1 {
                    Text("\(taskCount)")
                        .font(.system(size: 8, weight: .medium))
                        .foregroundStyle(.secondary)
                }
            }
            .accessibilityElement()
            .accessibilityAddTraits(.updatesFrequently)
            .accessibilityValue(
                progress != nil ? Text(progress!, format: .percent) : Text("working")
            )
    }
}

// MARK: - Activity Popover (detail view)

@available(macOS 26.0, *)
private struct ActivityPopoverView: View {
    var activityManager = ActivityManager.shared

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 12) {
                if activityManager.activities.isEmpty {
                    HStack {
                        Image(systemName: "checkmark.circle.fill")
                            .foregroundStyle(.green)
                        Text("Ready")
                            .font(.system(size: 13, weight: .semibold))
                    }
                } else {
                    ForEach(activityManager.activities) { activity in
                        ActivityDetailRow(activity: activity)
                    }
                }
            }
        }
        .padding(15)
        .frame(minWidth: 320, maxWidth: 400)
    }
}

// MARK: - Activity Detail Row

@available(macOS 26.0, *)
private struct ActivityDetailRow: View {
    let activity: ActivityItem
    var activityManager = ActivityManager.shared

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack(alignment: .center, spacing: 8) {
                if activity.isActive {
                    ActivityCircularProgressView(
                        progress: activity.progress
                    )
                    .frame(width: 16, height: 16)
                } else {
                    Image(systemName: "checkmark.circle.fill")
                        .foregroundStyle(.green)
                        .font(.system(size: 14))
                }

                VStack(alignment: .leading, spacing: 2) {
                    HStack {
                        Text(activity.kind.label)
                            .font(.system(size: 12, weight: .semibold))
                        Spacer()
                        if let progress = activity.progress, activity.isActive {
                            Text("\(Int(progress * 100))%")
                                .font(.system(size: 11, design: .monospaced))
                                .foregroundStyle(.secondary)
                        }
                    }

                    Text(activity.title)
                        .font(.system(size: 11))
                        .foregroundStyle(.secondary)

                    if !activity.message.isEmpty {
                        Text(activity.message)
                            .font(.system(size: 10))
                            .foregroundStyle(.tertiary)
                    }
                }

                if !activity.isActive && activity.kind == .moduleCompilation {
                    if let onReload = activityManager.onModuleReload {
                        Button {
                            onReload()
                        } label: {
                            Image(systemName: "arrow.clockwise")
                                .font(.system(size: 11))
                        }
                        .buttonStyle(.bordered)
                        .controlSize(.small)
                        .help("Reload module")
                    }
                }
            }

            if !activity.logLines.isEmpty {
                Divider()
                ScrollViewReader { proxy in
                    ScrollView {
                        LazyVStack(alignment: .leading, spacing: 2) {
                            ForEach(
                                Array(activity.logLines.enumerated()),
                                id: \.offset
                            ) { index, line in
                                Text(line)
                                    .font(.system(size: 10, design: .monospaced))
                                    .foregroundStyle(.secondary)
                                    .textSelection(.enabled)
                                    .id(index)
                            }
                        }
                    }
                    .frame(maxHeight: 150)
                    .onChange(of: activity.logLines.count) { _, count in
                        if count > 0 {
                            proxy.scrollTo(count - 1, anchor: .bottom)
                        }
                    }
                }
            }
        }
        .padding(8)
    }
}
