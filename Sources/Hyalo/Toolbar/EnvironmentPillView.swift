// EnvironmentPillView.swift - Toolbar breadcrumb: user/host | environment | build status
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Three-segment layout in a Capsule pill (Tahoe):
//
//   [UserHostDropDownView] [EnvironmentDropDownView]  [Build status + spinner]
//     person icon + user@host  env icon + summary      notification text + ring
//
// Segment 1 (UserHostDropDownView): shows user@hostname with SSH actions.
// Segment 2 (EnvironmentDropDownView): shows detected dev environments.
// Segment 3: inline build/activity progress (existing, unchanged).
//
// State for segments 1 and 2 is pushed from Emacs via the hyalo-environment
// channel. State for segment 3 is pushed via ActivityManager (unchanged).

import SwiftUI

// MARK: - Environment Pill View (entry point from HyaloNavigationLayout)

@available(macOS 26.0, *)
struct EnvironmentPillView: View {
    @Bindable var workspace: HyaloWorkspaceState

    var model: EnvironmentBreadcrumbModel = EnvironmentBreadcrumbModel.shared

    var body: some View {
        BreadcrumbContent(workspace: workspace, model: model)
    }
}

// MARK: - Breadcrumb Content

@available(macOS 26.0, *)
private struct BreadcrumbContent: View {
    @Environment(\.colorScheme) private var colorScheme

    var workspace: HyaloWorkspaceState
    var model: EnvironmentBreadcrumbModel
    var activityManager = ActivityManager.shared

    var body: some View {
        HStack(spacing: 0) {
            // Segment 1: user/host
            UserHostDropDownView(model: model)

            // Segment 2: environment
            EnvironmentDropDownView(model: model)

            Spacer(minLength: 0)

            // Segment 3: build/activity status (existing, right-aligned)
            BuildStatusView(activityManager: activityManager)
                .fixedSize()
                .padding(.trailing, 4)
        }
        .clipShape(Capsule())
        .padding(5)
        // Dynamic width: min width for usability, hug content
        .frame(minWidth: 100)
        .fixedSize(horizontal: true, vertical: false)
    }
}

// MARK: - Build Status View (unchanged from ActivityViewerView)

@available(macOS 26.0, *)
private struct BuildStatusView: View {
    @Environment(\.controlActiveState) private var activeState

    var activityManager: ActivityManager

    @State private var isPresented = false
    @State private var displayed: ActivityItem?

    var body: some View {
        Group {
            if let activity = displayed {
                HStack(spacing: 6) {
                    buildStatusText(activity)
                    buildStatusIndicator(activity)
                }
                .transition(.opacity.combined(with: .move(edge: .trailing)))
            }
        }
        .animation(.easeInOut, value: displayed)
        .opacity(activeState == .inactive ? 0.4 : 1.0)
        .popover(isPresented: $isPresented, arrowEdge: .bottom) {
            BuildStatusDetailView(activityManager: activityManager)
        }
        .onTapGesture { isPresented.toggle() }
        .onChange(of: activityManager.activities) { _, activities in
            withAnimation {
                displayed = activities.first(where: \.isActive) ?? activities.first
            }
        }
        .onAppear {
            displayed = activityManager.activities.first(where: \.isActive)
                ?? activityManager.activities.first
        }
    }

    @ViewBuilder
    private func buildStatusText(_ activity: ActivityItem) -> some View {
        Text(activity.title)
            .font(.subheadline)
            .lineLimit(1)
            .truncationMode(.tail)
            .transition(
                .asymmetric(
                    insertion: .move(edge: .top),
                    removal: .move(edge: .bottom)
                )
                .combined(with: .opacity)
            )
            .id("BuildStatus-" + activity.id + activity.title)
    }

    @ViewBuilder
    private func buildStatusIndicator(_ activity: ActivityItem) -> some View {
        if activity.isActive {
            ActivityCircularProgressView(
                progress: activity.progress,
                taskCount: activityManager.activeCount
            )
            .frame(width: 16, height: 16)
        } else if activityManager.activeCount == 0,
                  activityManager.activities.count > 1 {
            Text("\(activityManager.activities.count)")
                .font(.caption)
                .padding(4)
                .background(Circle().foregroundStyle(.secondary.opacity(0.2)))
        }
    }
}

// MARK: - Circular Progress View (indeterminate or determinate ring)

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
                        .stroke(Color.accentColor,
                                style: StrokeStyle(lineWidth: lineWidth, lineCap: .round))
                        .animation(.easeInOut, value: progress)
                } else {
                    Circle()
                        .trim(from: 0, to: 0.5)
                        .stroke(Color.accentColor,
                                style: StrokeStyle(lineWidth: lineWidth, lineCap: .round))
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

// MARK: - Build Status Detail Popover

@available(macOS 26.0, *)
private struct BuildStatusDetailView: View {
    var activityManager: ActivityManager

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
                        BuildStatusDetailRow(activity: activity, activityManager: activityManager)
                    }
                }
            }
        }
        .padding(15)
        .frame(minWidth: 320, maxWidth: 400)
    }
}

@available(macOS 26.0, *)
private struct BuildStatusDetailRow: View {
    let activity: ActivityItem
    var activityManager: ActivityManager

    var body: some View {
        HStack(alignment: .center, spacing: 8) {
            if activity.isActive {
                ActivityCircularProgressView(progress: activity.progress)
                    .frame(width: 16, height: 16)
            } else {
                Image(systemName: "checkmark.circle.fill")
                    .foregroundStyle(.green)
                    .font(.system(size: 14))
            }

            VStack(alignment: .leading, spacing: 2) {
                Text(activity.kind.label)
                    .font(.system(size: 12, weight: .semibold))
                Text(activity.title)
                    .font(.system(size: 11))
                    .foregroundStyle(.secondary)
                if !activity.message.isEmpty {
                    Text(activity.message)
                        .font(.system(size: 10))
                        .foregroundStyle(.tertiary)
                }
            }

            Spacer()

            if !activity.isActive && activity.kind == .moduleCompilation,
               let onReload = activityManager.onModuleReload {
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

        if !activity.logLines.isEmpty {
            Divider()
            ScrollViewReader { proxy in
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 2) {
                        ForEach(Array(activity.logLines.enumerated()), id: \.offset) { idx, line in
                            Text(line)
                                .font(.system(size: 10, design: .monospaced))
                                .foregroundStyle(.secondary)
                                .textSelection(.enabled)
                                .id(idx)
                        }
                    }
                }
                .frame(maxHeight: 150)
                .onChange(of: activity.logLines.count) { _, count in
                    if count > 0 { proxy.scrollTo(count - 1, anchor: .bottom) }
                }
            }
        }
    }
}
