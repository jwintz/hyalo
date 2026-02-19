// ActivityModel.swift - Activity notification model
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Three activity types:
// - Native compilation (elisp .eln)
// - Dynamic module compilation (swift build)
// - Package installation (package.el)
//
// Activities are displayed in the toolbar activity viewer.
// The most recent active notification is shown inline; a popover
// lists all notifications with detail. No polling — all updates
// are push-based from Emacs channels or file system events.

import Foundation
import SwiftUI

// MARK: - Activity Kind

@available(macOS 26.0, *)
enum ActivityKind: String, Identifiable, CaseIterable {
    case nativeCompilation = "native-compilation"
    case moduleCompilation = "module-compilation"
    case packageInstallation = "package-installation"

    var id: String { rawValue }

    var label: String {
        switch self {
        case .nativeCompilation: return "Native Compilation"
        case .moduleCompilation: return "Module Build"
        case .packageInstallation: return "Package Installation"
        }
    }

    var systemImage: String {
        switch self {
        case .nativeCompilation: return "gearshape.2"
        case .moduleCompilation: return "hammer"
        case .packageInstallation: return "shippingbox"
        }
    }
}

// MARK: - Activity Item

@available(macOS 26.0, *)
struct ActivityItem: Identifiable, Equatable {
    let id: String
    let kind: ActivityKind
    var title: String
    var message: String
    var progress: Double?     // nil = indeterminate, 0.0–1.0 = determinate
    var isActive: Bool
    var logLines: [String]

    static func == (lhs: ActivityItem, rhs: ActivityItem) -> Bool {
        lhs.id == rhs.id
            && lhs.title == rhs.title
            && lhs.message == rhs.message
            && lhs.progress == rhs.progress
            && lhs.isActive == rhs.isActive
            && lhs.logLines.count == rhs.logLines.count
    }
}

// MARK: - Activity Manager

@available(macOS 26.0, *)
@MainActor
@Observable
final class ActivityManager {
    static let shared = ActivityManager()

    /// All current activities, most recent first.
    var activities: [ActivityItem] = []

    /// Number of active (in-progress) activities.
    var activeCount: Int {
        activities.filter(\.isActive).count
    }

    /// True if any activity is in progress.
    var hasActiveWork: Bool {
        activeCount > 0
    }

    // MARK: - Mutations

    func upsert(id: String, kind: ActivityKind, title: String, message: String = "",
                progress: Double? = nil, isActive: Bool = true) {
        if let idx = activities.firstIndex(where: { $0.id == id }) {
            activities[idx].title = title
            activities[idx].message = message
            activities[idx].progress = progress
            activities[idx].isActive = isActive
        } else {
            let item = ActivityItem(
                id: id, kind: kind, title: title, message: message,
                progress: progress, isActive: isActive, logLines: []
            )
            // Insert at beginning (most recent first)
            activities.insert(item, at: 0)
        }
    }

    func appendLog(id: String, line: String) {
        guard let idx = activities.firstIndex(where: { $0.id == id }) else { return }
        activities[idx].logLines.append(line)
        // Keep last 200 lines
        if activities[idx].logLines.count > 200 {
            activities[idx].logLines.removeFirst(activities[idx].logLines.count - 200)
        }
    }

    func clearLog(id: String) {
        guard let idx = activities.firstIndex(where: { $0.id == id }) else { return }
        activities[idx].logLines.removeAll()
    }

    func finish(id: String, message: String = "") {
        guard let idx = activities.firstIndex(where: { $0.id == id }) else { return }
        activities[idx].isActive = false
        activities[idx].progress = nil
        if !message.isEmpty {
            activities[idx].message = message
        }
    }

    func remove(id: String) {
        activities.removeAll { $0.id == id }
    }

    /// Move an activity to the front of the list so it becomes
    /// the displayed notification in the toolbar.
    func bringToFront(id: String) {
        guard let idx = activities.firstIndex(where: { $0.id == id }), idx != 0 else { return }
        let item = activities.remove(at: idx)
        activities.insert(item, at: 0)
    }

    func removeAfterDelay(id: String, delay: TimeInterval = 5.0) {
        DispatchQueue.main.asyncAfter(deadline: .now() + delay) { [weak self] in
            self?.remove(id: id)
        }
    }

    // MARK: - Convenience: Native Compilation

    /// Stable ID for the native compilation activity.
    static let nativeCompilationID = "native-compilation"

    func startNativeCompilation(total: Int) {
        upsert(
            id: Self.nativeCompilationID,
            kind: .nativeCompilation,
            title: "Compiling \(total) file\(total == 1 ? "" : "s")…",
            progress: 0.0
        )
    }

    func updateNativeCompilation(done: Int, total: Int) {
        let progress = total > 0 ? Double(done) / Double(total) : 0.0
        let remaining = total - done
        upsert(
            id: Self.nativeCompilationID,
            kind: .nativeCompilation,
            title: "Native compiling \(remaining) file\(remaining == 1 ? "" : "s")…",
            progress: progress
        )
    }

    func finishNativeCompilation(compiled: Int) {
        finish(
            id: Self.nativeCompilationID,
            message: compiled > 0
                ? "Compiled \(compiled) file\(compiled == 1 ? "" : "s")"
                : "All packages natively compiled"
        )
        removeAfterDelay(id: Self.nativeCompilationID, delay: 8.0)
    }

    // MARK: - Convenience: Module Build

    /// Stable ID for the module build activity.
    static let moduleBuildID = "module-build"

    /// Callback for reload action. Set by Module.swift channel setup.
    var onModuleReload: (() -> Void)?

    func startModuleBuild() {
        upsert(
            id: Self.moduleBuildID,
            kind: .moduleCompilation,
            title: "Building Hyalo module…"
        )
    }

    func updateModuleBuild(message: String) {
        upsert(
            id: Self.moduleBuildID,
            kind: .moduleCompilation,
            title: "Building Hyalo module…",
            message: message
        )
    }

    func finishModuleBuild(success: Bool) {
        finish(
            id: Self.moduleBuildID,
            message: success ? "Build succeeded" : "Build failed"
        )
        // Do not auto-remove — show reload button
    }

    // MARK: - Convenience: Package Installation

    /// Stable ID for the package installation activity.
    static let packageInstallationID = "package-installation"

    func startPackageInstallation(name: String) {
        upsert(
            id: Self.packageInstallationID,
            kind: .packageInstallation,
            title: "Installing \(name)…"
        )
    }

    func updatePackageInstallation(message: String) {
        upsert(
            id: Self.packageInstallationID,
            kind: .packageInstallation,
            title: message
        )
    }

    func finishPackageInstallation(message: String = "Packages up to date") {
        finish(id: Self.packageInstallationID, message: message)
        removeAfterDelay(id: Self.packageInstallationID, delay: 5.0)
    }

    private init() {}
}
