// HyaloWidgetData.swift - Data model for the Hyalo desktop widget
// Target: macOS 26 Tahoe with Liquid Glass design
// Shared between the main app (writer) and widget extension (reader)

import Foundation

// MARK: - App Groups

/// Shared container identifiers for main app ↔ widget communication.
public enum HyaloAppGroup {
    public static let suiteName = "group.org.gnu.hyalo"
    public static let widgetDataKey = "hyalo_widget_data"
}

// MARK: - Per-Instance Data

/// State snapshot of a single Emacs instance (one fork-emacs process).
public struct HyaloInstanceData: Codable, Sendable, Identifiable {
    public var id: String
    public var pid: Int32
    public var bufferCount: Int
    public var currentBuffer: String
    public var currentDirectory: String
    public var gitBranch: String
    public var majorMode: String
    public var uptimeSeconds: TimeInterval
    public var memoryMB: Double
    public var recentFiles: [String]

    public init(
        id: String = UUID().uuidString,
        pid: Int32 = 0,
        bufferCount: Int = 0,
        currentBuffer: String = "*scratch*",
        currentDirectory: String = "~",
        gitBranch: String = "",
        majorMode: String = "fundamental-mode",
        uptimeSeconds: TimeInterval = 0,
        memoryMB: Double = 0,
        recentFiles: [String] = []
    ) {
        self.id = id
        self.pid = pid
        self.bufferCount = bufferCount
        self.currentBuffer = currentBuffer
        self.currentDirectory = currentDirectory
        self.gitBranch = gitBranch
        self.majorMode = majorMode
        self.uptimeSeconds = uptimeSeconds
        self.memoryMB = memoryMB
        self.recentFiles = recentFiles
    }

    /// Human-readable uptime, e.g. "2h 14m"
    public var formattedUptime: String {
        let hours = Int(uptimeSeconds) / 3600
        let minutes = (Int(uptimeSeconds) % 3600) / 60
        if hours > 0 {
            return "\(hours)h \(minutes)m"
        }
        return "\(minutes)m"
    }

    /// Short directory, collapsing $HOME to ~
    public var shortDirectory: String {
        let home = NSHomeDirectory()
        if currentDirectory.hasPrefix(home) {
            return "~" + currentDirectory.dropFirst(home.count)
        }
        return currentDirectory
    }

    /// Pretty major mode: "emacs-lisp-mode" → "Emacs Lisp"
    public var prettyMode: String {
        majorMode
            .replacingOccurrences(of: "-mode", with: "")
            .split(separator: "-")
            .map { $0.prefix(1).uppercased() + $0.dropFirst() }
            .joined(separator: " ")
    }
}

// MARK: - Aggregate Widget Data

/// Top-level payload written by the main app, read by the widget.
public struct HyaloWidgetData: Codable, Sendable {
    public var instances: [HyaloInstanceData]
    public var lastUpdated: Date

    public init(instances: [HyaloInstanceData] = [], lastUpdated: Date = .now) {
        self.instances = instances
        self.lastUpdated = lastUpdated
    }

    public var instanceCount: Int { instances.count }

    /// The most recently active instance (first in list by convention).
    public var activeInstance: HyaloInstanceData? { instances.first }
}

// MARK: - Shared Container Read/Write

public enum HyaloWidgetStore {

    /// Shared file in the App Group container.
    /// The unsandboxed main app creates the directory and writes here.
    /// The sandboxed widget (with App Group entitlement) reads via the same path.
    private static var sharedFileURL: URL {
        let groupDir = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent("Library/Group Containers")
            .appendingPathComponent(HyaloAppGroup.suiteName)
        return groupDir.appendingPathComponent("widget-data.json")
    }

    /// Write widget data to the shared App Group container.
    public static func write(_ data: HyaloWidgetData) {
        let url = sharedFileURL
        do {
            try FileManager.default.createDirectory(
                at: url.deletingLastPathComponent(),
                withIntermediateDirectories: true
            )
            let encoded = try JSONEncoder().encode(data)
            try encoded.write(to: url, options: .atomic)
        } catch {
            #if DEBUG
            NSLog("[HyaloWidgetStore] write failed: \(error)")
            #endif
        }
    }

    /// Read widget data from the shared App Group container.
    public static func read() -> HyaloWidgetData {
        let url = sharedFileURL
        guard let data = try? Data(contentsOf: url),
              let decoded = try? JSONDecoder().decode(HyaloWidgetData.self, from: data)
        else {
            return .placeholder
        }
        return decoded
    }
}

// MARK: - Placeholder / Preview Data

extension HyaloWidgetData {
    public static let placeholder = HyaloWidgetData(
        instances: [.placeholder],
        lastUpdated: .now
    )

    public static let previewMultiple = HyaloWidgetData(
        instances: [
            HyaloInstanceData(
                id: "1", pid: 1234, bufferCount: 12,
                currentBuffer: "init.el",
                currentDirectory: "~/Syntropment/hyalo",
                gitBranch: "main",
                majorMode: "emacs-lisp-mode",
                uptimeSeconds: 8040, memoryMB: 142.3,
                recentFiles: ["init.el", "Package.swift", "README.md", "PLAN.md"]
            ),
            HyaloInstanceData(
                id: "2", pid: 5678, bufferCount: 5,
                currentBuffer: "main.swift",
                currentDirectory: "~/Projects/myapp",
                gitBranch: "feature/widgets",
                majorMode: "swift-mode",
                uptimeSeconds: 3600, memoryMB: 98.7,
                recentFiles: ["main.swift", "AppDelegate.swift"]
            ),
        ],
        lastUpdated: .now
    )
}

extension HyaloInstanceData {
    public static let placeholder = HyaloInstanceData(
        id: "placeholder", pid: 0, bufferCount: 3,
        currentBuffer: "*scratch*",
        currentDirectory: "~",
        gitBranch: "main",
        majorMode: "lisp-interaction-mode",
        uptimeSeconds: 0, memoryMB: 0,
        recentFiles: ["*scratch*"]
    )
}
