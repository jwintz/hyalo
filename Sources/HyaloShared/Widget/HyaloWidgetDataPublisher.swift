// HyaloWidgetDataPublisher.swift - Main app side: publish Emacs state to the widget
// Target: macOS 26 Tahoe
//
// Call HyaloWidgetDataPublisher.shared.start() from the main app after init.
// The publisher periodically snapshots all running Emacs instances and writes
// the data to the App Groups shared container for the widget to read.

import Foundation

#if canImport(Darwin)
import Darwin
#endif

@available(macOS 26.0, *)
@MainActor
public final class HyaloWidgetDataPublisher {

    public static let shared = HyaloWidgetDataPublisher()

    private var timer: Timer?

    /// Per-instance metadata supplied by the main app via channels.
    /// Keyed by a stable instance identifier (e.g., Emacs frame ID).
    public var instanceMetadata: [String: InstanceMeta] = [:]

    /// Callback to reload widget timelines (set by extension host that can import WidgetKit).
    public var reloadTimelines: (() -> Void)?

    public struct InstanceMeta {
        public var pid: Int32
        public var startDate: Date
        public var bufferCount: Int
        public var currentBuffer: String
        public var currentDirectory: String
        public var gitBranch: String
        public var majorMode: String
        public var recentFiles: [String]

        public init(
            pid: Int32 = 0,
            startDate: Date = .now,
            bufferCount: Int = 0,
            currentBuffer: String = "*scratch*",
            currentDirectory: String = "~",
            gitBranch: String = "",
            majorMode: String = "fundamental-mode",
            recentFiles: [String] = []
        ) {
            self.pid = pid
            self.startDate = startDate
            self.bufferCount = bufferCount
            self.currentBuffer = currentBuffer
            self.currentDirectory = currentDirectory
            self.gitBranch = gitBranch
            self.majorMode = majorMode
            self.recentFiles = recentFiles
        }
    }

    private init() {}

    /// Start periodic publishing (every 15 seconds).
    public func start() {
        timer?.invalidate()
        timer = Timer.scheduledTimer(withTimeInterval: 15, repeats: true) { [weak self] _ in
            Task { @MainActor in
                self?.publish()
            }
        }
        publish()
    }

    public func stop() {
        timer?.invalidate()
        timer = nil
    }

    /// Snapshot current state and write to shared container.
    public func publish() {
        let instances = instanceMetadata.map { (id, meta) in
            HyaloInstanceData(
                id: id,
                pid: meta.pid,
                bufferCount: meta.bufferCount,
                currentBuffer: meta.currentBuffer,
                currentDirectory: meta.currentDirectory,
                gitBranch: meta.gitBranch,
                majorMode: meta.majorMode,
                uptimeSeconds: Date.now.timeIntervalSince(meta.startDate),
                memoryMB: Self.residentMemoryMB(pid: meta.pid),
                recentFiles: Array(meta.recentFiles.prefix(5))
            )
        }

        let widgetData = HyaloWidgetData(instances: instances, lastUpdated: .now)
        HyaloWidgetStore.write(widgetData)
        reloadTimelines?()
    }

    // MARK: - Process Memory

    /// Read RSS (resident set size) for a given PID via proc_pidinfo.
    static func residentMemoryMB(pid: Int32) -> Double {
        guard pid > 0 else { return 0 }
        var info = proc_taskinfo()
        let size = MemoryLayout<proc_taskinfo>.size
        let result = proc_pidinfo(pid, PROC_PIDTASKINFO, 0, &info, Int32(size))
        guard result == Int32(size) else { return 0 }
        return Double(info.pti_resident_size) / (1024 * 1024)
    }
}
