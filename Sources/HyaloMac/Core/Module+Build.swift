// Module+Build.swift - Build watcher, async build process, build channel
// Target: macOS 26 Tahoe

import AppKit
import HyaloShared
import CoreServices
import EmacsSwiftModule
import SwiftUI

extension HyaloModule {

    func setupBuildBindings(_ env: EmacsSwiftModule.Environment) throws {

        // MARK: - Module Build Watcher

        try env.defun("hyalo-start-build-watcher",
            with: """
            Start watching the .build/ directory for swift build completions.
            BASE-DIR is the project root (where Package.swift lives).
            Uses kqueue file system events — no polling.
            """
        ) { (env: EmacsSwiftModule.Environment, baseDir: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                HyaloModule.startBuildWatcher(baseDir: baseDir)
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-set-module-reload",
            with: """
            Set the module reload callback for the activity viewer.
            When the user clicks the reload button after a module build,
            this function is called on the Emacs side.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-module-reload")
                HyaloModule.moduleReloadChannel = channel
                let reloadCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-rebuild-and-reload")
                }
                MainActor.assumeIsolated {
                    ActivityManager.shared.onModuleReload = reloadCallback
                }
                return true
            }
            return false
        }

        // MARK: - Async Build Process

        try env.defun("hyalo-async-build",
            with: """
            Run `swift build` asynchronously with full lifecycle tracking.
            BASE-DIR is the project root.  CONFIG is "debug" or "release".
            The activity viewer shows start, real-time build logs, and
            completion with a reload button.  No polling — uses Process
            pipes for stdout/stderr streaming.
            Returns t immediately (build runs in background).
            """
        ) { (env: EmacsSwiftModule.Environment, baseDir: String, config: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    HyaloModule.runAsyncBuild(baseDir: baseDir, config: config)
                }
                return true
            }
            return false
        }

        // MARK: - Build Channel

        try env.defun("hyalo-setup-build-channel",
            with: """
            Open the async channel that forwards build output to an Emacs
            compilation buffer.  Swift fires three callbacks into Emacs for
            each async build triggered by `hyalo-async-build':
              hyalo-channels--handle-build-start (config string)
              hyalo-channels--handle-build-log-line (one output line)
              hyalo-channels--handle-build-finish (success bool)
            The Emacs side populates *hyalo-build* in compilation-mode.
            Returns t on success.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-build")
                HyaloModule.buildChannel = channel

                let startCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, config: String) in
                    try env.funcall("hyalo-channels--handle-build-start", with: config)
                }
                let logLineCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, line: String) in
                    try env.funcall("hyalo-channels--handle-build-log-line", with: line)
                }
                let finishCallback: (Bool) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, success: Bool) in
                    try env.funcall("hyalo-channels--handle-build-finish", with: success)
                }

                HyaloModule.buildStartCallback = startCallback
                HyaloModule.buildLogLineCallback = logLineCallback
                HyaloModule.buildFinishCallback = finishCallback
                return true
            }
            return false
        }
    }

    // MARK: - Module Build File Watcher

    /// Single FSEventStream watching `.build/` for build activity.
    private static var buildWatcherStream: FSEventStreamRef?

    /// Dylib paths to monitor (debug and release).
    private static var watchedDylibPaths: [String] = []

    /// Last known modification times keyed by path (dylibs + build.db).
    private static var watchedModTimes: [String: Date] = [:]

    /// Path to `.build/build.db` — always updated on every build, even
    /// incremental no-ops where the dylib is not relinked.
    private static var buildDbPath: String = ""

    /// Whether an external build is currently detected as in-progress.
    private static var externalBuildInProgress = false

    /// Timer for stale build detection (used by internal async builds).
    private static var buildStaleTimer: DispatchSourceTimer?

    /// Project root directory (set by startBuildWatcher).
    static var baseDir: String = ""

    /// Convenience: path to whichever dylib was most recently written.
    static var watchedDylibPath: String = ""

    /// Convenience: last mod time of the most recently written dylib.
    static var lastDylibModTime: Date? {
        get { watchedModTimes.values.max() }
        set {
            if let nv = newValue {
                watchedModTimes[watchedDylibPath] = nv
            }
        }
    }

    /// Start watching `.build/` for swift build activity.
    @available(macOS 26.0, *)
    static func startBuildWatcher(baseDir: String) {
        stopBuildWatcher()
        self.baseDir = baseDir

        let buildDir = (baseDir as NSString).appendingPathComponent(".build")
        let debugDylib = (buildDir as NSString).appendingPathComponent("debug/libHyalo.dylib")
        let releaseDylib = (buildDir as NSString).appendingPathComponent("release/libHyalo.dylib")

        watchedDylibPaths = [debugDylib, releaseDylib]
        watchedDylibPath = debugDylib
        buildDbPath = (buildDir as NSString).appendingPathComponent("build.db")

        for path in watchedDylibPaths + [buildDbPath] {
            if let attrs = try? FileManager.default.attributesOfItem(atPath: path),
               let modTime = attrs[.modificationDate] as? Date {
                watchedModTimes[path] = modTime
            }
        }

        try? FileManager.default.createDirectory(
            atPath: buildDir, withIntermediateDirectories: true)

        var context = FSEventStreamContext()
        let paths = [buildDir as CFString] as CFArray

        guard let stream = FSEventStreamCreate(
            nil,
            { _, _, numEvents, eventPaths, _, _ in
                let cPaths = eventPaths.assumingMemoryBound(
                    to: UnsafePointer<CChar>.self)
                var paths: [String] = []
                paths.reserveCapacity(numEvents)
                for i in 0..<numEvents {
                    paths.append(String(cString: cPaths[i]))
                }
                HyaloModule.handleBuildDirectoryChange(eventPaths: paths)
            },
            &context,
            paths,
            FSEventStreamEventId(kFSEventStreamEventIdSinceNow),
            1.0,
            FSEventStreamCreateFlags(kFSEventStreamCreateFlagFileEvents)
        ) else {
            return
        }

        FSEventStreamSetDispatchQueue(stream, DispatchQueue.main)
        FSEventStreamStart(stream)
        buildWatcherStream = stream
    }

    /// Handle file system changes in `.build/`.
    @available(macOS 26.0, *)
    private static func handleBuildDirectoryChange(eventPaths: [String]) {
        var buildDbChanged = false
        if FileManager.default.fileExists(atPath: buildDbPath),
           let attrs = try? FileManager.default.attributesOfItem(atPath: buildDbPath),
           let modTime = attrs[.modificationDate] as? Date {
            let last = watchedModTimes[buildDbPath]
            if last == nil || modTime > last! {
                watchedModTimes[buildDbPath] = modTime
                buildDbChanged = true
            }
        }

        var dylibChanged = false
        for path in watchedDylibPaths {
            guard FileManager.default.fileExists(atPath: path) else { continue }
            guard let attrs = try? FileManager.default.attributesOfItem(atPath: path),
                  let modTime = attrs[.modificationDate] as? Date else { continue }
            let last = watchedModTimes[path]
            if let l = last, modTime <= l { continue }
            watchedModTimes[path] = modTime
            dylibChanged = true
            break
        }

        if buildDbChanged {
            cancelBuildStaleTimer()
            externalBuildInProgress = false
            Task { @MainActor in
                ActivityManager.shared.finishModuleBuild(
                    success: true, dylibChanged: dylibChanged)
            }
        }
    }

    private static func cancelBuildStaleTimer() {
        buildStaleTimer?.cancel()
        buildStaleTimer = nil
    }

    /// Stop the build watcher.
    static func stopBuildWatcher() {
        if let stream = buildWatcherStream {
            FSEventStreamStop(stream)
            FSEventStreamInvalidate(stream)
            FSEventStreamRelease(stream)
            buildWatcherStream = nil
        }
        cancelBuildStaleTimer()
        watchedDylibPaths.removeAll()
        watchedModTimes.removeAll()
        externalBuildInProgress = false
    }

    // MARK: - Async Build Process

    /// Currently running build process, if any.
    static var buildProcess: Process?

    /// Run `swift build` asynchronously with full lifecycle tracking.
    @available(macOS 26.0, *)
    @MainActor
    static func runAsyncBuild(baseDir: String, config: String) {
        if let existing = buildProcess, existing.isRunning {
            existing.terminate()
        }

        let mgr = ActivityManager.shared
        let activityID = ActivityManager.moduleBuildID

        Task { @MainActor in
            mgr.clearLog(id: activityID)
            mgr.startModuleBuild()
            HyaloModule.buildStartCallback?(config)
        }

        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/usr/bin/swift")
        process.arguments = ["build", "--product", "Hyalo", "-c", config]
        process.currentDirectoryURL = URL(fileURLWithPath: baseDir)

        let pipe = Pipe()
        process.standardOutput = pipe
        process.standardError = pipe

        // Wrap the line accumulation buffer in a reference type shared between
        // the readabilityHandler and terminationHandler closures. A serial queue
        // serializes access because readabilityHandler may still be executing
        // when terminationHandler fires (setting readabilityHandler = nil does
        // not guarantee the currently-executing handler has completed).
        final class LineAccumulator: @unchecked Sendable {
            var data = Data()
            let queue = DispatchQueue(label: "hyalo.build.lineaccumulator")
        }
        let acc = LineAccumulator()

        let handle = pipe.fileHandleForReading
        handle.readabilityHandler = { fh in
            let chunk = fh.availableData
            guard !chunk.isEmpty else { return }
            var lines: [String] = []
            acc.queue.sync {
                acc.data.append(chunk)
                while let range = acc.data.range(of: Data([0x0A])) {
                    let lineData = acc.data.subdata(in: acc.data.startIndex..<range.lowerBound)
                    acc.data.removeSubrange(acc.data.startIndex...range.lowerBound)
                    if let line = String(data: lineData, encoding: .utf8), !line.isEmpty {
                        lines.append(line)
                    }
                }
            }
            for line in lines {
                Task { @MainActor in
                    mgr.appendLog(id: activityID, line: line)
                    HyaloModule.buildLogLineCallback?(line)
                    let trimmed = line.trimmingCharacters(in: .whitespaces)
                    if trimmed.hasPrefix("[") || trimmed.hasPrefix("Build") || trimmed.hasPrefix("Compiling") ||
                       trimmed.hasPrefix("Linking") || trimmed.hasPrefix("error:") || trimmed.hasPrefix("warning:") {
                        mgr.updateModuleBuild(message: trimmed)
                    }
                }
            }
        }

        process.terminationHandler = { proc in
            handle.readabilityHandler = nil
            let remainingLine: String? = acc.queue.sync {
                if !acc.data.isEmpty, let line = String(data: acc.data, encoding: .utf8) {
                    return line
                }
                return nil
            }
            if let line = remainingLine {
                Task { @MainActor in
                    mgr.appendLog(id: activityID, line: line)
                    HyaloModule.buildLogLineCallback?(line)
                }
            }

            let success = proc.terminationStatus == 0
            Task { @MainActor in
                mgr.finishModuleBuild(success: success, dylibChanged: success)
                HyaloModule.buildFinishCallback?(success)
                if success {
                    let debugDylib = (HyaloModule.baseDir as NSString).appendingPathComponent(".build/debug/libHyalo.dylib")
                    let releaseDylib = (HyaloModule.baseDir as NSString).appendingPathComponent(".build/release/libHyalo.dylib")
                    let dylibPath = config == "release" ? releaseDylib : debugDylib
                    if let attrs = try? FileManager.default.attributesOfItem(atPath: dylibPath),
                       let modTime = attrs[.modificationDate] as? Date {
                        HyaloModule.watchedModTimes[dylibPath] = modTime
                    }
                }
                if let attrs = try? FileManager.default.attributesOfItem(atPath: HyaloModule.buildDbPath),
                   let modTime = attrs[.modificationDate] as? Date {
                    HyaloModule.watchedModTimes[HyaloModule.buildDbPath] = modTime
                }
                HyaloModule.externalBuildInProgress = false
            }

            HyaloModule.buildProcess = nil
        }

        do {
            try process.run()
            buildProcess = process
        } catch {
            Task { @MainActor in
                mgr.appendLog(id: activityID, line: "Failed to start: \(error.localizedDescription)")
                mgr.finishModuleBuild(success: false)
            }
        }
    }
}
