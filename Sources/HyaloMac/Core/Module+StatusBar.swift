// Module+StatusBar.swift - Status bar, toolbar, and activity system bindings
// Target: macOS 26 Tahoe

import AppKit
import HyaloShared
import EmacsSwiftModule
import SwiftUI

extension HyaloModule {

    func setupStatusBarBindings(_ env: EmacsSwiftModule.Environment) throws {

        // MARK: - Status Bar

        try env.defun("hyalo-status-update",
            with: """
            Update the status bar with current buffer information.
            Includes modeline segments and minor modes.
            """
        ) { (env: EmacsSwiftModule.Environment,
             line: Int, column: Int, mode: String,
             encoding: String?, lineEnding: String?,
             indentStyle: String?, indentWidth: Int?,
             fileType: String?, fileSize: String?,
             minorModesJson: String?,
             modelineLHS: String?,
             modelineRHS: String?) throws -> Bool in
            if #available(macOS 26.0, *) {
                var minorModes: [String]?
                if let json = minorModesJson, let data = json.data(using: .utf8) {
                    minorModes = try? JSONDecoder().decode([String].self, from: data)
                }
                Task { @MainActor in
                    StatusBarManager.shared.updateStatus(
                        line: line, column: column, mode: mode,
                        encoding: encoding, lineEnding: lineEnding,
                        indentStyle: indentStyle, indentWidth: indentWidth,
                        fileType: fileType, fileSize: fileSize,
                        minorModes: minorModes,
                        modelineLHS: modelineLHS,
                        modelineRHS: modelineRHS
                    )
                }
                return true
            }
            return false
        }

        // MARK: - Status Channel

        try env.defun("hyalo-setup-status-channel",
            with: "Setup the async channel for status bar callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-status")
                HyaloModule.statusChannel = channel

                let encodingCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, encoding: String) in
                    try env.funcall("hyalo-status--set-encoding", with: encoding)
                }

                let lineEndingCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, lineEnding: String) in
                    try env.funcall("hyalo-status--set-line-ending", with: lineEnding)
                }

                let indentStyleCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, indentJson: String) in
                    try env.funcall("hyalo-status--set-indent", with: indentJson)
                }

                Task { @MainActor in
                    let vm = StatusBarManager.shared.viewModel
                    vm.onEncodingChange = encodingCallback
                    vm.onLineEndingChange = lineEndingCallback
                    vm.onIndentStyleChange = { style, width in
                        indentStyleCallback("\(style):\(width)")
                    }
                }

                return true
            }
            return false
        }

        // MARK: - Toolbar Data

        try env.defun("hyalo-update-branch-info",
            with: "Update the toolbar branch picker from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    ToolbarManager.shared.updateBranchInfo(from: data)
                }
                return true
            }
            return false
        }

        // MARK: - Activity System

        try env.defun("hyalo-activity-upsert",
            with: """
            Create or update an activity in the toolbar activity viewer.
            ID is a unique string key for this activity.
            KIND is one of: \"native-compilation\", \"module-compilation\", \"package-installation\".
            TITLE is the primary text shown inline.
            MESSAGE is optional secondary text shown in the popover.
            PROGRESS is a float 0.0-1.0, or nil for indeterminate.
            IS-ACTIVE is t for in-progress, nil for finished.
            """
        ) { (env: EmacsSwiftModule.Environment, id: String, kind: String,
             title: String, message: String?, progress: Double?,
             isActive: Bool) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    guard let activityKind = ActivityKind(rawValue: kind) else { return }
                    ActivityManager.shared.upsert(
                        id: id, kind: activityKind, title: title,
                        message: message ?? "", progress: progress,
                        isActive: isActive
                    )
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-finish",
            with: """
            Mark an activity as finished.
            ID is the activity key.  MESSAGE is an optional completion message.
            """
        ) { (env: EmacsSwiftModule.Environment, id: String, message: String?) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.finish(id: id, message: message ?? "")
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-remove",
            with: "Remove an activity from the activity viewer."
        ) { (env: EmacsSwiftModule.Environment, id: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.remove(id: id)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-remove-after-delay",
            with: """
            Remove an activity after DELAY seconds.
            """
        ) { (env: EmacsSwiftModule.Environment, id: String, delay: Double) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.removeAfterDelay(id: id, delay: delay)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-append-log",
            with: """
            Append a log line to an activity's detail log.
            ID is the activity key.  LINE is the text to append.
            """
        ) { (env: EmacsSwiftModule.Environment, id: String, line: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.appendLog(id: id, line: line)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-activity-clear-log",
            with: "Clear the log lines for an activity."
        ) { (env: EmacsSwiftModule.Environment, id: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    ActivityManager.shared.clearLog(id: id)
                }
                return true
            }
            return false
        }

        // MARK: - Toolbar Channel

        try env.defun("hyalo-setup-toolbar-channel",
            with: "Setup the async channel for toolbar callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-toolbar")
                HyaloModule.toolbarChannel = channel

                let branchSwitchCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, branch: String) in
                    try env.funcall("hyalo-channels--handle-branch-switch", with: branch)
                }

                Task { @MainActor in
                    ToolbarManager.shared.viewModel.onBranchSwitch = branchSwitchCallback
                }

                return true
            }
            return false
        }

        // MARK: - Package Management

        try env.defun("hyalo-update-package-status",
            with: """
            Update the package management toolbar from JSON.
            JSON-DATA has keys: status ("idle"/"refreshing"/"upgrading"),
            upgradable (array of {name, installed, available}).
            """
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    ToolbarManager.shared.updatePackageStatus(from: data)
                }
                return true
            }
            return false
        }

        // MARK: - Package Channel

        try env.defun("hyalo-setup-package-channel",
            with: "Setup the async channel for package management callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-package")
                HyaloModule.packageChannel = channel

                let refreshCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-channels--handle-package-refresh")
                }

                let upgradeAllCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-channels--handle-package-upgrade-all")
                }

                let upgradeSingleCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, name: String) in
                    try env.funcall("hyalo-channels--handle-package-upgrade-single", with: name)
                }

                let listCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-channels--handle-package-list")
                }

                HyaloModule.packageRefreshCallback = refreshCallback
                HyaloModule.packageUpgradeAllCallback = upgradeAllCallback
                HyaloModule.packageUpgradeSingleCallback = upgradeSingleCallback
                HyaloModule.packageListCallback = listCallback

                Task { @MainActor in
                    let vm = ToolbarManager.shared.viewModel
                    vm.onPackageRefresh = refreshCallback
                    vm.onPackageUpgradeAll = upgradeAllCallback
                    vm.onPackageUpgradeSingle = upgradeSingleCallback
                    vm.onPackageList = listCallback
                }

                return true
            }
            return false
        }
    }
}
