// ToolbarManager.swift - Shared toolbar state singleton
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Build status is managed by ActivityManager (ActivityModel.swift).
// ToolbarManager retains branch info, package management state, and callbacks.

import AppKit
import SwiftUI

// MARK: - Branch Info

@available(macOS 26.0, *)
struct BranchInfo: Codable {
    var currentBranch: String
    var branches: [String]
}

// MARK: - Toolbar View Model

@available(macOS 26.0, *)
@MainActor
@Observable
final class ToolbarViewModel {
    var projectName: String = ""
    var currentBranch: String = ""
    var branches: [String] = []
    var statusMessage: String = ""

    // Package management
    var packageOperation: PackageOperation = .idle
    var upgradablePackages: [UpgradablePackage] = []
    var vcPackages: [VCPackage] = []
    var lastChecked: Date? = nil

    // Keycast
    var keycastVisible: Bool = false
    var keycastKey: String = ""
    var keycastCommand: String = ""

    // Callbacks (set by Module.swift channel setup)
    var onBranchSwitch: ((String) -> Void)?
    var onPackageRefresh: (() -> Void)?
    var onPackageUpgradeAll: (() -> Void)?
    var onPackageUpgradeSingle: ((String) -> Void)?
    var onPackageList: (() -> Void)?
}

// MARK: - Toolbar Manager

@available(macOS 26.0, *)
@MainActor
final class ToolbarManager {
    static let shared = ToolbarManager()

    let viewModel = ToolbarViewModel()

    private init() {}

    func updateBranchInfo(from data: Data) {
        do {
            let info = try JSONDecoder().decode(BranchInfo.self, from: data)
            viewModel.currentBranch = info.currentBranch
            viewModel.branches = info.branches
        } catch {
            NSLog("[Hyalo] updateBranchInfo decode error: %@", String(describing: error))
        }
    }

    func updateStatusMessage(_ message: String) {
        viewModel.statusMessage = message
    }

    func updatePackageStatus(from data: Data) {
        do {
            let payload = try JSONDecoder().decode(PackageStatusPayload.self, from: data)
            if let op = PackageOperation(rawValue: payload.status) {
                viewModel.packageOperation = op
            }
            viewModel.upgradablePackages = payload.upgradable
            viewModel.vcPackages = payload.vcPackages
            if let ts = payload.lastChecked {
                let formatter = ISO8601DateFormatter()
                formatter.formatOptions = [
                    .withFullDate, .withTime,
                    .withDashSeparatorInDate, .withColonSeparatorInTime,
                    .withTimeZone
                ]
                viewModel.lastChecked = formatter.date(from: ts)
            }
        } catch {
            NSLog("[Hyalo] updatePackageStatus decode error: %@", String(describing: error))
        }
    }
}
