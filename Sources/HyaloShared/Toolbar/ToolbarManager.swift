// ToolbarManager.swift - Shared toolbar state singleton
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Build status is managed by ActivityManager (ActivityModel.swift).
// ToolbarManager retains branch info, package management state, and callbacks.

import SwiftUI

import AppKit

// MARK: - Branch Info

@available(macOS 26.0, *)
public struct BranchInfo: Codable {
    public var currentBranch: String
    public var branches: [String]
}

// MARK: - Toolbar View Model

@available(macOS 26.0, *)
@MainActor
@Observable
public final class ToolbarViewModel {
    public init() {}
    public var projectName: String = ""
    public var currentBranch: String = ""
    public var branches: [String] = []
    public var statusMessage: String = ""

    // Package management
    public var packageOperation: PackageOperation = .idle
    public var upgradablePackages: [UpgradablePackage] = []
    public var vcPackages: [VCPackage] = []
    public var lastChecked: Date? = nil

    // Keycast
    public var keycastVisible: Bool = false
    public var keycastKey: String = ""
    public var keycastCommand: String = ""

    // Callbacks (set by Module.swift channel setup)
    public var onBranchSwitch: ((String) -> Void)?
    public var onPackageRefresh: (() -> Void)?
    public var onPackageUpgradeAll: (() -> Void)?
    public var onPackageUpgradeSingle: ((String) -> Void)?
    public var onPackageList: (() -> Void)?
}

// MARK: - Toolbar Manager

@available(macOS 26.0, *)
@MainActor
public final class ToolbarManager {
    public static let shared = ToolbarManager()

    public let viewModel = ToolbarViewModel()

    private init() {}

    public func updateBranchInfo(from data: Data) {
        do {
            let info = try JSONDecoder().decode(BranchInfo.self, from: data)
            viewModel.currentBranch = info.currentBranch
            viewModel.branches = info.branches
        } catch {
            platformLog("[Hyalo] updateBranchInfo decode error: \(error)")
        }
    }

    public func updateStatusMessage(_ message: String) {
        viewModel.statusMessage = message
    }

    public func updatePackageStatus(from data: Data) {
        do {
            let payload = try JSONDecoder().decode(PackageStatusPayload.self, from: data)
            if let op = PackageOperation(rawValue: payload.status) {
                viewModel.packageOperation = op
            }
            viewModel.upgradablePackages = payload.upgradable
            viewModel.vcPackages = payload.vcPackages
            if let ts = payload.lastChecked {
                viewModel.lastChecked = DateFormatting.parseISO8601Full(ts)
            }
        } catch {
            platformLog("[Hyalo] updatePackageStatus decode error: \(error)")
        }
    }
}
