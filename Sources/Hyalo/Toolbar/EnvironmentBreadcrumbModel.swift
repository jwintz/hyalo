// EnvironmentBreadcrumbModel.swift - Observable model for environment pill
// Target: macOS 26 Tahoe
//
// Two segments:
//   Segment 1 — User/Host: shows user@hostname
//   Segment 2 — Environment: shows detected dev environments (pixi, npm, etc.)
//
// State is pushed from Emacs via hyalo-environment channel. No polling.

import Foundation
import SwiftUI

// MARK: - Codable payloads (matching JSON from hyalo-environment.el)

struct UserHostInfo: Codable, Identifiable, Equatable {
    var username: String
    var hostname: String
    
    var id: String { "\(username)@\(hostname)" }
    var displayName: String { "\(username)@\(hostname)" }
}

struct DevEnvironment: Codable, Identifiable, Equatable {
    var type: String
    var name: String
    var icon: String
    var isActive: Bool?
    var path: String?
    
    var id: String { type }
    
    var displayIcon: String {
        // Map to SF Symbols, fallback to circle.fill
        switch type {
        case "pixi": return "puzzlepiece.extension.fill"
        case "conda": return "c.circle.fill"
        case "npm": return "n.circle.fill"
        case "bun": return "b.circle.fill"
        case "swift": return "swift"
        case "rust": return "r.circle.fill"
        case "python": return "play.circle.fill"
        case "docker": return "shippingbox"
        default: return "circle.fill"
        }
    }
}

// MARK: - Observable Model

@available(macOS 26.0, *)
@MainActor
@Observable
final class EnvironmentBreadcrumbModel {
    static let shared = EnvironmentBreadcrumbModel()
    
    // Segment 1: User/Host
    var userHost: UserHostInfo?
    
    // Segment 2: Environments
    var environments: [DevEnvironment] = []
    
    var primaryEnvironment: DevEnvironment? {
        environments.first(where: { $0.isActive == true }) ?? environments.first
    }
    
    var activeEnvironments: [DevEnvironment] {
        environments.filter { $0.isActive == true }
    }
    
    var inactiveEnvironments: [DevEnvironment] {
        environments.filter { $0.isActive != true }
    }
    
    // Callbacks wired by channel setup in Module.swift
    var onEnvironmentSwitch: ((String) -> Void)?
    var onOpenTerminal: (() -> Void)?
    var onCopySSHCommand: (() -> Void)?
    
    // MARK: - Updates from Emacs
    
    func updateUserHost(from json: String) {
        guard let data = json.data(using: .utf8),
              let decoded = try? JSONDecoder().decode(UserHostInfo.self, from: data)
        else { return }
        userHost = decoded
    }
    
    func updateEnvironments(from json: String) {
        guard let data = json.data(using: .utf8),
              let decoded = try? JSONDecoder().decode([DevEnvironment].self, from: data)
        else { return }
        environments = decoded
    }
    
    private init() {}
}
