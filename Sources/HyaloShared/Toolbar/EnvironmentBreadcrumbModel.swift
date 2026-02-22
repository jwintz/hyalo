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

public struct UserHostInfo: Codable, Identifiable, Equatable {
    public var username: String
    public var hostname: String
    
    public var id: String { "\(username)@\(hostname)" }
    public var displayName: String { "\(username)@\(hostname)" }
}

public struct DevEnvironment: Codable, Identifiable, Equatable {
    public var type: String
    public var name: String
    public var icon: String
    public var isActive: Bool?
    public var path: String?
    
    public var id: String { type }
    
    public var displayIcon: String {
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

@available(macOS 26.0, iOS 26.0, *)
@MainActor
@Observable
public final class EnvironmentBreadcrumbModel {
    public static let shared = EnvironmentBreadcrumbModel()
    
    // Segment 1: User/Host
    public var userHost: UserHostInfo?
    
    // Segment 2: Environments
    public var environments: [DevEnvironment] = []
    
    public var primaryEnvironment: DevEnvironment? {
        environments.first(where: { $0.isActive == true }) ?? environments.first
    }
    
    public var activeEnvironments: [DevEnvironment] {
        environments.filter { $0.isActive == true }
    }
    
    public var inactiveEnvironments: [DevEnvironment] {
        environments.filter { $0.isActive != true }
    }
    
    // Callbacks wired by channel setup in Module.swift
    public var onEnvironmentSwitch: ((String) -> Void)?
    public var onOpenTerminal: (() -> Void)?
    public var onCopySSHCommand: (() -> Void)?
    public var onSSHHost: (() -> Void)?
    
    // MARK: - Updates from Emacs
    
    public func updateUserHost(from json: String) {
        guard let data = json.data(using: .utf8),
              let decoded = try? JSONDecoder().decode(UserHostInfo.self, from: data)
        else { return }
        userHost = decoded
    }
    
    public func updateEnvironments(from json: String) {
        guard let data = json.data(using: .utf8),
              let decoded = try? JSONDecoder().decode([DevEnvironment].self, from: data)
        else { return }
        environments = decoded
    }
    
    private init() {}
}
