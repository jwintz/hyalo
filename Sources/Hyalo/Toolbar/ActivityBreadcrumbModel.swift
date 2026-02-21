// ActivityBreadcrumbModel.swift - Observable model for the toolbar breadcrumb segments
// Target: macOS 26 Tahoe
//
// Two segments:
//   Segment 1 — Workspace: lists all decorated Emacs frames (projects).
//   Segment 2 — Activity:  lists all tab-bar tabs in the current frame.
//
// State is pushed from Emacs via hyalo-update-activities and
// hyalo-update-frame-list.  No polling.

import Foundation
import SwiftUI

// MARK: - Codable payloads (matching JSON from hyalo-activities.el)

struct ActivityTabItem: Codable, Identifiable, Equatable {
    var name: String
    var isCurrent: Bool

    // Identifiable by name (tab names are unique within a frame)
    var id: String { name }
}

struct FrameItem: Codable, Identifiable, Equatable {
    var id: Int
    var name: String
    var isCurrent: Bool
}

// MARK: - Observable Model

@available(macOS 26.0, *)
@MainActor
@Observable
final class ActivityBreadcrumbModel {
    static let shared = ActivityBreadcrumbModel()

    // Segment 1: frames (workspaces)
    var frames: [FrameItem] = []
    var currentFrame: FrameItem? { frames.first(where: \.isCurrent) }

    // Segment 2: activities (tab-bar tabs)
    var tabs: [ActivityTabItem] = []
    var currentTab: ActivityTabItem? { tabs.first(where: \.isCurrent) }

    // Callbacks wired by channel setup in Module.swift
    var onTabSwitch: ((String) -> Void)?
    var onTabNew: (() -> Void)?
    var onTabClose: ((String) -> Void)?
    var onTabRename: ((String, String) -> Void)?
    var onFrameSwitch: ((Int) -> Void)?

    // MARK: - Updates from Emacs

    func updateTabs(from json: String) {
        guard let data = json.data(using: .utf8),
              let decoded = try? JSONDecoder().decode([ActivityTabItem].self, from: data)
        else { return }
        tabs = decoded
    }

    func updateFrames(from json: String) {
        guard let data = json.data(using: .utf8),
              let decoded = try? JSONDecoder().decode([FrameItem].self, from: data)
        else { return }
        frames = decoded
    }

    private init() {}
}
