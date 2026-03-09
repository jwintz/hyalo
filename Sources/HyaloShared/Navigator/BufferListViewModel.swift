// BufferListViewModel.swift - Buffer list state with filtering
// Target: macOS 26 Tahoe with Liquid Glass design
// Split from NavigatorViewModel for granular updates (AUDIT.md #3)

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
public final class BufferListViewModel {
    // MARK: - State

    public var buffers: [BufferInfo] = [] {
        didSet { updateFilteredBuffers() }
    }

    public var filter: String = "" {
        didSet { updateFilteredBuffers() }
    }

    public var selectedBuffer: String?
    public var activeBuffer: String?

    // MARK: - Cached Output

    public private(set) var filteredBuffers: [BufferInfo] = []

    // MARK: - Dependencies

    public var onBufferSelect: ((String) -> Void)?
    public var onBufferClose: ((String) -> Void)?
    public var onBufferSave: ((String) -> Void)?

    // MARK: - Initialization

    public init(buffers: [BufferInfo] = []) {
        self.buffers = buffers
        updateFilteredBuffers()
    }

    // MARK: - Filtering (moved from view body per AUDIT.md #1)

    private func updateFilteredBuffers() {
        if filter.isEmpty {
            filteredBuffers = buffers
        } else {
            let query = filter.lowercased()
            filteredBuffers = buffers.filter {
                $0.name.lowercased().contains(query)
            }
        }
    }

    // MARK: - Updates from Emacs

    public func updateBuffers(_ newBuffers: [BufferInfo]) {
        let incoming = Dictionary(uniqueKeysWithValues: newBuffers.map { ($0.id, $0) })

        // Keep existing items in their current order, updating data
        var result = buffers.compactMap { existing -> BufferInfo? in
            incoming[existing.id]
        }

        // Append new items at the end
        let kept = Set(result.map(\.id))
        for buffer in newBuffers where !kept.contains(buffer.id) {
            result.append(buffer)
        }

        buffers = result
    }

    // MARK: - Actions to Emacs
    // Swift does NOT modify local state.  Emacs is the single source of truth.
    // The callback tells Emacs to switch; Emacs pushes the new state back
    // via hyalo-sync--push within ~20ms (requires wakeEmacs).

    public func selectBuffer(_ name: String) {
        onBufferSelect?(name)
    }

    public func closeBuffer(_ name: String) {
        onBufferClose?(name)
    }

    public func saveBuffer(_ name: String) {
        onBufferSave?(name)
    }

    // MARK: - Updates from Emacs (callbacks)

    /// Called from Emacs when the active buffer changes.
    /// No guard needed — window-buffer-change-functions fires AFTER the
    /// buffer switch is complete, so stale echoes cannot arrive.
    public func setActiveBuffer(_ name: String) {
        activeBuffer = name
        selectedBuffer = name
    }
}
