// BufferListViewModel.swift - Buffer list state with filtering
// Target: macOS 26 Tahoe with Liquid Glass design
// Split from NavigatorViewModel for granular updates (AUDIT.md #3)

import Foundation

@available(macOS 26.0, *)
@MainActor
@Observable
final class BufferListViewModel {
    // MARK: - State

    var buffers: [BufferInfo] = [] {
        didSet { updateFilteredBuffers() }
    }

    var filter: String = "" {
        didSet { updateFilteredBuffers() }
    }

    var selectedBuffer: String?
    var activeBuffer: String?

    // MARK: - Cached Output

    private(set) var filteredBuffers: [BufferInfo] = []

    // MARK: - Dependencies

    var onBufferSelect: ((String) -> Void)?
    var onBufferClose: ((String) -> Void)?
    var onBufferSave: ((String) -> Void)?

    // MARK: - Initialization

    init(buffers: [BufferInfo] = []) {
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

    func updateBuffers(_ newBuffers: [BufferInfo]) {
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

    func selectBuffer(_ name: String) {
        // Set activeBuffer IMMEDIATELY before sending to channel.
        // Prevents race where rapid clicks or stale callbacks revert selection.
        selectedBuffer = name
        activeBuffer = name
        onBufferSelect?(name)
    }

    func closeBuffer(_ name: String) {
        onBufferClose?(name)
        buffers.removeAll { $0.name == name }
        if selectedBuffer == name {
            selectedBuffer = nil
        }
    }

    func saveBuffer(_ name: String) {
        onBufferSave?(name)
    }

    // MARK: - Updates from Emacs (callbacks)

    func setActiveBuffer(_ name: String) {
        // Skip stale callbacks for buffers that are no longer active.
        // activeBuffer is set immediately in selectBuffer().
        if let active = activeBuffer, name != active {
            return
        }
        activeBuffer = name
    }
}
