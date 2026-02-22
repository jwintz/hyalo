// CommandPaletteViewModel.swift - View model for Command Palette
// Target: macOS 26 Tahoe with Liquid Glass design
// Ported from emacs.d

import Foundation
import SwiftUI

@available(macOS 26.0, iOS 26.0, *)
@Observable
public final class CommandPaletteViewModel {
    public init() { }
    public var searchText: String = ""
    public var selectedCommand: CommandItem?
    public var commands: [CommandItem] = []
    public var filteredCommands: [CommandItem] = []
    public var recentCommands: [CommandItem] = []

    public var onExecute: ((CommandItem) -> Void)?
    public var onClose: (() -> Void)?

    // MARK: - Data Updates

    public func updateCommands(_ newCommands: [CommandItem]) {
        commands = newCommands
        filterCommands()
    }

    public func updateCommands(from jsonData: Data) {
        do {
            let decoded = try JSONDecoder().decode([CommandItem].self, from: jsonData)
            updateCommands(decoded)
        } catch {
            NSLog("[Hyalo] Failed to decode commands: \(error)")
        }
    }

    // MARK: - Filtering

    public func filterCommands() {
        if searchText.isEmpty {
            filteredCommands = recentCommands + commands.filter { cmd in
                !recentCommands.contains(where: { $0.name == cmd.name })
            }
            selectedCommand = filteredCommands.first
        } else {
            filteredCommands = FuzzyMatcher.filter(query: searchText, items: commands)
            selectedCommand = filteredCommands.first
        }
    }

    // MARK: - Execution

    public func executeCommand(_ command: CommandItem) {
        if !recentCommands.contains(where: { $0.name == command.name }) {
            recentCommands.insert(command, at: 0)
            if recentCommands.count > 10 {
                recentCommands.removeLast()
            }
        }
        onExecute?(command)
    }

    public func executeCurrent() {
        if let cmd = selectedCommand {
            executeCommand(cmd)
        } else if let first = filteredCommands.first {
            executeCommand(first)
        }
    }

    // MARK: - Navigation

    public func selectNext() {
        guard let current = selectedCommand,
              let index = filteredCommands.firstIndex(where: { $0.id == current.id }),
              index < filteredCommands.count - 1 else {
            selectedCommand = filteredCommands.first
            return
        }
        selectedCommand = filteredCommands[index + 1]
    }

    public func selectPrevious() {
        guard let current = selectedCommand,
              let index = filteredCommands.firstIndex(where: { $0.id == current.id }),
              index > 0 else {
            return
        }
        selectedCommand = filteredCommands[index - 1]
    }
}
