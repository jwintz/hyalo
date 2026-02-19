// CommandPaletteManager.swift - Manager for command palette panels
// Target: macOS 26 Tahoe with Liquid Glass design
// Keyboard events handled via SearchPanel's NSEvent monitor.

import AppKit
import SwiftUI

// MARK: - Command Item

// AUDIT.md #9: Stable identity — use name as ID instead of UUID()
@available(macOS 26.0, *)
struct CommandItem: Identifiable, Hashable, Codable {
    var id: String { name }  // Stable: command names are unique
    var name: String
    var description: String
    var icon: String
    var keybinding: String?
    var category: String?
}

@available(macOS 26.0, *)
extension CommandItem: FuzzyMatchable {
    var fuzzyMatchText: String {
        name + " " + description
    }
}

// MARK: - Open Quickly Item

// AUDIT.md #9: Stable identity — use path as ID instead of UUID()
@available(macOS 26.0, *)
struct OpenQuicklyItem: Identifiable, Hashable, Codable {
    var id: String { path }  // Stable: file paths are unique
    var name: String
    var path: String
    var icon: String
    var relativePath: String?
}

@available(macOS 26.0, *)
extension OpenQuicklyItem: FuzzyMatchable {
    var fuzzyMatchText: String {
        name + " " + (relativePath ?? path)
    }
}

// MARK: - Command Palette Manager

@available(macOS 26.0, *)
final class CommandPaletteManager {
    static let shared = CommandPaletteManager()

    private var openQuicklyPanel: SearchPanel?
    private var commandPalettePanel: SearchPanel?

    let openQuicklyViewModel = OpenQuicklyViewModel()
    let commandPaletteViewModel = CommandPaletteViewModel()

    // Callbacks to Emacs
    var onOpenFile: ((String) -> Void)?
    var onExecuteCommand: ((String) -> Void)?

    private init() {}

    // MARK: - Open Quickly

    func showOpenQuickly() {
        if openQuicklyPanel != nil {
            if openQuicklyPanel!.isKeyWindow {
                closeOpenQuickly()
                return
            } else {
                openQuicklyPanel!.makeKeyAndOrderFront(nil)
                return
            }
        }

        guard let parentWindow = findParentWindow() else { return }

        openQuicklyViewModel.searchText = ""
        openQuicklyViewModel.filterItems()

        let panel = SearchPanel { [weak self] in
            self?.closeOpenQuickly()
        }

        // Wire up keyboard event callbacks to view model
        panel.onArrowUp = { [weak self] in
            self?.openQuicklyViewModel.selectPrevious()
        }
        panel.onArrowDown = { [weak self] in
            self?.openQuicklyViewModel.selectNext()
        }
        panel.onConfirm = { [weak self] in
            guard let self else { return }
            let vm = self.openQuicklyViewModel
            if let item = vm.selectedItem ?? vm.filteredItems.first {
                MainActor.assumeIsolated {
                    NavigatorManager.shared.setActiveFile(item.path)
                }
                self.onOpenFile?(item.path)
                vm.searchText = ""
                self.closeOpenQuickly()
            } else {
                NSSound.beep()
            }
        }

        openQuicklyPanel = panel

        let contentView = OpenQuicklyView(
            viewModel: openQuicklyViewModel,
            onClose: { [weak self] in
                self?.closeOpenQuickly()
            },
            openFile: { [weak self] item in
                // Update navigator selection immediately (before the Emacs
                // round-trip completes) so the tree highlights the file.
                MainActor.assumeIsolated {
                    NavigatorManager.shared.setActiveFile(item.path)
                }
                self?.onOpenFile?(item.path)
            }
        )

        panel.contentView = NSHostingView(rootView: contentView)
        panel.positionRelativeToParent(parentWindow)
        parentWindow.addChildWindow(panel, ordered: .above)
        panel.makeKeyAndOrderFront(nil)
    }

    func closeOpenQuickly() {
        guard let panel = openQuicklyPanel else { return }
        openQuicklyPanel = nil
        panel.close()
        restoreEmacsFirstResponder()
    }

    func updateOpenQuicklyItems(from data: Data) {
        openQuicklyViewModel.updateItems(from: data)
    }

    // MARK: - Command Palette

    func showCommandPalette() {
        if commandPalettePanel != nil {
            if commandPalettePanel!.isKeyWindow {
                closeCommandPalette()
                return
            } else {
                commandPalettePanel!.makeKeyAndOrderFront(nil)
                return
            }
        }

        guard let parentWindow = findParentWindow() else { return }

        commandPaletteViewModel.searchText = ""
        commandPaletteViewModel.filterCommands()

        let panel = SearchPanel { [weak self] in
            self?.closeCommandPalette()
        }

        // Wire up keyboard event callbacks to view model
        panel.onArrowUp = { [weak self] in
            self?.commandPaletteViewModel.selectPrevious()
        }
        panel.onArrowDown = { [weak self] in
            self?.commandPaletteViewModel.selectNext()
        }
        panel.onConfirm = { [weak self] in
            guard let self else { return }
            let vm = self.commandPaletteViewModel
            if let cmd = vm.selectedCommand ?? vm.filteredCommands.first {
                self.onExecuteCommand?(cmd.name)
                vm.searchText = ""
                self.closeCommandPalette()
            } else {
                NSSound.beep()
            }
        }

        commandPalettePanel = panel

        let contentView = CommandPaletteView(
            viewModel: commandPaletteViewModel,
            onClose: { [weak self] in
                self?.closeCommandPalette()
            },
            executeCommand: { [weak self] command in
                self?.onExecuteCommand?(command.name)
            }
        )

        panel.contentView = NSHostingView(rootView: contentView)
        panel.positionRelativeToParent(parentWindow)
        parentWindow.addChildWindow(panel, ordered: .above)
        panel.makeKeyAndOrderFront(nil)
    }

    func closeCommandPalette() {
        guard let panel = commandPalettePanel else { return }
        commandPalettePanel = nil
        panel.close()
        restoreEmacsFirstResponder()
    }

    func updateCommandList(from data: Data) {
        commandPaletteViewModel.updateCommands(from: data)
    }

    // MARK: - Window Discovery

    private func findParentWindow() -> NSWindow? {
        // Prefer the main Emacs window over any floating panel
        if let window = NSApp.mainWindow, !window.isMiniaturized { return window }
        if let window = NSApp.keyWindow, !window.isMiniaturized { return window }
        return NSApp.windows.first { $0.isVisible && !$0.isMiniaturized }
    }

    /// Restore Emacs first responder after closing a panel.
    /// Finds the EmacsView in the window hierarchy and makes it first responder.
    private func restoreEmacsFirstResponder() {
        guard let window = findParentWindow() else { return }
        DispatchQueue.main.async {
            window.makeKeyAndOrderFront(nil)
            func findEmacsView(in view: NSView) -> NSView? {
                let className = String(describing: type(of: view))
                if className.contains("EmacsView") { return view }
                for subview in view.subviews {
                    if let found = findEmacsView(in: subview) { return found }
                }
                return nil
            }
            if let contentView = window.contentView,
               let emacsView = findEmacsView(in: contentView) {
                window.makeFirstResponder(emacsView)
            }
        }
    }
}
