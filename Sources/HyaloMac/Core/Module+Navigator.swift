// Module+Navigator.swift - Navigator panel, data, channels, and search bindings
// Target: macOS 26 Tahoe

import AppKit
import HyaloShared
import EmacsSwiftModule
import SwiftUI

extension HyaloModule {

    func setupNavigatorBindings(_ env: EmacsSwiftModule.Environment) throws {

        // MARK: - Navigator Panel

        try env.defun("hyalo-navigator-toggle",
            with: "Toggle the navigator (left sidebar) visibility."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.toggleNavigator()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-show",
            with: "Show the navigator (left sidebar)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.setNavigatorVisible(true)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-hide",
            with: "Hide the navigator (left sidebar)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.setNavigatorVisible(false)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-visible-p",
            with: "Return t if navigator is visible, nil otherwise."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let controller = HyaloModule.activeController else { return false }
                return controller.isNavigatorVisible
            }
            return false
        }

        // MARK: - Navigator Tab Selection

        try env.defun("hyalo-navigator-select-tab",
            with: """
            Select navigator tab by 1-based INDEX.
            If already on that tab and navigator is visible, toggle navigator off.
            This is the Xcode Cmd-1/2/3/4 behavior.
            """
        ) { (env: EmacsSwiftModule.Environment, index: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.selectNavigatorTab(index)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-tab-index",
            with: "Return the current navigator tab index (1-based), or 0 if none."
        ) { (env: EmacsSwiftModule.Environment) throws -> Int in
            if #available(macOS 26.0, *) {
                guard let controller = HyaloModule.activeController else { return 0 }
                return controller.navigatorTabIndex
            }
            return 0
        }

        // MARK: - Navigator Data

        try env.defun("hyalo-navigator-update-buffers",
            with: "Update the navigator buffer list from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                do {
                    let buffers = try JSONDecoder().decode([BufferInfo].self, from: data)
                    DispatchQueue.main.async {
                        NavigatorManager.shared.updateBufferList(buffers)
                    }
                    return true
                } catch { return false }
            }
            return false
        }

        try env.defun("hyalo-navigator-update-file-tree",
            with: "Update the navigator file tree from JSON. (Legacy — prefer hyalo-navigator-set-project-root)"
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-set-project-root",
            with: "Set the project root directory. Rebuilds file tree in Swift."
        ) { (env: EmacsSwiftModule.Environment, rootPath: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.setProjectRoot(rootPath)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-refresh-file-tree",
            with: "Refresh the file tree from the current project root."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.refreshFileTree()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-set-active-buffer",
            with: "Set the currently active buffer in the navigator."
        ) { (env: EmacsSwiftModule.Environment, bufferName: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.setActiveBuffer(bufferName)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-navigator-set-active-file",
            with: "Set the currently active file in the navigator."
        ) { (env: EmacsSwiftModule.Environment, filePath: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.setActiveFile(filePath)
                }
                return true
            }
            return false
        }

        // MARK: - Navigator Channel

        try env.defun("hyalo-setup-navigator-channel",
            with: "Setup the async channel for navigator callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-navigator")
                HyaloModule.navigatorChannel = channel

                let bufferSelectCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, bufferName: String) in
                    try env.funcall("hyalo-channels--handle-switch-buffer", with: bufferName)
                }

                let bufferCloseCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, bufferName: String) in
                    try env.funcall("kill-buffer", with: bufferName)
                }

                let fileSelectCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, filePath: String) in
                    try env.funcall("hyalo-channels--handle-find-file", with: filePath)
                }

                DispatchQueue.main.async {
                    NavigatorManager.shared.onBufferSelect = bufferSelectCallback
                    NavigatorManager.shared.onBufferClose = bufferCloseCallback
                    NavigatorManager.shared.onFileSelect = fileSelectCallback
                }

                return true
            }
            return false
        }

        // MARK: - Search

        try env.defun("hyalo-update-search-results",
            with: "Update search results in the navigator from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else {
                    return false
                }
                do {
                    let results = try JSONDecoder().decode([SearchResult].self, from: data)
                    DispatchQueue.main.async {
                        NavigatorManager.shared.updateSearchResults(results)
                    }
                    return true
                } catch {
                    NSLog("[Hyalo:Search] decode error: \(error)")
                    return false
                }
            }
            return false
        }

        try env.defun("hyalo-setup-search-channel",
            with: "Setup the async channel for search callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-search")

                let searchCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, query: String) in
                    try env.funcall("hyalo-channels--handle-search", with: query)
                }

                let navigateCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, location: String) in
                    try env.funcall("hyalo-channels--handle-search-navigate", with: location)
                }

                HyaloModule.onSearchExecute = searchCallback
                HyaloModule.onSearchNavigate = { file, line, col in
                    navigateCallback("\(file):\(line):\(col)")
                }

                DispatchQueue.main.async {
                    NavigatorManager.shared.onSearchExecute = searchCallback
                    NavigatorManager.shared.onSearchResultSelect = { file, line, col in
                        navigateCallback("\(file):\(line):\(col)")
                    }
                }

                return true
            }
            return false
        }

        try env.defun("hyalo-update-search-status",
            with: "Update search status counts in the find navigator."
        ) { (env: EmacsSwiftModule.Environment, resultCount: Int, fileCount: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    NavigatorManager.shared.updateSearchStatus(resultCount: resultCount, fileCount: fileCount)
                }
                return true
            }
            return false
        }
    }
}
