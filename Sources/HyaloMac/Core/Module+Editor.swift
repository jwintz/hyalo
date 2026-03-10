// Module+Editor.swift - Editor tab bindings and channels
// Target: macOS 26 Tahoe

import AppKit
import HyaloShared
import EmacsSwiftModule
import SwiftUI

extension HyaloModule {

    func setupEditorBindings(_ env: EmacsSwiftModule.Environment) throws {

        // MARK: - Editor Tabs

        try env.defun("hyalo-update-editor-tabs",
            with: "Update the editor tab bar from JSON. Pushes to all frames."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                do {
                    let tabs = try JSONDecoder().decode([EditorTab].self, from: data)
                    MainActor.assumeIsolated {
                        for controller in HyaloModule.allControllers {
                            controller.editorTabViewModel.updateTabs(tabs)
                        }
                    }
                    return true
                } catch { return false }
            }
            return false
        }

        try env.defun("hyalo-select-editor-tab",
            with: "Select an editor tab by buffer name in the active frame. Called from Emacs when file opens."
        ) { (env: EmacsSwiftModule.Environment, bufferName: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    if let wc = HyaloModule.activeController {
                        wc.editorTabViewModel.onTabSelected(bufferName)
                    }
                }
                return true
            }
            return false
        }

        // MARK: - Editor Tab Channel

        try env.defun("hyalo-setup-editor-tab-channel",
            with: "Setup the async channel for editor tab callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-editor-tabs")
                HyaloModule.editorTabChannel = channel

                let tabSelectCallback: @MainActor (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, bufferName: String) in
                    try env.funcall("hyalo-channels--handle-switch-buffer", with: bufferName)
                }

                let tabCloseCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, bufferName: String) in
                    try env.funcall("hyalo-channels--handle-close-tab", with: bufferName)
                }

                let navigateBackCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("previous-buffer")
                    try env.funcall("hyalo-sync--push")
                }

                let navigateForwardCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("next-buffer")
                    try env.funcall("hyalo-sync--push")
                }

                HyaloModule.editorTabSelectCallback = tabSelectCallback
                HyaloModule.editorTabCloseCallback = tabCloseCallback
                HyaloModule.editorNavigateBackCallback = navigateBackCallback
                HyaloModule.editorNavigateForwardCallback = navigateForwardCallback

                MainActor.assumeIsolated {
                    for controller in HyaloModule.allControllers {
                        controller.editorTabViewModel.onTabSelect = tabSelectCallback
                        controller.editorTabViewModel.onTabClose = tabCloseCallback
                        controller.editorTabViewModel.onNavigateBack = navigateBackCallback
                        controller.editorTabViewModel.onNavigateForward = navigateForwardCallback
                    }
                }

                return true
            }
            return false
        }
    }
}
