// Module+Minibuffer.swift - Minibuffer bridge bindings
// Defuns and channel setup for the native Swift minibuffer panel.

import AppKit
import HyaloShared
import EmacsSwiftModule
import SwiftUI

extension HyaloModule {

    func setupMinibufferBindings(_ env: EmacsSwiftModule.Environment) throws {

        // MARK: - Minibuffer Show

        try env.defun("hyalo-minibuffer-show",
            with: """
            Show the native minibuffer panel.
            JSON-DATA contains sessionId, prompt, input, candidates, selectedIndex, totalCandidates.
            """
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    MinibufferManager.shared.show(from: data)
                }
                return true
            }
            return false
        }

        // MARK: - Minibuffer Update

        try env.defun("hyalo-minibuffer-update",
            with: """
            Update the native minibuffer panel with new candidates.
            JSON-DATA contains sessionId, prompt, input, candidates, selectedIndex, totalCandidates.
            """
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    MinibufferManager.shared.update(from: data)
                }
                return true
            }
            return false
        }

        // MARK: - Minibuffer Hide

        try env.defun("hyalo-minibuffer-hide",
            with: "Hide the native minibuffer panel."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    MinibufferManager.shared.hide()
                }
                return true
            }
            return false
        }

        // MARK: - Minibuffer Channel

        try env.defun("hyalo-setup-minibuffer-channel",
            with: "Setup the async channel for minibuffer callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-minibuffer")
                HyaloModule.minibufferChannel = channel

                let inputCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, text: String) in
                    try env.funcall("hyalo-channels--handle-minibuffer-input", with: text)
                }

                let selectCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, index: String) in
                    try env.funcall("hyalo-channels--handle-minibuffer-select", with: index)
                }

                let abortCallback: () -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment) in
                    try env.funcall("hyalo-channels--handle-minibuffer-abort")
                }

                MainActor.assumeIsolated {
                    let manager = MinibufferManager.shared
                    manager.onInputChanged = inputCallback
                    manager.onCandidateSelected = selectCallback
                    manager.onAbort = abortCallback
                }

                return true
            }
            return false
        }
    }
}
