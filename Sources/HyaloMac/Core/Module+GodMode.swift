// Module+GodMode.swift - God-mode toolbar bindings
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import EmacsSwiftModule
import HyaloShared

extension HyaloModule {

    func setupGodModeBindings(_ env: EmacsSwiftModule.Environment) throws {

        try env.defun("hyalo-update-god-mode",
            with: """
            Update the god-mode toolbar pill state.
            STATE is a string: \"inactive\", \"control\", \"literal\",
            \"meta\", \"control-meta\", \"digit-argument\", or \"universal-argument\".
            """
        ) { (env: EmacsSwiftModule.Environment, state: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    let vm = ToolbarManager.shared.viewModel
                    let nextState = GodModeState(rawValue: state) ?? .inactive
                    if vm.godModeState != nextState {
                        NSLog("[Hyalo] god-mode state <- %@", state)
                        vm.godModeState = nextState
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-god-mode-visible",
            with: "Set god-mode toolbar pill visibility. VISIBLE is t or nil."
        ) { (env: EmacsSwiftModule.Environment, visible: Bool) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    let vm = ToolbarManager.shared.viewModel
                    if vm.godModeVisible != visible {
                        NSLog("[Hyalo] god-mode visible <- %@", visible ? "true" : "false")
                        vm.godModeVisible = visible
                    }
                }
                return true
            }
            return false
        }
    }
}
