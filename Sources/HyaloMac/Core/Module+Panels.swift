// Module+Panels.swift - Inspector, utility area, and diagnostics bindings
// Target: macOS 26 Tahoe

import AppKit
import HyaloShared
import KelyphosKit
import EmacsSwiftModule
import SwiftUI

extension HyaloModule {

    func setupPanelBindings(_ env: EmacsSwiftModule.Environment) throws {

        // MARK: - Inspector Panel

        try env.defun("hyalo-inspector-toggle",
            with: "Toggle the inspector (right sidebar) visibility."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.toggleInspector()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-inspector-show",
            with: "Show the inspector (right sidebar)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.setInspectorVisible(true)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-inspector-hide",
            with: "Hide the inspector (right sidebar)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.setInspectorVisible(false)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-inspector-visible-p",
            with: "Return t if inspector is visible, nil otherwise."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let controller = HyaloModule.activeController else { return false }
                return controller.isInspectorVisible
            }
            return false
        }

        // MARK: - Inspector Tab Selection

        try env.defun("hyalo-inspector-select-tab",
            with: """
            Select inspector tab by 1-based INDEX.
            If already on that tab and inspector is visible, toggle inspector off.
            This is the Xcode Cmd-Option-1/2/3 behavior.
            """
        ) { (env: EmacsSwiftModule.Environment, index: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.selectInspectorTab(index)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-inspector-tab-index",
            with: "Return the current inspector tab index (1-based), or 0 if none."
        ) { (env: EmacsSwiftModule.Environment) throws -> Int in
            if #available(macOS 26.0, *) {
                guard let controller = HyaloModule.activeController else { return 0 }
                return controller.inspectorTabIndex
            }
            return 0
        }

        // MARK: - Inspector Data

        try env.defun("hyalo-update-file-info",
            with: "Update the inspector file info panel from JSON."
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    InspectorManager.shared.updateFileInfo(from: data)
                }
                return true
            }
            return false
        }

        // MARK: - Utility Area

        try env.defun("hyalo-utility-area-toggle",
            with: """
            Toggle the utility area (bottom panel) visibility.
            When shown, the terminal gains keyboard focus.
            When hidden, the Emacs view regains focus.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    guard let controller = HyaloModule.activeController else { return }
                    let shellState = controller.shellState
                    let willShow = !shellState.utilityAreaVisible
                    withAnimation(.easeInOut(duration: 0.15)) {
                        shellState.utilityAreaVisible = willShow
                    }
                    if willShow {
                        DispatchQueue.main.async { controller.focusTerminal() }
                    } else {
                        controller.focusEmacs()
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-utility-area-show",
            with: """
            Show the utility area (bottom panel).
            The terminal gains keyboard focus.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    guard let controller = HyaloModule.activeController else { return }
                    withAnimation(.easeInOut(duration: 0.15)) {
                        controller.shellState.utilityAreaVisible = true
                    }
                    DispatchQueue.main.async { controller.focusTerminal() }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-utility-area-hide",
            with: """
            Hide the utility area (bottom panel).
            The Emacs view regains keyboard focus.
            """
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                MainActor.assumeIsolated {
                    guard let controller = HyaloModule.activeController else { return }
                    withAnimation(.easeInOut(duration: 0.15)) {
                        controller.shellState.utilityAreaVisible = false
                    }
                    controller.focusEmacs()
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-utility-area-select-tab",
            with: """
            Select utility area tab by 1-based INDEX.
            If already on that tab and utility area is visible, toggle it off.
            This is the Cmd-Option-Shift-1/2 behavior.
            """
        ) { (env: EmacsSwiftModule.Environment, index: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.selectUtilityAreaTab(index)
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-utility-area-show-tab",
            with: """
            Show utility area and select tab by 1-based INDEX.
            Never hides — always shows the utility area and selects the tab.
            """
        ) { (env: EmacsSwiftModule.Environment, index: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let controller = HyaloModule.activeController else { return }
                    controller.showUtilityAreaTab(index)
                }
                return true
            }
            return false
        }

        // MARK: - Diagnostics

        try env.defun("hyalo-update-diagnostics",
            with: """
            Update the diagnostics panel from JSON.
            JSON-DATA is a JSON array of diagnostic objects with keys:
            id, file, line, column, severity, message, source.
            """
        ) { (env: EmacsSwiftModule.Environment, jsonData: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let data = jsonData.data(using: .utf8) else { return false }
                MainActor.assumeIsolated {
                    if let wc = HyaloModule.activeController {
                        wc.utilityAreaViewModel.diagnosticsViewModel.update(from: data)
                    }
                }
                return true
            }
            return false
        }

        // MARK: - Diagnostics Channel

        try env.defun("hyalo-setup-diagnostics-channel",
            with: "Setup the async channel for diagnostics navigation callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-diagnostics")
                HyaloModule.diagnosticsChannel = channel

                let navigateCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, location: String) in
                    try env.funcall("hyalo-channels--handle-diagnostic-navigate", with: location)
                }

                HyaloModule.diagnosticsNavigateCallback = navigateCallback

                MainActor.assumeIsolated {
                    for controller in HyaloModule.allControllers {
                        controller.utilityAreaViewModel.diagnosticsViewModel.onNavigate = { file, line, col in
                            navigateCallback("\(file):\(line):\(col)")
                        }
                    }
                }

                return true
            }
            return false
        }
    }
}
