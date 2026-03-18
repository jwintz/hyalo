// Module+Appearance.swift - Appearance, theme, and terminal palette bindings
// Target: macOS 26 Tahoe

import AppKit
import HyaloShared
import KelyphosKit
import EmacsSwiftModule
import SwiftUI

extension HyaloModule {

    func setupAppearanceBindings(_ env: EmacsSwiftModule.Environment) throws {

        // MARK: - Appearance

        try env.defun("hyalo-set-vibrancy-material",
            with: "Set the vibrancy material level for the main content area."
        ) { (env: EmacsSwiftModule.Environment, material: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                Task { @MainActor in
                    guard let mat = VibrancyMaterial(rawValue: material) else { return }
                    for state in HyaloModule.allShellStates {
                        state.vibrancyMaterial = mat
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-background-color",
            with: "Set the workspace background color. Alpha is preserved from UserDefaults."
        ) { (env: EmacsSwiftModule.Environment, hexColor: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                Task { @MainActor in
                    guard let color = NSColor(hexString: hexColor) else { return }
                    for state in HyaloModule.allShellStates {
                        state.backgroundColor = color
                    }
                    for controller in HyaloModule.allControllers {
                        controller.updateLayerBackgrounds()
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-workspace-appearance",
            with: """
            Apply NSWindow appearance (light/dark chrome) without
            changing the mode selector preference.  The picker value
            is only changed by explicit user interaction.
            """
        ) { (env: EmacsSwiftModule.Environment, appearance: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                Task { @MainActor in
                    let nsAppearance: NSAppearance?
                    switch appearance {
                    case "light": nsAppearance = NSAppearance(named: .aqua)
                    case "dark": nsAppearance = NSAppearance(named: .darkAqua)
                    default: nsAppearance = nil
                    }
                    for controller in HyaloModule.allControllers {
                        controller.applyAppearance(nsAppearance)
                    }
                }
                return true
            }
            return false
        }

        // MARK: - Appearance Panel

        try env.defun("hyalo-show-appearance-panel",
            with: "Show the appearance settings in the inspector (tab 3)."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                Task { @MainActor in
                    if let wc = HyaloModule.activeController {
                        wc.selectInspectorTab(3)
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-current-theme-name",
            with: "Set the current theme name displayed in the appearance panel."
        ) { (env: EmacsSwiftModule.Environment, name: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                Task { @MainActor in
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.currentThemeName = name
                    }
                }
                return true
            }
            return false
        }

        try env.defun("hyalo-set-color-theme",
            with: """
            Set the color theme for a variant (\"light\" or \"dark\").
            VARIANT is \"light\" or \"dark\".
            JSON-COLORS is a JSON object with keys: background, backgroundDim,
            foreground, foregroundDim, accent, accentSecondary, error, warning,
            success, link, string, comment, constant, border, selection.
            """
        ) { (env: EmacsSwiftModule.Environment, variant: String, jsonColors: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                Task { @MainActor in
                    guard let data = jsonColors.data(using: .utf8),
                          let dict = try? JSONSerialization.jsonObject(with: data) as? [String: String]
                    else { return }
                    for workspace in HyaloModule.allWorkspaces {
                        workspace.colorTheme.update(variant: variant, from: dict)
                    }
                }
                return true
            }
            return false
        }

        // MARK: - Terminal Palette

        try env.defun("hyalo-set-terminal-palette",
            with: """
            Set the terminal ANSI palette from JSON.
            JSON is a JSON object with keys: foreground, background, cursor,
            ansi (array of 16 hex color strings).
            Updates the current appearance's color scheme (light or dark).
            """
        ) { (env: EmacsSwiftModule.Environment, json: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                Task { @MainActor in
                    guard let data = json.data(using: .utf8),
                          let dict = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
                    else { return }
                    let palette = TerminalPalette.shared
                    let fg = dict["foreground"] as? String
                    let bg = dict["background"] as? String
                    let cursor = dict["cursor"] as? String
                    let ansi = dict["ansi"] as? [String]
                    palette.updateCurrentScheme(
                        foreground: fg,
                        background: bg,
                        cursor: cursor,
                        ansiColors: ansi?.count == 16 ? ansi : nil
                    )
                }
                return true
            }
            return false
        }

        // MARK: - Appearance Channel

        try env.defun("hyalo-setup-appearance-channel",
            with: "Setup the async channel for appearance mode and opacity callbacks."
        ) { (env: EmacsSwiftModule.Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                let channel = try env.openChannel(name: "hyalo-appearance")

                let modeChangedCallback: (String) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, mode: String) in
                    try env.funcall("hyalo-channels--handle-appearance-mode", with: mode)
                }

                let opacityChangedCallback: (Double) -> Void = channel.callback {
                    (env: EmacsSwiftModule.Environment, opacity: Double) in
                    try env.funcall("hyalo-channels--handle-opacity-change", with: opacity)
                }

                HyaloModule.appearanceChannel = channel
                HyaloModule.onAppearanceModeChanged = modeChangedCallback
                HyaloModule.onOpacityChanged = opacityChangedCallback

                MainActor.assumeIsolated {
                    if let shellState = HyaloModule.activeShellState {
                        let opacity = Double(shellState.backgroundAlpha)
                        opacityChangedCallback(opacity)
                        let fringeAlpha = min(opacity + HyaloModule.fringeAlphaOffset, 1.0)
                        HyaloModule.setFringeAlpha(fringeAlpha)
                    }
                }

                return true
            }
            return false
        }
    }
}
