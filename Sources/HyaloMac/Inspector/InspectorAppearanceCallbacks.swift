import AppKit
import HyaloShared
import KelyphosKit

@available(macOS 26.0, *)
enum InspectorAppearanceCallbackWirer {
    @MainActor
    static func wire() {
        InspectorAppearanceCallbacks.onAppearanceModeChanged = { mode in
            for controller in HyaloModule.allControllers {
                let nsAppearance: NSAppearance?
                switch mode {
                case "light": nsAppearance = NSAppearance(named: .aqua)
                case "dark":  nsAppearance = NSAppearance(named: .darkAqua)
                default:      nsAppearance = nil
                }
                controller.applyAppearance(nsAppearance)
            }
            for ws in HyaloModule.allWorkspaces {
                switch mode {
                case "light":
                    ws.colorTheme.isDark = false
                    TerminalPalette.shared.setAppearance(isDark: false)
                case "dark":
                    ws.colorTheme.isDark = true
                    TerminalPalette.shared.setAppearance(isDark: true)
                default:
                    ws.colorTheme.refreshAppearance()
                    TerminalPalette.shared.refreshAppearance()
                }
            }
            HyaloModule.onAppearanceModeChanged?(mode)
            HyaloModule.wakeEmacs()
        }

        InspectorAppearanceCallbacks.onOpacityChanged = { alpha in
            for state in HyaloModule.allShellStates {
                state.backgroundAlpha = alpha
            }
            let fringeAlpha = min(alpha + HyaloModule.fringeAlphaOffset, 1.0)
            HyaloModule.setFringeAlpha(fringeAlpha)
            HyaloModule.onOpacityChanged?(alpha)
        }

        InspectorAppearanceCallbacks.onMaterialChanged = { material in
            for state in HyaloModule.allShellStates {
                state.vibrancyMaterial = material
            }
        }
    }
}
