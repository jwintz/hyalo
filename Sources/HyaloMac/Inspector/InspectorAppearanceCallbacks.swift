import AppKit
import HyaloShared

@available(macOS 26.0, *)
enum InspectorAppearanceCallbackWirer {
    @MainActor
    static func wire() {
        InspectorAppearanceCallbacks.onAppearanceModeChanged = { mode, nsAppearance in
            for controller in HyaloModule.allControllers {
                controller.applyAppearance(nsAppearance)
            }
            for ws in HyaloModule.allWorkspaces {
                ws.windowAppearance = mode
                // Update the color theme's isDark flag to match the resolved
                // appearance. platformIsDarkMode() reads NSApp.effectiveAppearance
                // which reflects the *system* mode, not the per-window override.
                // When the user explicitly picks light/dark, we must set isDark
                // directly so all @Observable SwiftUI views re-render.
                switch mode {
                case "light": ws.colorTheme.isDark = false
                case "dark":  ws.colorTheme.isDark = true
                default:      ws.colorTheme.refreshAppearance()
                }
            }
            HyaloModule.onAppearanceModeChanged?(mode)
            HyaloModule.wakeEmacs()
        }

        InspectorAppearanceCallbacks.onOpacityChanged = { alpha in
            for ws in HyaloModule.allWorkspaces {
                ws.backgroundAlpha = alpha
            }
            let fringeAlpha = min(alpha + HyaloModule.fringeAlphaOffset, 1.0)
            HyaloModule.setFringeAlpha(fringeAlpha)
            HyaloModule.onOpacityChanged?(alpha)
        }

        InspectorAppearanceCallbacks.onMaterialChanged = { material in
            for ws in HyaloModule.allWorkspaces {
                ws.vibrancyMaterial = material
            }
        }
    }
}
