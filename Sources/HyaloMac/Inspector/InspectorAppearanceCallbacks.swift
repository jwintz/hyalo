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
