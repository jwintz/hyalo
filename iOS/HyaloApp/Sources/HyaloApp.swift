// HyaloApp.swift - Thin iOS entry point
// All lifecycle management is delegated to HyaloKit.

import SwiftUI
import HyaloKitFramework

@main
struct HyaloApp: App {
    var body: some Scene {
        WindowGroup {
            HyaloRootView()
        }
    }
}
