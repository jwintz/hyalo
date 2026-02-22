// ChannelBridge.swift - Emacs↔Swift Communication Bridge for iOS
// Implements channel communication without module-load

import Foundation
import SwiftUI

/// Channel bridge for iOS - manages bidirectional communication between Emacs and Swift
@objc class ChannelBridge: NSObject {
    static let shared = ChannelBridge()
    
    // Channel handlers
    var navigatorHandler: NavigatorChannelHandler?
    var editorHandler: EditorChannelHandler?
    var statusHandler: StatusChannelHandler?
    var appearanceHandler: AppearanceChannelHandler?
    
    // Callback storage for Lisp→Swift calls
    var callbacks: [String: (Any?) -> Void] = [:]
    
    override private init() {
        super.init()
        setupChannels()
    }
    
    func setupChannels() {
        navigatorHandler = NavigatorChannelHandler()
        editorHandler = EditorChannelHandler()
        statusHandler = StatusChannelHandler()
        appearanceHandler = AppearanceChannelHandler()
    }
    
    // MARK: - Channel Registration (called from Emacs)
    
    @_cdecl("hyalo_ios_setup_channels")
    static func setupChannelsFromEmacs() {
        DispatchQueue.main.async {
            ChannelBridge.shared.setupChannels()
            print("✅ iOS channels setup complete")
        }
    }
    
    // MARK: - Navigator Channel
    
    @_cdecl("hyalo_ios_navigator_update_buffers")
    static func navigatorUpdateBuffers(_ jsonCString: UnsafePointer<CChar>) {
        let json = String(cString: jsonCString)
        DispatchQueue.main.async {
            ChannelBridge.shared.navigatorHandler?.updateBuffers(json)
        }
    }
    
    @_cdecl("hyalo_ios_navigator_set_active_buffer")
    static func navigatorSetActiveBuffer(_ bufferName: UnsafePointer<CChar>) {
        let name = String(cString: bufferName)
        DispatchQueue.main.async {
            ChannelBridge.shared.navigatorHandler?.setActiveBuffer(name)
        }
    }
    
    @_cdecl("hyalo_ios_navigator_set_active_file")
    static func navigatorSetActiveFile(_ filePath: UnsafePointer<CChar>) {
        let path = String(cString: filePath)
        DispatchQueue.main.async {
            ChannelBridge.shared.navigatorHandler?.setActiveFile(path)
        }
    }
    
    // MARK: - Editor Channel
    
    @_cdecl("hyalo_ios_editor_update_tabs")
    static func editorUpdateTabs(_ jsonCString: UnsafePointer<CChar>) {
        let json = String(cString: jsonCString)
        DispatchQueue.main.async {
            ChannelBridge.shared.editorHandler?.updateTabs(json)
        }
    }
    
    @_cdecl("hyalo_ios_editor_select_tab")
    static func editorSelectTab(_ bufferName: UnsafePointer<CChar>) {
        let name = String(cString: bufferName)
        DispatchQueue.main.async {
            ChannelBridge.shared.editorHandler?.selectTab(name)
        }
    }
    
    // MARK: - Status Channel
    
    @_cdecl("hyalo_ios_status_update")
    static func statusUpdate(_ jsonCString: UnsafePointer<CChar>) {
        let json = String(cString: jsonCString)
        DispatchQueue.main.async {
            ChannelBridge.shared.statusHandler?.updateStatus(json)
        }
    }
    
    // MARK: - Appearance Channel
    
    @_cdecl("hyalo_ios_appearance_set_mode")
    static func appearanceSetMode(_ modeCString: UnsafePointer<CChar>) {
        let mode = String(cString: modeCString)
        DispatchQueue.main.async {
            ChannelBridge.shared.appearanceHandler?.setMode(mode)
        }
    }
    
    // MARK: - Swift→Emacs calls (via C FFI)
    
    /// Call Emacs Lisp function from Swift
    func callEmacsFunction(_ function: String, _ args: [Any]) {
        // This will be implemented via C FFI callback
        // Emacs exposes functions that Swift can call
        print("📞 Calling Emacs: \(function)")
    }
}

// MARK: - Channel Handlers

class NavigatorChannelHandler {
    func updateBuffers(_ json: String) {
        print("📂 Navigator: update buffers - \(json.prefix(100))...")
        // Parse JSON and update SwiftUI state
    }
    
    func setActiveBuffer(_ name: String) {
        print("📂 Navigator: active buffer - \(name)")
    }
    
    func setActiveFile(_ path: String) {
        print("📂 Navigator: active file - \(path)")
    }
}

class EditorChannelHandler {
    func updateTabs(_ json: String) {
        print("📝 Editor: update tabs - \(json.prefix(100))...")
    }
    
    func selectTab(_ name: String) {
        print("📝 Editor: select tab - \(name)")
    }
}

class StatusChannelHandler {
    func updateStatus(_ json: String) {
        print("📊 Status: update - \(json.prefix(100))...")
    }
}

class AppearanceChannelHandler {
    func setMode(_ mode: String) {
        print("🎨 Appearance: set mode - \(mode)")
    }
}
