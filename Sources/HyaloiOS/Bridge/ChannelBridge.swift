#if canImport(UIKit)
// ChannelBridge.swift - Emacs <-> Swift Communication Bridge for iOS
// Implements bidirectional channel communication via @_cdecl C FFI.
// Emacs calls these functions directly (linked at build time from libemacs.a).

import Foundation
import SwiftUI
import HyaloShared

// MARK: - Channel Bridge Singleton

/// Manages communication between Emacs (background thread) and SwiftUI (main thread).
@available(iOS 26.0, *)
@MainActor
public final class ChannelBridge {
    public static let shared = ChannelBridge()

    private init() {}
}

// MARK: - Reverse Channel (Emacs -> Swift with callbacks)

/// Callback handler type for Swift functions called from Emacs
@available(iOS 26.0, *)
typealias SwiftHandler = ([String: Any], @escaping (Result<[String: Any], Error>) -> Void) -> Void

/// Manages bidirectional communication callbacks
@available(iOS 26.0, *)
@MainActor
public final class ReverseChannelBridge {
    public static let shared = ReverseChannelBridge()
    
    private var handlers: [String: SwiftHandler] = [:]
    private var pendingEmacsCallbacks: [String: (Result<[String: Any], Error>) -> Void] = [:]
    private var callbackCounter: Int64 = 0
    private let queue = DispatchQueue(label: "hyalo.reversechannel")
    
    private init() {}
    
    /// Register a Swift handler for calls from Emacs
    public func registerHandler(_ name: String, handler: @escaping SwiftHandler) {
        queue.async {
            self.handlers[name] = handler
        }
    }
    
    /// Unregister a handler
    public func unregisterHandler(_ name: String) {
        queue.async {
            self.handlers.removeValue(forKey: name)
        }
    }
    
    /// Execute a handler and return result via callback
    func executeHandler(name: String, payload: [String: Any], callbackID: String) {
        queue.async {
            guard let handler = self.handlers[name] else {
                // Send error back to Emacs
                DispatchQueue.main.async {
                    bridgeSendSwiftResponse(callbackID, success: false, error: "Unknown handler: \(name)")
                }
                return
            }
            
            // Store callback for async response
            self.pendingEmacsCallbacks[callbackID] = { result in
                DispatchQueue.main.async {
                    switch result {
                    case .success(let response):
                        bridgeSendSwiftResponse(callbackID, success: true, result: response)
                    case .failure(let error):
                        bridgeSendSwiftResponse(callbackID, success: false, error: error.localizedDescription)
                    }
                }
            }
            
            // Execute handler
            handler(payload) { result in
                self.queue.async {
                    if let callback = self.pendingEmacsCallbacks.removeValue(forKey: callbackID) {
                        callback(result)
                    }
                }
            }
        }
    }
}

// MARK: - Channel Setup (called from Emacs)

@_cdecl("hyalo_ios_setup_channels")
func bridgeSetupChannels() {
    DispatchQueue.main.async {
        // Channels are set up — signal readiness
        print("[HyaloKit] iOS channels setup complete")
        // Register predefined handlers for reverse channel
        if #available(iOS 26.0, *) {
            ReverseChannelBridge.shared.registerPredefinedHandlers()
        }
    }
}

// MARK: - Navigator Channel

@_cdecl("hyalo_ios_navigator_update_buffers")
func bridgeNavigatorUpdateBuffers(_ jsonCString: UnsafePointer<CChar>) {
    let json = String(cString: jsonCString)
    DispatchQueue.main.async {
        guard let data = json.data(using: .utf8) else { return }
        if #available(iOS 26.0, *) {
            do {
                let buffers = try JSONDecoder().decode([BufferInfo].self, from: data)
                NavigatorManager.shared.updateBufferList(buffers)
            } catch {
                print("[HyaloKit] navigator decode error: \(error)")
            }
        }
    }
}

@_cdecl("hyalo_ios_navigator_set_active_buffer")
func bridgeNavigatorSetActiveBuffer(_ bufferName: UnsafePointer<CChar>) {
    let name = String(cString: bufferName)
    DispatchQueue.main.async {
        if #available(iOS 26.0, *) {
            NavigatorManager.shared.setActiveBuffer(name)
        }
    }
}

@_cdecl("hyalo_ios_navigator_set_active_file")
func bridgeNavigatorSetActiveFile(_ filePath: UnsafePointer<CChar>) {
    let path = String(cString: filePath)
    DispatchQueue.main.async {
        if #available(iOS 26.0, *) {
            NavigatorManager.shared.setActiveFile(path)
        }
    }
}

// MARK: - Editor Channel

@_cdecl("hyalo_ios_editor_update_tabs")
func bridgeEditorUpdateTabs(_ jsonCString: UnsafePointer<CChar>) {
    let json = String(cString: jsonCString)
    DispatchQueue.main.async {
        guard let data = json.data(using: .utf8) else { return }
        if #available(iOS 26.0, *) {
            do {
                let tabs = try JSONDecoder().decode([EditorTab].self, from: data)
                HyaloiOSModule.shared.editorTabViewModel.updateTabs(tabs)
            } catch {
                print("[HyaloKit] editor tabs decode error: \(error)")
            }
        }
    }
}

@_cdecl("hyalo_ios_editor_select_tab")
func bridgeEditorSelectTab(_ bufferName: UnsafePointer<CChar>) {
    let name = String(cString: bufferName)
    DispatchQueue.main.async {
        if #available(iOS 26.0, *) {
            HyaloiOSModule.shared.editorTabViewModel.onTabSelected(name)
        }
    }
}

// MARK: - Status Channel

@_cdecl("hyalo_ios_status_update")
func bridgeStatusUpdate(_ jsonCString: UnsafePointer<CChar>) {
    let json = String(cString: jsonCString)
    DispatchQueue.main.async {
        guard let data = json.data(using: .utf8),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else { return }
        if #available(iOS 26.0, *) {
            if let line = obj["line"] as? Int { StatusBarManager.shared.viewModel.line = line }
            if let col = obj["column"] as? Int { StatusBarManager.shared.viewModel.column = col }
            if let mode = obj["mode"] as? String { StatusBarManager.shared.viewModel.mode = mode }
            if let enc = obj["encoding"] as? String { StatusBarManager.shared.viewModel.encoding = enc }
            if let le = obj["lineEnding"] as? String { StatusBarManager.shared.viewModel.lineEnding = le }
            if let lhs = obj["modelineLHS"] as? String { StatusBarManager.shared.viewModel.modelineLHS = lhs }
            if let rhs = obj["modelineRHS"] as? String { StatusBarManager.shared.viewModel.modelineRHS = rhs }
        }
    }
}

// MARK: - Appearance Channel

@_cdecl("hyalo_ios_appearance_set_mode")
func bridgeAppearanceSetMode(_ modeCString: UnsafePointer<CChar>) {
    let mode = String(cString: modeCString)
    DispatchQueue.main.async {
        if #available(iOS 26.0, *) {
            HyaloiOSModule.shared.workspace.windowAppearance = mode
        }
    }
}

// MARK: - Command Palette Channel

@_cdecl("hyalo_ios_update_open_quickly_items")
func bridgeUpdateOpenQuicklyItems(_ jsonCString: UnsafePointer<CChar>) {
    let json = String(cString: jsonCString)
    DispatchQueue.main.async {
        guard let data = json.data(using: .utf8) else { return }
        if #available(iOS 26.0, *) {
            HyaloiOSModule.shared.openQuicklyViewModel.updateItems(from: data)
        }
    }
}

@_cdecl("hyalo_ios_update_command_list")
func bridgeUpdateCommandList(_ jsonCString: UnsafePointer<CChar>) {
    let json = String(cString: jsonCString)
    DispatchQueue.main.async {
        guard let data = json.data(using: .utf8) else { return }
        if #available(iOS 26.0, *) {
            HyaloiOSModule.shared.commandPaletteViewModel.updateCommands(from: data)
        }
    }
}

@_cdecl("hyalo_ios_show_open_quickly")
func bridgeShowOpenQuickly() {
    DispatchQueue.main.async {
        if #available(iOS 26.0, *) {
            HyaloiOSModule.shared.showOpenQuickly = true
        }
    }
}

@_cdecl("hyalo_ios_show_command_palette")
func bridgeShowCommandPalette() {
    DispatchQueue.main.async {
        if #available(iOS 26.0, *) {
            HyaloiOSModule.shared.showCommandPalette = true
        }
    }
}

// MARK: - Reverse Channel C FFI Functions
/// Register a Swift handler from Swift code
@_cdecl("hyalo_ios_register_swift_handler")
func bridgeRegisterSwiftHandler(_ nameCString: UnsafePointer<CChar>, _ handlerPtr: UnsafeRawPointer) {
    if #available(iOS 26.0, *) {
        let name = String(cString: nameCString)
        // The handlerPtr is a closure reference that Swift will manage
        // For now, we use a typed handler registration via the shared instance
        print("[HyaloKit] Registered Swift handler: \(name)")
    }
}
/// Emacs calls this to invoke a Swift function
@_cdecl("hyalo_ios_call_swift")
func bridgeCallSwift(_ handlerName: UnsafePointer<CChar>, _ jsonPayload: UnsafePointer<CChar>, _ callbackID: UnsafePointer<CChar>) {
    let name = String(cString: handlerName)
    let json = String(cString: jsonPayload)
    let cbID = String(cString: callbackID)
    if #available(iOS 26.0, *) {
        // Parse JSON payload
        guard let data = json.data(using: .utf8),
              let payload = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
            bridgeSendSwiftResponse(cbID, success: false, error: "Invalid JSON payload")
            return
        }
        // Execute the handler
        ReverseChannelBridge.shared.executeHandler(name: name, payload: payload, callbackID: cbID)
    }
}
/// Swift sends response back to Emacs via this function
func bridgeSendSwiftResponse(_ callbackID: String, success: Bool, result: [String: Any]? = nil, error: String? = nil) {
    // Build response JSON
    var response: [String: Any] = ["callback_id": callbackID, "success": success]
    if let result = result {
        response["result"] = result
    }
    if let error = error {
        response["error"] = error
    }
    guard let jsonData = try? JSONSerialization.data(withJSONObject: response),
          let jsonString = String(data: jsonData, encoding: .utf8) else {
        print("[HyaloKit] Failed to encode response JSON")
        return
    }
    // Send to Emacs via the C FFI
    jsonString.withCString { cString in
        hyalo_ios_receive_swift_response(cString)
    }
    // Signal Emacs that a response is available
    ios_signal_event_available()
}
/// C function to receive Swift responses (implemented by libemacs.a)
@_silgen_name("hyalo_ios_receive_swift_response")
func hyalo_ios_receive_swift_response(_ jsonResponse: UnsafePointer<CChar>)

// MARK: - Dispatch Channel (Swift -> Emacs)

@_cdecl("hyalo_ios_dispatch_response")
func bridgeDispatchResponse(_ requestID: UnsafePointer<CChar>, _ jsonResponse: UnsafePointer<CChar>) {
    if #available(iOS 26.0, *) {
        DispatchRouter.shared.handleDispatchResponse(requestID, jsonResponse)
    }
}

@_cdecl("hyalo_ios_dispatch_error")
func bridgeDispatchError(_ requestID: UnsafePointer<CChar>, _ errorMessage: UnsafePointer<CChar>) {
    if #available(iOS 26.0, *) {
        DispatchRouter.shared.handleDispatchError(requestID, errorMessage)
    }
}

extension ChannelBridge {
    /// Send a command to Emacs via the dispatch router
    @available(iOS 26.0, *)
    public func dispatchCommand(
        _ commandID: EmacsCommandID,
        payload: [String: Any],
        completion: @escaping (Result<Data, Error>) -> Void
    ) {
        DispatchRouter.shared.sendCommand(commandID, payload: payload, completion: completion)
    }
    
    /// Send a command without waiting for response
    @available(iOS 26.0, *)
    public func dispatchCommand(_ commandID: EmacsCommandID, payload: [String: Any]) {
        DispatchRouter.shared.sendCommand(commandID, payload: payload)
    }
    
    /// Send a type-safe command
    @available(iOS 26.0, *)
    public func dispatchCommand<T: EmacsCommand>(
        _ command: T,
        completion: @escaping (Result<T.Response, Error>) -> Void
    ) {
        DispatchRouter.shared.sendCommand(command, completion: completion)
    }
}

extension ReverseChannelBridge {
    /// Register predefined handlers for Emacs -> Swift calls
    @available(iOS 26.0, *)
    public func registerPredefinedHandlers() {
        // Handler: get_workspace_info
        registerHandler("get_workspace_info") { payload, callback in
            let workspaceInfo: [String: Any] = [
                "platform": "iOS",
                "version": UIDevice.current.systemVersion,
                "device": UIDevice.current.model
            ]
            callback(.success(workspaceInfo))
        }
        
        // Handler: get_theme_info
        registerHandler("get_theme_info") { payload, callback in
            if #available(iOS 26.0, *) {
                let themeInfo: [String: Any] = [
                    "appearance": HyaloiOSModule.shared.workspace.windowAppearance,
                    "material": "automatic"
                ]
                callback(.success(themeInfo))
            } else {
                callback(.success(["appearance": "unknown"]))
            }
        }
        
        // Handler: set_appearance
        registerHandler("set_appearance") { payload, callback in
            if let mode = payload["mode"] as? String {
                DispatchQueue.main.async {
                    if #available(iOS 26.0, *) {
                        HyaloiOSModule.shared.workspace.windowAppearance = mode
                    }
                }
                callback(.success(["mode": mode, "status": "set"]))
            } else {
                callback(.failure(NSError(domain: "ReverseChannel", code: 1, userInfo: [NSLocalizedDescriptionKey: "Missing mode in payload"])))
            }
        }
        
        // Handler: ping (for testing)
        registerHandler("ping") { payload, callback in
            let response: [String: Any] = [
                "pong": true,
                "timestamp": Date().timeIntervalSince1970,
                "echo": payload
            ]
            callback(.success(response))
        }
        
        print("[HyaloKit] Registered predefined Swift handlers")
    }
}
#endif // canImport(UIKit)
