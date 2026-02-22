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

// MARK: - Channel Setup (called from Emacs)

@_cdecl("hyalo_ios_setup_channels")
func bridgeSetupChannels() {
    DispatchQueue.main.async {
        // Channels are set up — signal readiness
        print("[HyaloKit] iOS channels setup complete")
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

#endif // canImport(UIKit)
