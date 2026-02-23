#if canImport(UIKit)

import Foundation

@available(iOS 26.0, *)
public struct SwitchBufferCommand: EmacsCommand {
    public typealias Response = SwitchBufferResponse
    
    public let commandID: EmacsCommandID = .switchBuffer
    public let payload: [String: Any]
    
    public let bufferName: String
    
    public init(bufferName: String) {
        self.bufferName = bufferName
        self.payload = ["buffer": bufferName]
    }
}

@available(iOS 26.0, *)
public struct SwitchBufferResponse: Decodable {
    public let success: Bool
    public let error: String?
}

@available(iOS 26.0, *)
public struct KillBufferCommand: EmacsCommand {
    public typealias Response = KillBufferResponse
    
    public let commandID: EmacsCommandID = .killBuffer
    public let payload: [String: Any]
    
    public let bufferName: String
    public let force: Bool
    
    public init(bufferName: String, force: Bool = false) {
        self.bufferName = bufferName
        self.force = force
        self.payload = [
            "buffer": bufferName,
            "force": force
        ]
    }
}

@available(iOS 26.0, *)
public struct KillBufferResponse: Decodable {
    public let success: Bool
    public let killed: Bool
    public let error: String?
}

@available(iOS 26.0, *)
public struct SaveBufferCommand: EmacsCommand {
    public typealias Response = SaveBufferResponse
    
    public let commandID: EmacsCommandID = .saveBuffer
    public let payload: [String: Any]
    
    public let bufferName: String?
    
    public init(bufferName: String? = nil) {
        self.bufferName = bufferName
        if let name = bufferName {
            self.payload = ["buffer": name]
        } else {
            self.payload = [:]
        }
    }
}

@available(iOS 26.0, *)
public struct SaveBufferResponse: Decodable {
    public let success: Bool
    public let path: String?
    public let error: String?
}

#endif
