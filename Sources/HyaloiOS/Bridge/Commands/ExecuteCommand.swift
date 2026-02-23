#if canImport(UIKit)

import Foundation

@available(iOS 26.0, *)
public struct ExecuteCommand: EmacsCommand {
    public typealias Response = ExecuteResponse
    
    public let commandID: EmacsCommandID = .executeCommand
    public let payload: [String: Any]
    
    public let command: String
    public let args: [String]
    
    public init(command: String, args: [String] = []) {
        self.command = command
        self.args = args
        self.payload = [
            "command": command,
            "args": args
        ]
    }
}

@available(iOS 26.0, *)
public struct ExecuteResponse: Decodable {
    public let success: Bool
    public let result: String?
    public let error: String?
}

#endif
