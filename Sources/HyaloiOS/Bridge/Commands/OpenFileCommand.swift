#if canImport(UIKit)

import Foundation

@available(iOS 26.0, *)
public struct OpenFileCommand: EmacsCommand {
    public typealias Response = OpenFileResponse
    
    public let commandID: EmacsCommandID = .openFile
    public let payload: [String: Any]
    
    public let path: String
    public let line: Int?
    public let column: Int?
    
    public init(path: String, line: Int? = nil, column: Int? = nil) {
        self.path = path
        self.line = line
        self.column = column
        
        var p: [String: Any] = ["path": path]
        if let line = line { p["line"] = line }
        if let column = column { p["column"] = column }
        self.payload = p
    }
}

@available(iOS 26.0, *)
public struct OpenFileResponse: Decodable {
    public let success: Bool
    public let bufferName: String?
    public let error: String?
}

@available(iOS 26.0, *)
public struct FindFileCommand: EmacsCommand {
    public typealias Response = OpenFileResponse
    
    public let commandID: EmacsCommandID = .findFile
    public let payload: [String: Any]
    
    public let path: String
    
    public init(path: String) {
        self.path = path
        self.payload = ["path": path]
    }
}

#endif
