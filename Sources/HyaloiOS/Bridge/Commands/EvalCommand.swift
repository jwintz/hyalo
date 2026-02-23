#if canImport(UIKit)

import Foundation

@available(iOS 26.0, *)
public struct EvalCommand: EmacsCommand {
    public typealias Response = EvalResponse
    
    public let commandID: EmacsCommandID = .eval
    public let payload: [String: Any]
    
    public let expression: String
    
    public init(expression: String) {
        self.expression = expression
        self.payload = ["expression": expression]
    }
}

@available(iOS 26.0, *)
public struct EvalResponse: Decodable {
    public let result: String?
    public let error: String?
    
    public var isSuccess: Bool {
        return error == nil
    }
}

#endif
