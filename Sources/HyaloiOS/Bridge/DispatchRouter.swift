#if canImport(UIKit)

import Foundation

public enum EmacsCommandID: Int32 {
    case none = 0
    case eval = 1
    case openFile = 2
    case switchBuffer = 3
    case killBuffer = 4
    case findFile = 5
    case saveBuffer = 6
    case executeCommand = 7
    case navigatorSelect = 8
    case statusTap = 9
    case appearanceChange = 10
    case search = 11
    case searchNavigate = 12
    case diagnosticNavigate = 13
    case packageRefresh = 14
    case packageUpgrade = 15
    case gitShowCommit = 16
    case gitShowDiff = 17
}

public protocol EmacsCommand {
    associatedtype Response: Decodable
    var commandID: EmacsCommandID { get }
    var payload: [String: Any] { get }
}

public extension EmacsCommand {
    func encodePayload() -> String? {
        guard JSONSerialization.isValidJSONObject(payload) else {
            print("[DispatchRouter] Invalid JSON payload for command \(commandID)")
            return nil
        }
        do {
            let data = try JSONSerialization.data(withJSONObject: payload, options: [])
            return String(data: data, encoding: .utf8)
        } catch {
            print("[DispatchRouter] Failed to encode payload: \(error)")
            return nil
        }
    }
}

public struct EmacsCommandResponse: Decodable {
    public let success: Bool
    public let error: String?
    
    public init(success: Bool, error: String? = nil) {
        self.success = success
        self.error = error
    }
}

@available(iOS 26.0, *)
@MainActor
public final class DispatchRouter {
    public static let shared = DispatchRouter()
    
    private var pendingCallbacks: [String: (Result<Data, Error>) -> Void] = [:]
    private var requestCounter: Int64 = 0
    private let callbackQueue = DispatchQueue(label: "hyalo.dispatchrouter.callbacks")
    
    private init() {}
    
    public func sendCommand(
        _ commandID: EmacsCommandID,
        payload: [String: Any],
        completion: @escaping (Result<Data, Error>) -> Void
    ) {
        guard commandID != .none else {
            completion(.failure(DispatchRouterError.invalidCommand))
            return
        }
        
        guard let jsonString = encodePayload(payload) else {
            completion(.failure(DispatchRouterError.jsonEncodingFailed))
            return
        }
        
        let requestID = generateRequestID(for: commandID)
        
        callbackQueue.async {
            self.pendingCallbacks[requestID] = completion
        }
        
        jsonString.withCString { cString in
            hyalo_ios_dispatch_command(commandID.rawValue, cString)
        }
        
        ios_signal_event_available()
    }
    
    public func sendCommand<T: EmacsCommand>(
        _ command: T,
        completion: @escaping (Result<T.Response, Error>) -> Void
    ) {
        sendCommand(command.commandID, payload: command.payload) { result in
            switch result {
            case .success(let data):
                do {
                    let response = try JSONDecoder().decode(T.Response.self, from: data)
                    completion(.success(response))
                } catch {
                    completion(.failure(DispatchRouterError.decodingFailed(error)))
                }
            case .failure(let error):
                completion(.failure(error))
            }
        }
    }
    
    public func sendCommand(_ commandID: EmacsCommandID, payload: [String: Any]) {
        guard commandID != .none else { return }
        
        guard let jsonString = encodePayload(payload) else {
            print("[DispatchRouter] Failed to encode payload for command \(commandID)")
            return
        }
        
        jsonString.withCString { cString in
            hyalo_ios_dispatch_command(commandID.rawValue, cString)
        }
        
        ios_signal_event_available()
    }
    
    func handleDispatchResponse(_ requestID: String, _ jsonResponse: String) {
        guard let data = jsonResponse.data(using: .utf8) else {
            print("[DispatchRouter] Failed to decode response JSON")
            return
        }
        
        callbackQueue.async {
            if let callback = self.pendingCallbacks.removeValue(forKey: requestID) {
                DispatchQueue.main.async {
                    callback(.success(data))
                }
            }
        }
    }
    
    func handleDispatchError(_ requestID: String, _ errorMessage: String) {
        callbackQueue.async {
            if let callback = self.pendingCallbacks.removeValue(forKey: requestID) {
                DispatchQueue.main.async {
                    callback(.failure(DispatchRouterError.emacsError(errorMessage)))
                }
            }
        }
    }
    
    private func encodePayload(_ payload: [String: Any]) -> String? {
        guard JSONSerialization.isValidJSONObject(payload) else { return nil }
        do {
            let data = try JSONSerialization.data(withJSONObject: payload, options: [])
            return String(data: data, encoding: .utf8)
        } catch {
            print("[DispatchRouter] JSON encoding error: \(error)")
            return nil
        }
    }
    
    private func generateRequestID(for commandID: EmacsCommandID) -> String {
        let counter = callbackQueue.sync {
            requestCounter += 1
            return requestCounter
        }
        return "\(commandID.rawValue)_\(counter)"
    }
}

public enum DispatchRouterError: Error {
    case invalidCommand
    case jsonEncodingFailed
    case decodingFailed(Error)
    case emacsError(String)
    case timeout
}

extension DispatchRouterError: LocalizedError {
    public var errorDescription: String? {
        switch self {
        case .invalidCommand:
            return "Invalid command ID"
        case .jsonEncodingFailed:
            return "Failed to encode command payload as JSON"
        case .decodingFailed(let error):
            return "Failed to decode response: \(error.localizedDescription)"
        case .emacsError(let message):
            return "Emacs error: \(message)"
        case .timeout:
            return "Command timed out"
        }
    }
}

@_silgen_name("hyalo_ios_dispatch_command")
func hyalo_ios_dispatch_command(_ commandID: Int32, _ jsonPayload: UnsafePointer<CChar>)



#endif
