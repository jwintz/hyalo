import SwiftUI
import UIKit

// Hyalo iOS Entry Point
// This app embeds Emacs as a static library and wraps it in SwiftUI

@main
struct HyaloApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
    
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}

class AppDelegate: NSObject, UIApplicationDelegate {
    var emacsThread: Thread?
    
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        // Setup environment for Emacs
        setupEmacsEnvironment()
        
        // Start Emacs on background thread
        startEmacsThread()
        
        return true
    }
    
    func setupEmacsEnvironment() {
        let docsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0]
        let cachesPath = NSSearchPathForDirectoriesInDomains(.cachesDirectory, .userDomainMask, true)[0]
        let bundlePath = Bundle.main.bundlePath
        
        setenv("HOME", docsPath, 1)
        setenv("XDG_CONFIG_HOME", "\(docsPath)/emacs", 1)
        setenv("EMACSLOADPATH", "\(bundlePath)/emacs/lisp", 1)
        setenv("EMACSDATA", "\(cachesPath)/emacs/data", 1)
        setenv("EMACSDOC", "\(bundlePath)/emacs/etc", 1)
        
        print("📁 Environment configured:")
        print("   HOME: \(docsPath)")
        print("   XDG_CONFIG_HOME: \(docsPath)/emacs")
    }
    
    func startEmacsThread() {
        emacsThread = Thread {
            print("🚀 Starting Emacs on background thread...")
            
            // Call ios_emacs_init() from libemacs.a
            let result = ios_emacs_init()
            
            if result == 0 {
                print("✅ Emacs initialized successfully")
            } else {
                print("❌ Emacs initialization failed: \(result)")
            }
        }
        
        // 64MB stack for deep Lisp recursion
        emacsThread?.stackSize = 64 * 1024 * 1024
        emacsThread?.start()
    }
}

struct ContentView: View {
    @State private var status = "Starting Emacs..."
    
    var body: some View {
        VStack {
            Text("Hyalo for iPadOS")
                .font(.largeTitle)
                .padding()
            
            Text(status)
                .font(.headline)
                .foregroundColor(.secondary)
            
            // EmacsView will be integrated here
            // For now, show placeholder
            Rectangle()
                .fill(Color.black)
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .overlay(
                    Text("Emacs Frame")
                        .foregroundColor(.green)
                        .font(.system(.body, design: .monospaced))
                )
        }
    }
}

// Bridge to C functions from libemacs.a
@_silgen_name("ios_emacs_init")
func ios_emacs_init() -> Int32
