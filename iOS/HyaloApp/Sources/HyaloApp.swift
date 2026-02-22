// HyaloApp.swift - iOS Entry Point
// Full Hyalo on iPadOS

import SwiftUI
import UIKit

// Import Emacs C API
@_silgen_name("ios_emacs_init")
func ios_emacs_init(_ argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>, _ dump_file: UnsafePointer<CChar>?) -> Int32

@_silgen_name("ios_get_fingerprint")
func ios_get_fingerprint() -> UnsafeMutablePointer<CChar>?

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
        setupEnvironment()
        startEmacsThread()
        return true
    }
    
    func setupEnvironment() {
        let docsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0]
        let cachesPath = NSSearchPathForDirectoriesInDomains(.cachesDirectory, .userDomainMask, true)[0]
        let bundlePath = Bundle.main.bundlePath
        
        setenv("HOME", docsPath, 1)
        setenv("XDG_CONFIG_HOME", "\(docsPath)/emacs", 1)
        setenv("EMACSLOADPATH", "\(bundlePath)/lisp", 1)
        setenv("EMACSDATA", "\(cachesPath)/emacs/data", 1)
        setenv("EMACSDOC", "\(bundlePath)/etc", 1)
    }
    
    func startEmacsThread() {
        emacsThread = Thread {
            let bundlePath = Bundle.main.bundlePath
            let docsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0]
            
            // Get fingerprint
            guard let fingerprint = ios_get_fingerprint() else {
                print("Failed to get fingerprint")
                return
            }
            
            // Look for existing dump
            let fingerprintStr = String(cString: fingerprint)
            let dumpPath = "\(docsPath)/emacs-\(fingerprintStr).pdmp"
            
            var dumpFile: UnsafePointer<CChar>?
            if FileManager.default.fileExists(atPath: dumpPath) {
                dumpFile = dumpPath.withCString { $0 }
            }
            
            // Create argv
            var argv: [UnsafeMutablePointer<CChar>?] = [
                strdup("emacs"),
                strdup("--init-directory"),
                strdup("\(bundlePath)/init"),
                strdup("--debug-init"),
                nil
            ]
            
            let result = ios_emacs_init(Int32(argv.count - 1), &argv, dumpFile)
            
            if result != 0 {
                print("Emacs initialization failed: \(result)")
            }
        }
        
        emacsThread?.stackSize = 64 * 1024 * 1024 // 64MB
        emacsThread?.start()
    }
}

struct ContentView: View {
    @State private var isReady = false
    
    var body: some View {
        ZStack {
            if isReady {
                HyaloMainView()
            } else {
                LoadingView()
            }
        }
        .onAppear {
            // Wait for Emacs to signal ready
            DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
                isReady = true
            }
        }
    }
}

struct LoadingView: View {
    var body: some View {
        VStack {
            Text("Hyalo")
                .font(.largeTitle)
                .fontWeight(.bold)
            Text("Loading Emacs...")
                .font(.subheadline)
                .foregroundColor(.secondary)
        }
    }
}

struct HyaloMainView: View {
    @State private var selectedSidebarItem: SidebarItem = .navigator
    @State private var showInspector = false
    @State private var showUtilityArea = false
    
    enum SidebarItem {
        case navigator
        case search
        case sourceControl
    }
    
    var body: some View {
        NavigationSplitView {
            SidebarView(selection: $selectedSidebarItem)
        } content: {
            EditorAreaView()
        } detail: {
            if showInspector {
                InspectorView()
            }
        }
        .toolbar {
            ToolbarItemGroup(placement: .navigationBarTrailing) {
                Button(action: { showInspector.toggle() }) {
                    Image(systemName: "sidebar.right")
                }
                Button(action: { showUtilityArea.toggle() }) {
                    Image(systemName: "square.bottomthird.inset.filled")
                }
            }
        }
    }
}

struct SidebarView: View {
    @Binding var selection: ContentView.SidebarItem
    
    var body: some View {
        List {
            Section("Navigator") {
                Label("Files", systemImage: "folder")
                    .tag(ContentView.SidebarItem.navigator)
                Label("Search", systemImage: "magnifyingglass")
                    .tag(ContentView.SidebarItem.search)
            }
            
            Section("Source Control") {
                Label("Changes", systemImage: "arrow.triangle.branch")
                    .tag(ContentView.SidebarItem.sourceControl)
            }
        }
        .listStyle(.sidebar)
        .navigationTitle("Hyalo")
    }
}

struct EditorAreaView: View {
    var body: some View {
        VStack {
            // Emacs will render here
            Color.black
                .overlay(
                    Text("Emacs Editor Area")
                        .foregroundColor(.green)
                )
        }
    }
}

struct InspectorView: View {
    var body: some View {
        VStack {
            Text("Inspector")
                .font(.headline)
            Divider()
            Text("File properties, git info, etc.")
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .padding()
        .background(.ultraThinMaterial)
    }
}
