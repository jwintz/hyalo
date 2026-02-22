// swift-tools-version: 6.2

import PackageDescription

let package = Package(
    name: "Hyalo",
    platforms: [
        .macOS(.v26),
        .iOS(.v26)
    ],
    products: [
        // macOS: dynamic module loaded by Emacs
        .library(
            name: "Hyalo",
            type: .dynamic,
            targets: ["Hyalo"]
        ),
        // iOS: dynamic framework embedded in app
        .library(
            name: "HyaloKit",
            type: .dynamic,
            targets: ["HyaloKit"]
        )
    ],
    dependencies: [
        .package(
            url: "https://github.com/SavchenkoValeriy/emacs-swift-module.git",
            branch: "main"
        ),
        .package(
            url: "https://github.com/migueldeicaza/SwiftTerm.git",
            from: "1.0.0"
        ),
        .package(
            url: "https://github.com/mchakravarty/ProjectNavigator.git",
            from: "1.0.0"
        )
    ],
    targets: [
        // Cross-platform shared code (models, view models, pure SwiftUI views)
        .target(
            name: "HyaloShared",
            dependencies: [
                .product(name: "Files", package: "ProjectNavigator"),
                .product(name: "ProjectNavigator", package: "ProjectNavigator")
            ],
            path: "Sources/HyaloShared",
            swiftSettings: [
                .swiftLanguageMode(.v5)
            ]
        ),

        // macOS: Emacs dynamic module (AppKit, EmacsSwiftModule)
        .target(
            name: "Hyalo",
            dependencies: [
                "HyaloShared",
                .product(name: "EmacsSwiftModule", package: "emacs-swift-module"),
                .product(name: "SwiftTerm", package: "SwiftTerm")
            ],
            path: "Sources/HyaloMac",
            swiftSettings: [
                .swiftLanguageMode(.v5)
            ],
            plugins: [
                .plugin(name: "ModuleFactoryPlugin", package: "emacs-swift-module")
            ]
        ),

        // iOS: dynamic framework (UIKit, C FFI bridge to libemacs)
        .target(
            name: "HyaloKit",
            dependencies: [
                "HyaloShared"
            ],
            path: "Sources/HyaloiOS",
            swiftSettings: [
                .swiftLanguageMode(.v5)
            ]
        )
    ]
)
