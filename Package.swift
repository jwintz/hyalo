// swift-tools-version: 6.2

import PackageDescription

let package = Package(
    name: "Hyalo",
    platforms: [
        .macOS(.v26),
        .iOS(.v26)
    ],
    products: [
        .library(
            name: "Hyalo",
            type: .dynamic,
            targets: ["Hyalo"]
        ),
        .library(
            name: "HyaloKit",
            type: .static,
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
        ),
        .package(
            url: "https://github.com/yonaskolb/XcodeGen.git",
            from: "2.44.1"
        )
    ],
    targets: [
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
        .target(
            name: "HyaloKit",
            dependencies: [
                "HyaloShared",
                "HyaloEmacsStubs",
                .product(name: "SwiftTerm", package: "SwiftTerm")
            ],
            path: "Sources/HyaloiOS",
            swiftSettings: [
                .swiftLanguageMode(.v5)
            ]
        ),
        .target(
            name: "HyaloEmacsStubs",
            path: "Sources/HyaloEmacsStubs",
            publicHeadersPath: ""
        )
    ]
)
