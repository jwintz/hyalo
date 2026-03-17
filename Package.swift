// swift-tools-version: 6.2

import PackageDescription

let package = Package(
    name: "Hyalo",
    platforms: [
        .macOS(.v26)
    ],
    products: [
        .library(
            name: "Hyalo",
            type: .dynamic,
            targets: ["Hyalo"]
        )
    ],
    dependencies: [
        .package(
            path: "../kelyphos"
        ),
        .package(
            url: "https://github.com/SavchenkoValeriy/emacs-swift-module.git",
            branch: "main"
        ),
        .package(
            url: "https://github.com/migueldeicaza/SwiftTerm.git",
            from: "1.12.0"
        )
    ],
    targets: [
        .target(
            name: "HyaloShared",
            dependencies: [
                .product(name: "KelyphosKit", package: "kelyphos"),

                .product(name: "SwiftTerm", package: "SwiftTerm")
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

        .testTarget(
            name: "HyaloSharedTests",
            dependencies: ["HyaloShared"],
            path: "Tests/HyaloSharedTests",
            swiftSettings: [
                .swiftLanguageMode(.v5)
            ]
        )
    ]
)
