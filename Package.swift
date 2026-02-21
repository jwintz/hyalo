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
            url: "https://github.com/SavchenkoValeriy/emacs-swift-module.git",
            branch: "main"
        ),
        .package(
            url: "https://github.com/migueldeicaza/SwiftTerm.git",
            from: "1.11.2"
        ),
        .package(
            url: "https://github.com/mchakravarty/ProjectNavigator.git",
            from: "1.0.0"
        )
    ],
    targets: [
        .target(
            name: "Hyalo",
            dependencies: [
                .product(name: "EmacsSwiftModule", package: "emacs-swift-module"),
                .product(name: "SwiftTerm", package: "SwiftTerm"),
                .product(name: "Files", package: "ProjectNavigator"),
                .product(name: "ProjectNavigator", package: "ProjectNavigator")
            ],
            swiftSettings: [
                .swiftLanguageMode(.v5)
            ],
            plugins: [
                .plugin(name: "ModuleFactoryPlugin", package: "emacs-swift-module")
            ]
        )
    ]
)
