#!/bin/bash
set -e

echo "=== Building Hyalo for iOS Simulator ==="

# Paths
FEEDSTOCK="$HOME/Syntropment/hyalo-feedstock-unified"
EMACS_SRC="$FEEDSTOCK/emacs/src"
IOS_DIR="$PWD/iOS"
BUILD_DIR="$PWD/build-ios"

# Create build directory
mkdir -p "$BUILD_DIR"

# Generate Xcode project with xcodegen if available, otherwise use xcodebuild directly
if command -v xcodegen &> /dev/null; then
    echo "Generating Xcode project..."
    cd "$IOS_DIR"
    xcodegen generate
else
    echo "⚠️  xcodegen not found, using manual xcodebuild..."
fi

echo "Building with xcodebuild..."
xcodebuild \
    -project "$IOS_DIR/HyaloApp.xcodeproj" \
    -scheme HyaloApp \
    -configuration Debug \
    -destination "platform=iOS Simulator,name=iPad Pro 13-inch (M5)" \
    -derivedDataPath "$BUILD_DIR/DerivedData" \
    LIBRARY_SEARCH_PATHS="$EMACS_SRC" \
    OTHER_LDFLAGS="-lemacs" \
    build

echo "✅ Build complete!"
