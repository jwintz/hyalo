#!/bin/bash
set -e

echo "=== Building Hyalo for iOS Simulator ==="

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FEEDSTOCK_DIR="$PROJECT_DIR/../hyalo-feedstock-unified"
BUILD_DIR="$PROJECT_DIR/build"

echo "Project dir: $PROJECT_DIR"
echo "Build dir: $BUILD_DIR"

# Create build directory
mkdir -p "$BUILD_DIR"

# Build with xcodebuild directly
xcodebuild \
    -project "$PROJECT_DIR/HyaloApp.xcodeproj" \
    -scheme HyaloApp \
    -configuration Debug \
    -destination "platform=iOS Simulator,name=iPad Pro 13-inch (M5)" \
    -derivedDataPath "$BUILD_DIR/DerivedData" \
    build 2>&1 | tee "$BUILD_DIR/build.log"

echo "=== Build complete ==="
