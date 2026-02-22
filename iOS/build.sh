#!/bin/bash
# iOS build script for Hyalo.
#
# Usage:
#   ./build.sh                  # copy resources + generate project + build for simulator
#   ./build.sh --resources-only # copy resources only (for manual xcodebuild)
#
# Build output goes to /tmp/hyalo-ios-build (same as the simulator test loop).
# Resources are copied from the feedstock and the hyalo-unified lisp/init directories
# into iOS/HyaloApp/Resources/ which Xcode bundles into the .app.

set -e

FEEDSTOCK="$HOME/Syntropment/hyalo-feedstock-unified"
IOS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$IOS_DIR/.." && pwd)"

# -- Copy resources from feedstock and hyalo-unified --

mkdir -p "$IOS_DIR/HyaloApp/Resources"

cp -R "$FEEDSTOCK/ios/Resources/lisp" "$IOS_DIR/HyaloApp/Resources/"
cp -R "$FEEDSTOCK/ios/Resources/etc"  "$IOS_DIR/HyaloApp/Resources/"
cp "$FEEDSTOCK/ios/Resources/bootstrap-emacs.pdmp" "$IOS_DIR/HyaloApp/Resources/"

cp -R "$REPO_DIR/init" "$IOS_DIR/HyaloApp/Resources/"

# iOS-specific lisp files overlay the feedstock lisp directory
cp "$REPO_DIR/lisp/hyalo-ios.el"          "$IOS_DIR/HyaloApp/Resources/lisp/"
cp "$REPO_DIR/lisp/hyalo-channels-ios.el" "$IOS_DIR/HyaloApp/Resources/lisp/"

if [ "$1" = "--resources-only" ]; then
    echo "Resources copied to $IOS_DIR/HyaloApp/Resources/"
    exit 0
fi

# -- Generate Xcode project and build --

cd "$IOS_DIR"
swift run --package-path "$REPO_DIR" xcodegen generate

xcodebuild \
    -project HyaloApp.xcodeproj \
    -scheme HyaloApp \
    -configuration Debug \
    -destination "platform=iOS Simulator,name=iPad Pro 13-inch (M5)" \
    -derivedDataPath /tmp/hyalo-ios-build \
    build
