#!/bin/bash
set -e

FEEDSTOCK="$HOME/Syntropment/hyalo-feedstock-unified"
IOS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$IOS_DIR/build-ios"

mkdir -p "$IOS_DIR/HyaloApp/Resources"

cp -R "$FEEDSTOCK/ios/Resources/lisp" "$IOS_DIR/HyaloApp/Resources/"
cp -R "$FEEDSTOCK/ios/Resources/etc" "$IOS_DIR/HyaloApp/Resources/"
cp "$FEEDSTOCK/ios/Resources/bootstrap-emacs.pdmp" "$IOS_DIR/HyaloApp/Resources/"
cp -R "$IOS_DIR/../init" "$IOS_DIR/HyaloApp/Resources/"
cp "$IOS_DIR/../lisp/hyalo-ios.el" "$IOS_DIR/HyaloApp/Resources/lisp/"
cp "$IOS_DIR/../lisp/hyalo-channels-ios.el" "$IOS_DIR/HyaloApp/Resources/lisp/"

xcodegen generate

xcodebuild \
    -project HyaloApp.xcodeproj \
    -scheme HyaloApp \
    -configuration Debug \
    -destination "platform=iOS Simulator,name=iPad Pro 13-inch (M5)" \
    -derivedDataPath "$BUILD_DIR/DerivedData" \
    build
