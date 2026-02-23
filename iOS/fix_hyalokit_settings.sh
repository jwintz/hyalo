#!/bin/bash
# Post-generation script to add linker settings to HyaloKit target

PROJECT_FILE="HyaloApp.xcodeproj/project.pbxproj"

# Check if the file exists
if [ ! -f "$PROJECT_FILE" ]; then
    echo "Error: $PROJECT_FILE not found"
    exit 1
fi

# Add LIBRARY_SEARCH_PATHS and OTHER_LDFLAGS to HyaloKit target build settings
# This uses sed to insert the settings after the target's build configuration

# For simulator
sed -i '' '/ISA.*XCBuildConfiguration.*HyaloKit/,/\};/ {
    /settings = {/a\
        "LIBRARY_SEARCH_PATHS[sdk=iphonesimulator*]" = "$(inherited) $(SRCROOT)/../../hyalo-feedstock-unified/emacs/src $(SRCROOT)/../../hyalo-feedstock-unified/ios-sim-deps/lib";
}' "$PROJECT_FILE"

sed -i '' '/ISA.*XCBuildConfiguration.*HyaloKit/,/\};/ {
    /settings = {/a\
        "OTHER_LDFLAGS[sdk=iphonesimulator*]" = "$(inherited) -force_load $(SRCROOT)/../../hyalo-feedstock-unified/emacs/src/libemacs.a -lxml2 -ljansson -lgmp -lgnutls -lnettle -lhogweed -ltasn1 -lz -liconv";
}' "$PROJECT_FILE"

echo "Updated HyaloKit build settings"
