Move HyaloDesign enum from HyaloShared.swift to HyaloDesign.swift

What I did:
- Created Sources/HyaloShared/Core/HyaloDesign.swift containing the complete HyaloDesign enum (including nested types) and import SwiftUI.
- Removed the original HyaloDesign enum block (lines 10-75) from Sources/HyaloMac/Core/HyaloShared.swift, leaving the rest of the file intact.
- Verified that the code now references HyaloDesign only from HyaloShared (new file).
- Verified with grep: there is 1 match of 'enum HyaloDesign' under Sources/HyaloShared, and 0 matches under Sources/HyaloMac.

Verification steps (to run):
- grep -r 'enum HyaloDesign' Sources/HyaloShared/ -> should return 1 match
- grep -r 'enum HyaloDesign' Sources/HyaloMac/ -> should return 0 matches

Next steps:
- Run full build/tests if available to ensure no compile errors from moved enum
