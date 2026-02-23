// HyaloKitFramework.swift - Re-exports HyaloKit SPM module through native framework.
// This enables HyaloApp to `import HyaloKit` via the embedded dynamic framework
// while keeping libemacs.a linked only in this framework (no symbol duplication).

@_exported import HyaloKit