## Architectural decisions
- Expose HyaloShared types publicly to satisfy cross-module usage by HyaloMac. Limit exposure to only those types that HyaloMac actually references (avoid making additional internal types public).
- When exposing public Swift types, also expose explicit public initializers if default initializers are internal to ensure external modules can instantiate them.
- For SwiftUI View conformances, also mark 'body' property as public to satisfy the protocol requirements.
