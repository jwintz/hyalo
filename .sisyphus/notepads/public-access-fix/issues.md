## Issues encountered
- Initial attempt to make types public surfaced Swift protocol conformance requirements (View.body must be public). Resolved by updating body properties to public as well.
- Build of the entire project fails due to cross-module dependencies (UIKit on macOS targets, missing HyaloWorkspaceState type) which are outside HyaloShared fix scope. The focused HyaloShared target builds cleanly.
