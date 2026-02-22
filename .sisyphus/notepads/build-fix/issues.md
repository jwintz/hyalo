## Issue: HyaloShared build errors
- EnvironmentDropDownView.swift: clipShape usage on a Color ShapeStyle caused compiler error. Fixed by using Capsule().fill(...) in background to apply capsule shape without clipShape.
- UserHostDropDownView.swift: heavy type inference leading to compiler timeout. Fixed by introducing a lightweight hoverBackground computed property and using it in background to reduce expression complexity. Also moved its declaration for visibility.
