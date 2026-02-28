#if os(macOS)
import AppKit
import SwiftUI

// MARK: - Appearance Callbacks (wired from HyaloMac at startup)

/// Static closures injected by HyaloMac at module init time.
/// HyaloShared cannot import HyaloMac (circular), so the macOS-specific
/// side-effects (NSAppearance, fringe alpha, Emacs channels) are invoked
/// via these callbacks instead of a direct dependency.
@available(macOS 26.0, *)
public enum InspectorAppearanceCallbacks {
    /// Called when the user changes light/dark/auto appearance mode.
    /// Receives the new mode string ("light", "dark", "auto") and the resolved NSAppearance.
    public static var onAppearanceModeChanged: ((String, NSAppearance?) -> Void)?
    /// Called when the user changes the background opacity slider.
    public static var onOpacityChanged: ((Double) -> Void)?
    /// Called when the user changes the vibrancy material.
    public static var onMaterialChanged: ((VibrancyMaterial) -> Void)?
}

@available(macOS 26.0, *)
struct InspectorAppearanceView: View {
    @Environment(HyaloWorkspaceState.self) var workspace
    var body: some View {
        InspectorAppearanceContent(workspace: workspace)
    }
}

@available(macOS 26.0, *)
private struct InspectorAppearanceContent: View {
    @Bindable var workspace: HyaloWorkspaceState

    private var presetBinding: Binding<AppearancePreset?> {
        Binding(
            get: {
                if workspace.backgroundAlpha == 0.0 && workspace.vibrancyMaterial == .ultraThin { return .clear }
                if workspace.backgroundAlpha == 0.5 && workspace.vibrancyMaterial == .thin { return .balanced }
                if workspace.backgroundAlpha == 1.0 && workspace.vibrancyMaterial == .none { return .solid }
                return nil
            },
            set: { newValue in
                guard let preset = newValue else { return }
                switch preset {
                case .clear:
                    workspace.backgroundAlpha = 0.0
                    workspace.vibrancyMaterial = .ultraThin
                case .balanced:
                    workspace.backgroundAlpha = 0.5
                    workspace.vibrancyMaterial = .thin
                case .solid:
                    workspace.backgroundAlpha = 1.0
                    workspace.vibrancyMaterial = .none
                }
            }
        )
    }

    var body: some View {
        Form {
            if !workspace.currentThemeName.isEmpty {
                Section("Theme") {
                    LabeledContent("Current") {
                        Text(workspace.currentThemeName)
                            .font(.system(size: 11, design: .monospaced))
                            .foregroundStyle(.secondary)
                    }
                }
            }

            Section("Window") {
                LabeledContent("Appearance") {
                    Picker("Appearance", selection: $workspace.windowAppearance) {
                        Label("Auto", systemImage: "circle.lefthalf.filled").tag("auto")
                        Label("Light", systemImage: "sun.max.fill").tag("light")
                        Label("Dark", systemImage: "moon.fill").tag("dark")
                    }
                    .pickerStyle(.segmented)
                    .labelsHidden()
                }

                LabeledContent("Opacity") {
                    HStack(spacing: 8) {
                        Slider(value: $workspace.backgroundAlpha, in: 0...1)
                        Text("\(Int(workspace.backgroundAlpha * 100))%")
                            .monospacedDigit()
                            .foregroundStyle(.secondary)
                            .frame(width: 35, alignment: .trailing)
                    }
                }

                LabeledContent("Vibrancy") {
                    Picker("Material", selection: $workspace.vibrancyMaterial) {
                        ForEach(VibrancyMaterial.allCases, id: \.self) { material in
                            Text(material.rawValue.capitalized).tag(material)
                        }
                    }
                    .labelsHidden()
                    .frame(maxWidth: .infinity, alignment: .leading)
                }
            }

            Section("Presets") {
                // Note: This section is unique to macOS version (no vibrancy in iOS)
                Picker("Presets", selection: presetBinding) {
                    Text("Clear").tag(AppearancePreset.clear)
                    Text("Balanced").tag(AppearancePreset.balanced)
                    Text("Solid").tag(AppearancePreset.solid)
                }
                .pickerStyle(.segmented)
                .labelsHidden()
                .frame(maxWidth: .infinity)
            }
        }
        .formStyle(.grouped)
        .scrollContentBackground(.hidden)
        .font(.system(size: 12))
        .onChange(of: workspace.windowAppearance) { _, newValue in
            updateAppearance(newValue)
        }
        .onChange(of: workspace.backgroundAlpha) { _, newValue in
            updateOpacity(newValue)
        }
        .onChange(of: workspace.vibrancyMaterial) { _, newValue in
            updateMaterial(newValue)
        }
    }

    // MARK: - Update Logic

    private func updateAppearance(_ newValue: String) {
        workspace.saveAppearance()

        let nsAppearance: NSAppearance?
        switch newValue {
        case "light": nsAppearance = NSAppearance(named: .aqua)
        case "dark": nsAppearance = NSAppearance(named: .darkAqua)
        default: nsAppearance = nil
        }

        InspectorAppearanceCallbacks.onAppearanceModeChanged?(newValue, nsAppearance)
    }

    private func updateOpacity(_ newValue: Double) {
        workspace.saveAppearance()
        InspectorAppearanceCallbacks.onOpacityChanged?(newValue)
    }

    private func updateMaterial(_ newValue: VibrancyMaterial) {
        workspace.saveAppearance()
        InspectorAppearanceCallbacks.onMaterialChanged?(newValue)
    }
}

private enum AppearancePreset: String, CaseIterable, Identifiable {
    case clear = "Clear"
    case balanced = "Balanced"
    case solid = "Solid"

    var id: String { rawValue }
}
#else
@available(iOS 26.0, *)
struct InspectorAppearanceView: View {
    @Bindable var workspace: HyaloWorkspaceState

    var body: some View {
        Form {
            if !workspace.currentThemeName.isEmpty {
                Section("Theme") {
                    LabeledContent("Current") {
                        Text(workspace.currentThemeName)
                            .font(.system(size: 11, design: .monospaced))
                            .foregroundStyle(.secondary)
                    }
                }
            }

            Section("Window") {
                LabeledContent("Appearance") {
                    Picker("Appearance", selection: $workspace.windowAppearance) {
                        Label("Auto", systemImage: "circle.lefthalf.filled").tag("auto")
                        Label("Light", systemImage: "sun.max.fill").tag("light")
                        Label("Dark", systemImage: "moon.fill").tag("dark")
                    }
                    .pickerStyle(.segmented)
                    .labelsHidden()
                }

                LabeledContent("Opacity") {
                    HStack(spacing: 8) {
                        Slider(value: $workspace.backgroundAlpha, in: 0...1)
                        Text("\(Int(workspace.backgroundAlpha * 100))%")
                            .monospacedDigit()
                            .foregroundStyle(.secondary)
                            .frame(width: 35, alignment: .trailing)
                    }
                }
            }
        }
        .formStyle(.grouped)
        .scrollContentBackground(.hidden)
        .font(.system(size: 12))
        .onChange(of: workspace.windowAppearance) { _, newValue in
            workspace.saveAppearance()
            InspectorAppearanceCallbacksiOS.onAppearanceModeChanged?(newValue)
        }
        .onChange(of: workspace.backgroundAlpha) { _, _ in
            workspace.saveAppearance()
        }
    }
}

// MARK: - iOS Appearance Callbacks (wired from HyaloiOS at startup)

/// Static closures injected by HyaloiOS at module init time.
/// HyaloShared cannot import HyaloiOS (circular), so the iOS-specific
/// side-effects (DispatchRouter commands) are invoked via these callbacks.
@available(iOS 26.0, *)
public enum InspectorAppearanceCallbacksiOS {
    /// Called when the user changes light/dark/auto appearance mode.
    /// Receives the new mode string ("light", "dark", "auto").
    public static var onAppearanceModeChanged: ((String) -> Void)?
}
#endif
