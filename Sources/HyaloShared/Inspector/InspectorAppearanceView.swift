import SwiftUI
import KelyphosKit

// MARK: - Appearance Callbacks (wired from HyaloMac at startup)

/// Static closures injected by HyaloMac at module init time.
/// HyaloShared cannot import HyaloMac (circular), so the macOS-specific
/// side-effects (NSAppearance, fringe alpha, Emacs channels) are invoked
/// via these callbacks instead of a direct dependency.
@available(macOS 26.0, iOS 26.0, *)
public enum InspectorAppearanceCallbacks {
    /// Called when the user changes light/dark/auto appearance mode.
    /// Receives the new mode string ("light", "dark", "auto").
    public static var onAppearanceModeChanged: ((String) -> Void)?
    /// Called when the user changes the background opacity slider.
    public static var onOpacityChanged: ((Double) -> Void)?
    /// Called when the user changes the vibrancy material.
    public static var onMaterialChanged: ((VibrancyMaterial) -> Void)?
}

// MARK: - View

@available(macOS 26.0, iOS 26.0, *)
struct InspectorAppearanceView: View {
    @Environment(\.kelyphosShellState) private var shellState
    @Environment(HyaloWorkspaceState.self) private var workspace

    var body: some View {
        if let shellState {
            InspectorAppearanceContent(shellState: shellState, workspace: workspace)
        }
    }
}

// MARK: - Content (bindable shell state)

@available(macOS 26.0, iOS 26.0, *)
private struct InspectorAppearanceContent: View {
    @Bindable var shellState: KelyphosShellState
    let workspace: HyaloWorkspaceState

    private var presetBinding: Binding<AppearancePreset?> {
        Binding(
            get: {
                if shellState.backgroundAlpha == 0.0 && shellState.vibrancyMaterial == .ultraThin { return .clear }
                if shellState.backgroundAlpha == 0.5 && shellState.vibrancyMaterial == .thin { return .balanced }
                if shellState.backgroundAlpha == 1.0 && shellState.vibrancyMaterial == .none { return .solid }
                return nil
            },
            set: { newValue in
                guard let preset = newValue else { return }
                switch preset {
                case .clear:
                    shellState.backgroundAlpha = 0.0
                    shellState.vibrancyMaterial = .ultraThin
                case .balanced:
                    shellState.backgroundAlpha = 0.5
                    shellState.vibrancyMaterial = .thin
                case .solid:
                    shellState.backgroundAlpha = 1.0
                    shellState.vibrancyMaterial = .none
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
                    Picker("Appearance", selection: $shellState.windowAppearance) {
                        Label("Auto", systemImage: "circle.lefthalf.filled").tag("auto")
                        Label("Light", systemImage: "sun.max.fill").tag("light")
                        Label("Dark", systemImage: "moon.fill").tag("dark")
                    }
                    .pickerStyle(.segmented)
                    .labelsHidden()
                }

                LabeledContent("Opacity") {
                    HStack(spacing: 8) {
                        Slider(value: $shellState.backgroundAlpha, in: 0...1)
                        Text("\(Int(shellState.backgroundAlpha * 100))%")
                            .monospacedDigit()
                            .foregroundStyle(.secondary)
                            .frame(width: 35, alignment: .trailing)
                    }
                }

                LabeledContent("Vibrancy") {
                    Picker("Material", selection: $shellState.vibrancyMaterial) {
                        ForEach(VibrancyMaterial.allCases, id: \.self) { material in
                            Text(material.rawValue.capitalized).tag(material)
                        }
                    }
                    .labelsHidden()
                    .frame(maxWidth: .infinity, alignment: .leading)
                }
            }

            Section("Presets") {
                Picker("Presets", selection: presetBinding) {
                    Text("Clear").tag(AppearancePreset.clear as AppearancePreset?)
                    Text("Balanced").tag(AppearancePreset.balanced as AppearancePreset?)
                    Text("Solid").tag(AppearancePreset.solid as AppearancePreset?)
                }
                .pickerStyle(.segmented)
                .labelsHidden()
                .frame(maxWidth: .infinity)
            }
        }
        .formStyle(.grouped)
        .scrollContentBackground(.hidden)
        .font(.system(size: 12))
        .onChange(of: shellState.windowAppearance) { _, newValue in
            shellState.saveAppearance()
            #if os(macOS)
            let nsAppearance: NSAppearance?
            switch newValue {
            case "light": nsAppearance = NSAppearance(named: .aqua)
            case "dark": nsAppearance = NSAppearance(named: .darkAqua)
            default: nsAppearance = nil
            }
            NSApp.appearance = nsAppearance
            #endif
            InspectorAppearanceCallbacks.onAppearanceModeChanged?(newValue)
        }
        .onChange(of: shellState.backgroundAlpha) { _, newValue in
            shellState.saveAppearance()
            InspectorAppearanceCallbacks.onOpacityChanged?(newValue)
        }
        .onChange(of: shellState.vibrancyMaterial) { _, newValue in
            shellState.saveAppearance()
            InspectorAppearanceCallbacks.onMaterialChanged?(newValue)
        }
    }
}

// MARK: - Presets

private enum AppearancePreset: String, CaseIterable, Identifiable {
    case clear = "Clear"
    case balanced = "Balanced"
    case solid = "Solid"

    var id: String { rawValue }
}
