// InspectorAppearanceView.swift - Appearance settings in the inspector
// Target: macOS 26 Tahoe with Liquid Glass design
// Uses standard Form and LabeledContent to match FileInspectorView consistency.

import SwiftUI

@available(macOS 26.0, *)
struct InspectorAppearanceView: View {
    var body: some View {
        if let workspace = HyaloModule.activeWorkspace {
            InspectorAppearanceContent(workspace: workspace)
        } else {
            HyaloContentUnavailableView(
                "No Workspace",
                description: "Workspace not initialized",
                systemImage: "paintbrush"
            )
        }
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
        
        for controller in HyaloModule.allControllers {
            controller.applyAppearance(nsAppearance)
        }
        
        for ws in HyaloModule.allWorkspaces {
            ws.windowAppearance = newValue
        }
        
        HyaloModule.onAppearanceModeChanged?(newValue)
        HyaloModule.wakeEmacs()
    }

    private func updateOpacity(_ newValue: Double) {
        workspace.saveAppearance()
        
        for ws in HyaloModule.allWorkspaces {
            ws.backgroundAlpha = newValue
        }
        
        let fringeAlpha = min(Double(newValue) + HyaloModule.fringeAlphaOffset, 1.0)
        HyaloModule.setFringeAlpha(fringeAlpha)
        
        HyaloModule.onOpacityChanged?(Double(newValue))
    }

    private func updateMaterial(_ newValue: VibrancyMaterial) {
        workspace.saveAppearance()
        
        for ws in HyaloModule.allWorkspaces {
            ws.vibrancyMaterial = newValue
        }
    }
}

private enum AppearancePreset: String, CaseIterable, Identifiable {
    case clear = "Clear"
    case balanced = "Balanced"
    case solid = "Solid"
    
    var id: String { rawValue }
}
