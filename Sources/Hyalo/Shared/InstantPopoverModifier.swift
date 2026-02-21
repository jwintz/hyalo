// InstantPopoverModifier.swift - Zero-animation NSPopover via SwiftUI
// Target: macOS 26 Tahoe
//
// Ported from CodeEdit's InstantPopoverModifier pattern.
// Standard SwiftUI .popover has a 200ms fade animation that makes
// dropdown menus feel sluggish.  This presents an NSPopover directly,
// with animates = false and .semitransient behavior.
//
// WARNING: Views inside this popover MUST be dismissed by negating the
// isPresented binding.  Using SwiftUI's dismiss environment value causes
// a crash (FB16221871 / rdar://FB16221871).

import AppKit
import SwiftUI

// MARK: - Modifier

struct InstantPopoverModifier<PopoverContent: View>: ViewModifier {
    @Binding var isPresented: Bool
    let arrowEdge: Edge
    let popoverContent: PopoverContent

    func body(content: Content) -> some View {
        content
            .background(
                InstantPopoverPresenter(
                    isPresented: $isPresented,
                    arrowEdge: arrowEdge,
                    contentView: popoverContent
                )
            )
    }
}

// MARK: - NSViewRepresentable presenter

struct InstantPopoverPresenter<ContentView: View>: NSViewRepresentable {
    @Binding var isPresented: Bool
    let arrowEdge: Edge
    let contentView: ContentView

    func makeNSView(context: Context) -> NSView { NSView() }

    func updateNSView(_ nsView: NSView, context: Context) {
        if isPresented && context.coordinator.popover == nil {
            let popover = NSPopover()
            popover.animates = false
            let hostingController = NSHostingController(rootView: contentView)
            hostingController.view.layoutSubtreeIfNeeded()
            popover.contentSize = hostingController.view.fittingSize
            popover.contentViewController = hostingController
            popover.delegate = context.coordinator
            popover.behavior = .semitransient
            popover.show(
                relativeTo: nsView.bounds,
                of: nsView,
                preferredEdge: arrowEdge.nsRectEdge
            )
            context.coordinator.popover = popover
            if let window = nsView.window {
                context.coordinator.startObservingWindow(window)
            }
        } else if !isPresented, let popover = context.coordinator.popover {
            popover.close()
            context.coordinator.popover = nil
        }
    }

    func makeCoordinator() -> Coordinator { Coordinator(isPresented: $isPresented) }

    final class Coordinator: NSObject, NSPopoverDelegate {
        @Binding var isPresented: Bool
        var popover: NSPopover?
        private var windowObserver: NSObjectProtocol?

        init(isPresented: Binding<Bool>) {
            _isPresented = isPresented
        }

        func startObservingWindow(_ window: NSWindow) {
            windowObserver = NotificationCenter.default.addObserver(
                forName: NSWindow.didResignKeyNotification,
                object: window,
                queue: .main
            ) { [weak self] _ in
                DispatchQueue.main.async {
                    self?.isPresented = false
                    self?.popover?.close()
                }
            }
        }

        func popoverWillClose(_ notification: Notification) {
            DispatchQueue.main.async { self.isPresented = false }
        }

        func popoverDidClose(_ notification: Notification) {
            popover = nil
            if let obs = windowObserver {
                NotificationCenter.default.removeObserver(obs)
                windowObserver = nil
            }
        }
    }
}

// MARK: - Edge → NSRectEdge

private extension Edge {
    var nsRectEdge: NSRectEdge {
        switch self {
        case .top:      return .minY
        case .leading:  return .minX
        case .bottom:   return .maxY
        case .trailing: return .maxX
        }
    }
}

// MARK: - View extension

extension View {
    /// Present a zero-animation popover attached to this view.
    ///
    /// - Warning: Dismiss by negating `isPresented`.  Do NOT use
    ///   SwiftUI's `dismiss` environment value — it will crash.
    func instantPopover<Content: View>(
        isPresented: Binding<Bool>,
        arrowEdge: Edge = .bottom,
        @ViewBuilder content: @escaping () -> Content
    ) -> some View {
        modifier(
            InstantPopoverModifier(
                isPresented: isPresented,
                arrowEdge: arrowEdge,
                popoverContent: InstantPopoverContainer(content: content)
            )
        )
    }
}

// MARK: - Container (Tahoe-aware padding + shape)

struct InstantPopoverContainer<ContentView: View>: View {
    let content: () -> ContentView

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            content()
        }
        .font(.subheadline)
        .padding(13)
        .frame(minWidth: 215)
    }
}

// MARK: - Dropdown item style modifier

struct DropdownItemStyleModifier: ViewModifier {
    @State private var isHovering = false

    func body(content: Content) -> some View {
        content
            .padding(.vertical, 4)
            .padding(.horizontal, 8)
            .background(
                isHovering
                    ? AnyView(RoundedRectangle(cornerRadius: 6).fill(Color.accentColor))
                    : AnyView(Color.clear)
            )
            .foregroundStyle(isHovering ? Color.white : Color.primary)
            .clipShape(RoundedRectangle(cornerRadius: 6))
            .onHover { isHovering = $0 }
    }
}

extension View {
    func dropdownItemStyle() -> some View {
        modifier(DropdownItemStyleModifier())
    }
}
