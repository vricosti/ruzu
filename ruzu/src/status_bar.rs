// SPDX-License-Identifier: GPL-3.0-or-later
//
// Bottom status bar — counterpart of the permanent status widgets upstream
// `GMainWindow` builds in `main.cpp` (`renderer_status_button`,
// `gpu_accuracy_button`, `dock_status_button`, `filter_status_button`,
// `aa_status_button`, and the volume button). Each shows the current value of a
// `Settings` field, formatted the same way yuzu formats them.
//
// Presentation only for now (labels reflect settings); the click-to-cycle
// behaviour upstream wires on these buttons is a later step.

use gtk::prelude::*;

use common::settings;
use common::settings_enums::{
    AntiAliasing, ConsoleMode, GpuAccuracy, RendererBackend, ScalingFilter,
};

/// The status bar and handles to the value buttons so they can be refreshed.
pub struct StatusBar {
    root: gtk::Box,
    renderer: gtk::Button,
    accuracy: gtk::Button,
    dock: gtk::Button,
    filter: gtk::Button,
    aa: gtk::Button,
    volume: gtk::Button,
}

impl StatusBar {
    pub fn new() -> Self {
        install_css();

        let root = gtk::Box::new(gtk::Orientation::Horizontal, 2);
        root.add_css_class("ruzu-statusbar");
        root.set_margin_start(4);
        root.set_margin_end(4);

        // Left-aligned status buttons (order mirrors yuzu's bottom bar:
        // renderer, accuracy, dock, filter, AA, volume).
        let renderer = status_button();
        let accuracy = status_button();
        let dock = status_button();
        let filter = status_button();
        let aa = status_button();
        let volume = status_button();
        for b in [&renderer, &accuracy, &dock, &filter, &aa, &volume] {
            root.append(b);
        }

        // Right side: message label (upstream `message_label`, stretch) then the
        // firmware/room widgets (placeholder for now).
        let message = gtk::Label::new(None);
        message.set_hexpand(true);
        root.append(&message);

        let bar = Self {
            root,
            renderer,
            accuracy,
            dock,
            filter,
            aa,
            volume,
        };
        bar.refresh();
        bar
    }

    /// The widget to place at the bottom of the window.
    pub fn widget(&self) -> &gtk::Box {
        &self.root
    }

    /// Re-read the settings values and update the button labels. Upstream calls
    /// the equivalent from `UpdateStatusButtons` on config/emulation changes.
    pub fn refresh(&self) {
        let values = settings::values();

        let renderer = match *values.renderer_backend.get_value() {
            RendererBackend::OpenGL => "OPENGL",
            RendererBackend::Vulkan => "VULKAN",
            RendererBackend::Null => "NULL",
        };
        self.renderer.set_label(renderer);

        let accuracy = match *values.gpu_accuracy.get_value() {
            GpuAccuracy::Normal => "NORMAL",
            GpuAccuracy::High => "HIGH",
            GpuAccuracy::Extreme => "EXTREME",
        };
        self.accuracy.set_label(accuracy);

        let dock = match *values.use_docked_mode.get_value() {
            ConsoleMode::Docked => "DOCKED",
            ConsoleMode::Handheld => "HANDHELD",
        };
        self.dock.set_label(dock);

        let filter = match *values.scaling_filter.get_value() {
            ScalingFilter::NearestNeighbor => "NEAREST",
            ScalingFilter::Bilinear => "BILINEAR",
            ScalingFilter::Bicubic => "BICUBIC",
            ScalingFilter::Gaussian => "GAUSSIAN",
            ScalingFilter::ScaleForce => "SCALEFORCE",
            ScalingFilter::Fsr => "FSR",
            ScalingFilter::MaxEnum => "BILINEAR",
        };
        self.filter.set_label(filter);

        let aa = match *values.anti_aliasing.get_value() {
            AntiAliasing::None => "NO AA",
            AntiAliasing::Fxaa => "FXAA",
            AntiAliasing::Smaa => "SMAA",
            AntiAliasing::MaxEnum => "NO AA",
        };
        self.aa.set_label(aa);

        let volume = *values.volume.get_value();
        self.volume.set_label(&format!("VOLUME: {volume}%"));
    }
}

impl Default for StatusBar {
    fn default() -> Self {
        Self::new()
    }
}

/// A flat status-bar button, matching yuzu's `QPushButton` status widgets
/// (borderless, compact).
fn status_button() -> gtk::Button {
    let button = gtk::Button::new();
    button.add_css_class("flat");
    button.set_has_frame(false);
    button.set_focus_on_click(false);
    button
}

/// Install the compact status-bar styling once. yuzu's status bar is a thin
/// strip with small text and no button chrome; GTK buttons are chunky by
/// default, so tighten padding and font size.
fn install_css() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let Some(display) = gtk::gdk::Display::default() else {
            return;
        };
        let provider = gtk::CssProvider::new();
        provider.load_from_data(
            ".ruzu-statusbar { padding: 0 2px; min-height: 0; } \
             .ruzu-statusbar button { padding: 2px 6px; min-height: 0; min-width: 0; \
                border: none; box-shadow: none; background: none; font-size: 11px; } \
             .ruzu-statusbar label { font-size: 11px; }",
        );
        gtk::style_context_add_provider_for_display(
            &display,
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    });
}
