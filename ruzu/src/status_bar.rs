// SPDX-License-Identifier: GPL-3.0-or-later
//
// Bottom status bar — counterpart of the permanent status widgets upstream
// `GMainWindow` builds in `main.cpp` (`renderer_status_button`,
// `gpu_accuracy_button`, `dock_status_button`, `filter_status_button`,
// `aa_status_button`, and `volume_button`), together with the
// `UpdateAPIText` / `UpdateGPUAccuracyButton` / `UpdateFilterText` /
// `UpdateAAText` / `UpdateDockedButton` / `UpdateVolumeUI` refreshers and the
// `OnToggle*` click handlers.
//
// Each button shows a `Settings` value and *writes it back* when clicked,
// cycling through the same sequence upstream does. The colours come from
// upstream's own stylesheet — see [`css`] below.

use std::cell::RefCell;
use std::rc::Rc;

use gtk::prelude::*;

use common::settings;
use common::settings_enums::{
    AntiAliasing, ConsoleMode, GpuAccuracy, RendererBackend, ScalingFilter, ShaderBackend,
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
    /// Invoked after any button changes a setting, so the owner can react
    /// (upstream calls `system->ApplySettings()` from the same handlers).
    on_changed: RefCell<Option<Box<dyn Fn()>>>,
}

impl StatusBar {
    pub fn new() -> Rc<Self> {
        install_css();

        let root = gtk::Box::new(gtk::Orientation::Horizontal, 2);
        root.add_css_class("ruzu-statusbar");
        root.set_margin_start(4);
        root.set_margin_end(4);

        // Left-aligned status buttons. Upstream inserts each with
        // `insertPermanentWidget(0, …)`, so the *last* inserted ends up
        // leftmost: renderer, accuracy, dock, filter, AA, volume.
        let renderer = status_button(class::RENDERER);
        let accuracy = status_button(class::GPU);
        let dock = status_button(class::DOCKING);
        let filter = status_button(class::TOGGLABLE);
        let aa = status_button(class::TOGGLABLE);
        let volume = status_button(class::TOGGLABLE);
        for b in [&renderer, &accuracy, &dock, &filter, &aa, &volume] {
            root.append(b);
        }

        // Right side: message label (upstream `message_label`, stretch) then the
        // firmware/room widgets (placeholder for now).
        let message = gtk::Label::new(None);
        message.set_hexpand(true);
        root.append(&message);

        let bar = Rc::new(Self {
            root,
            renderer,
            accuracy,
            dock,
            filter,
            aa,
            volume,
            on_changed: RefCell::new(None),
        });

        bar.connect_actions();
        bar.refresh();
        bar
    }

    /// Register a callback fired whenever a status button changes a setting.
    pub fn connect_changed(&self, f: impl Fn() + 'static) {
        *self.on_changed.borrow_mut() = Some(Box::new(f));
    }

    /// The widget to place at the bottom of the window.
    pub fn widget(&self) -> &gtk::Box {
        &self.root
    }

    /// Wire each button to the upstream `OnToggle*` behaviour.
    fn connect_actions(self: &Rc<Self>) {
        macro_rules! on_click {
            ($button:expr, $handler:ident) => {{
                let bar = Rc::clone(self);
                $button.connect_clicked(move |_| {
                    log::debug!("status bar: {} clicked", stringify!($handler));
                    bar.$handler();
                    bar.refresh();
                    if let Some(f) = bar.on_changed.borrow().as_ref() {
                        f();
                    }
                });
            }};
        }

        on_click!(self.renderer, on_toggle_graphics_api);
        on_click!(self.accuracy, on_toggle_gpu_accuracy);
        on_click!(self.dock, on_toggle_docked_mode);
        on_click!(self.filter, on_toggle_adapting_filter);
        on_click!(self.aa, on_toggle_anti_aliasing);
        on_click!(self.volume, on_toggle_mute);
    }

    /// Upstream `GMainWindow::OnToggleGraphicsAPI`: Vulkan ⇄ OpenGL.
    fn on_toggle_graphics_api(&self) {
        let mut values = settings::values_mut();
        let api = if *values.renderer_backend.get_value() != RendererBackend::Vulkan {
            RendererBackend::Vulkan
        } else {
            // Upstream falls back to `Null` where OpenGL is not compiled in;
            // ruzu always builds both backends.
            RendererBackend::OpenGL
        };
        values.renderer_backend.set_value(api);
    }

    /// Upstream `GMainWindow::OnToggleGpuAccuracy`: High ⇄ Normal.
    ///
    /// Note this is *not* a cycle through every value — upstream deliberately
    /// bounces between High and Normal, and treats Extreme as "go to High".
    fn on_toggle_gpu_accuracy(&self) {
        let mut values = settings::values_mut();
        let accuracy = match *values.gpu_accuracy.get_value() {
            GpuAccuracy::High => GpuAccuracy::Normal,
            GpuAccuracy::Normal | GpuAccuracy::Extreme => GpuAccuracy::High,
        };
        values.gpu_accuracy.set_value(accuracy);
    }

    /// Upstream `GMainWindow::OnToggleDockedMode`.
    ///
    /// Upstream additionally disconnects a handheld controller and warns, which
    /// needs `HIDCore`; that is not reachable from the launcher yet, so only the
    /// console-mode flip is performed here.
    fn on_toggle_docked_mode(&self) {
        let mut values = settings::values_mut();
        let mode = match *values.use_docked_mode.get_value() {
            ConsoleMode::Docked => ConsoleMode::Handheld,
            ConsoleMode::Handheld => ConsoleMode::Docked,
        };
        values.use_docked_mode.set_value(mode);
    }

    /// Upstream `GMainWindow::OnToggleAdaptingFilter`: advance one step,
    /// wrapping past `MaxEnum` back to `NearestNeighbor`.
    fn on_toggle_adapting_filter(&self) {
        let mut values = settings::values_mut();
        let next = next_wrapping(
            *values.scaling_filter.get_value() as u32,
            ScalingFilter::MaxEnum as u32,
        );
        if let Some(filter) = ScalingFilter::from_u32(next) {
            values.scaling_filter.set_value(filter);
        }
    }

    /// Upstream's `aa_status_button` click handler: advance one step, wrapping
    /// past `MaxEnum` back to `None`.
    fn on_toggle_anti_aliasing(&self) {
        let mut values = settings::values_mut();
        let next = next_wrapping(
            *values.anti_aliasing.get_value() as u32,
            AntiAliasing::MaxEnum as u32,
        );
        if let Some(aa) = AntiAliasing::from_u32(next) {
            values.anti_aliasing.set_value(aa);
        }
    }

    /// Upstream exposes mute on the volume button's context menu; a plain click
    /// opens a volume slider popup. Without that popup ported yet, the click
    /// toggles mute, which is the one action the button's own checked state
    /// already reflects (`UpdateVolumeUI` shows "VOLUME: MUTE" when muted).
    fn on_toggle_mute(&self) {
        let mut values = settings::values_mut();
        let muted = *values.audio_muted.get_value();
        values.audio_muted.set_value(!muted);
    }

    /// Re-read the settings and update every label and colour state — upstream
    /// `UpdateStatusButtons` plus the individual `Update*` refreshers.
    pub fn refresh(&self) {
        let values = settings::values();

        // `UpdateAPIText`: OpenGL additionally shows the shader backend.
        let backend = *values.renderer_backend.get_value();
        let renderer = match backend {
            RendererBackend::OpenGL => {
                let shader = match *values.shader_backend.get_value() {
                    ShaderBackend::Glsl => "GLSL",
                    ShaderBackend::Glasm => "GLASM",
                    ShaderBackend::SpirV => "SPIRV",
                };
                format!("OPENGL {shader}")
            }
            RendererBackend::Vulkan => "VULKAN".to_string(),
            RendererBackend::Null => "NULL".to_string(),
        };
        self.renderer.set_label(&renderer);
        // `renderer_status_button->setChecked(api == Vulkan)` — checked renders
        // orange, unchecked blue.
        set_checked(&self.renderer, backend == RendererBackend::Vulkan);

        // `UpdateGPUAccuracyButton`.
        let accuracy = *values.gpu_accuracy.get_value();
        self.accuracy.set_label(match accuracy {
            GpuAccuracy::Normal => "NORMAL",
            GpuAccuracy::High => "HIGH",
            GpuAccuracy::Extreme => "EXTREME",
        });
        set_checked(&self.accuracy, accuracy != GpuAccuracy::Normal);

        // `UpdateDockedButton`.
        let console_mode = *values.use_docked_mode.get_value();
        self.dock.set_label(match console_mode {
            ConsoleMode::Docked => "DOCKED",
            ConsoleMode::Handheld => "HANDHELD",
        });
        set_checked(&self.dock, console_mode == ConsoleMode::Docked);

        // `UpdateFilterText`: FSR gets a short label of its own.
        self.filter
            .set_label(match *values.scaling_filter.get_value() {
                ScalingFilter::NearestNeighbor => "NEAREST",
                ScalingFilter::Bilinear => "BILINEAR",
                ScalingFilter::Bicubic => "BICUBIC",
                ScalingFilter::Gaussian => "GAUSSIAN",
                ScalingFilter::ScaleForce => "SCALEFORCE",
                ScalingFilter::Fsr => "FSR",
                ScalingFilter::MaxEnum => "BILINEAR",
            });
        // Upstream keeps the filter button permanently checked.
        set_checked(&self.filter, true);

        // `UpdateAAText`.
        self.aa.set_label(match *values.anti_aliasing.get_value() {
            AntiAliasing::None => "NO AA",
            AntiAliasing::Fxaa => "FXAA",
            AntiAliasing::Smaa => "SMAA",
            AntiAliasing::MaxEnum => "NO AA",
        });
        set_checked(&self.aa, true);

        // `UpdateVolumeUI`.
        let muted = *values.audio_muted.get_value();
        if muted {
            self.volume.set_label("VOLUME: MUTE");
        } else {
            self.volume
                .set_label(&format!("VOLUME: {}%", values.volume.get_value()));
        }
        set_checked(&self.volume, !muted);
    }
}

/// Next enum discriminant, wrapping back to 0 once `max` is reached.
///
/// Mirrors upstream's `static_cast<Enum>(static_cast<u32>(value) + 1)` followed
/// by a `== MaxEnum` reset, used by both the filter and AA buttons.
fn next_wrapping(current: u32, max: u32) -> u32 {
    let next = current + 1;
    if next >= max {
        0
    } else {
        next
    }
}

/// CSS classes standing in for upstream's `objectName`s, which is how its
/// stylesheet targets each button.
mod class {
    pub const TOGGLABLE: &str = "ruzu-status-togglable";
    pub const RENDERER: &str = "ruzu-status-renderer";
    pub const GPU: &str = "ruzu-status-gpu";
    pub const DOCKING: &str = "ruzu-status-docking";
}

/// Qt's `:checked` pseudo-state, which the stylesheet colours against.
const CHECKED_CLASS: &str = "ruzu-status-checked";

fn set_checked(button: &gtk::Button, checked: bool) {
    if checked {
        button.add_css_class(CHECKED_CLASS);
    } else {
        button.remove_css_class(CHECKED_CLASS);
    }
}

/// A flat status-bar button, matching yuzu's `QPushButton` status widgets
/// (borderless, compact).
fn status_button(class: &str) -> gtk::Button {
    let button = gtk::Button::new();
    button.add_css_class("flat");
    button.add_css_class(class);
    button.set_has_frame(false);
    button.set_focus_on_click(false);
    button
}

/// Install the status-bar styling once.
///
/// The colours are upstream's, from
/// `zuyu/dist/qt_themes/default/style.qss` — the stylesheet yuzu's default
/// theme loads. Qt's `:checked` / `:!checked` pseudo-states become the
/// [`CHECKED_CLASS`] marker here:
///
/// ```qss
/// QPushButton#RendererStatusBarButton:checked  { color: #e85c00; }  /* Vulkan */
/// QPushButton#RendererStatusBarButton:!checked { color: #0066ff; }  /* OpenGL */
/// QPushButton#GPUStatusBarButton:checked       { color: #b06020; }
/// QPushButton#GPUStatusBarButton:!checked      { color: #109010; }
/// QPushButton#TogglableStatusBarButton         { color: #959595; }
/// QPushButton#TogglableStatusBarButton:checked { color: #000000; }
/// QPushButton#DockingStatusBarButton           { color: #000000; }
/// ```
///
/// Note the docking button has no `:checked` rule upstream — it is always
/// rendered in the base colour, whichever console mode is active.
fn install_css() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let Some(display) = gtk::gdk::Display::default() else {
            return;
        };
        let provider = gtk::CssProvider::new();
        provider.load_from_data(&format!(
            ".ruzu-statusbar {{ padding: 0 2px; min-height: 0; }}\
             .ruzu-statusbar button {{ padding: 2px 6px; min-height: 0; min-width: 0;\
                 border: 1px solid transparent; box-shadow: none; background: none;\
                 font-size: 11px; }}\
             .ruzu-statusbar button:hover {{ border: 1px solid #76797C; }}\
             .ruzu-statusbar label {{ font-size: 11px; }}\
             .{togglable} {{ color: #959595; }}\
             .{togglable}.{checked} {{ color: #000000; }}\
             .{renderer} {{ color: #0066ff; }}\
             .{renderer}.{checked} {{ color: #e85c00; }}\
             .{gpu} {{ color: #109010; }}\
             .{gpu}.{checked} {{ color: #b06020; }}\
             .{docking} {{ color: #000000; }}",
            togglable = class::TOGGLABLE,
            renderer = class::RENDERER,
            gpu = class::GPU,
            docking = class::DOCKING,
            checked = CHECKED_CLASS,
        ));
        gtk::style_context_add_provider_for_display(
            &display,
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filter_cycle_wraps_past_the_last_real_value() {
        // ScalingFilter: NearestNeighbor..Fsr, then MaxEnum is the sentinel.
        let fsr = ScalingFilter::Fsr as u32;
        let max = ScalingFilter::MaxEnum as u32;
        assert_eq!(next_wrapping(fsr, max), 0);
        assert_eq!(
            ScalingFilter::from_u32(next_wrapping(fsr, max)),
            Some(ScalingFilter::NearestNeighbor)
        );
    }

    #[test]
    fn filter_cycle_advances_one_step() {
        let max = ScalingFilter::MaxEnum as u32;
        assert_eq!(
            ScalingFilter::from_u32(next_wrapping(ScalingFilter::NearestNeighbor as u32, max)),
            Some(ScalingFilter::Bilinear)
        );
    }

    #[test]
    fn anti_aliasing_cycle_wraps_to_none() {
        let max = AntiAliasing::MaxEnum as u32;
        assert_eq!(
            AntiAliasing::from_u32(next_wrapping(AntiAliasing::Smaa as u32, max)),
            Some(AntiAliasing::None)
        );
    }

    #[test]
    fn cycles_never_land_on_the_sentinel() {
        // Selecting MaxEnum would render "BILINEAR"/"NO AA" while storing an
        // invalid value, so the wrap must skip it from every starting point.
        let max = ScalingFilter::MaxEnum as u32;
        for start in 0..max {
            assert_ne!(next_wrapping(start, max), max);
        }
        let max = AntiAliasing::MaxEnum as u32;
        for start in 0..max {
            assert_ne!(next_wrapping(start, max), max);
        }
    }
}
