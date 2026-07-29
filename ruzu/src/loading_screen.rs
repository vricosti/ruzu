// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of the upstream `LoadingScreen` widget in
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/loading_screen.{h,cpp,ui}`.
//
// Shown while the emulator loads disk resources (chiefly shader compilation).
// It displays the title-provided logo and banner, a stage label, a progress
// bar, and an estimated-time label. The progress feed is
// `OnLoadProgress(stage, value, total)`, driven by `VideoCore`'s
// `DiskResourceLoadCallback`.

use std::cell::RefCell;
use std::time::{Duration, Instant, SystemTime};

use gtk::gdk_pixbuf::prelude::*;
use gtk::glib;
use gtk::prelude::*;

/// Disk-resource load stage. Mirrors
/// `video_core::rasterizer_interface::LoadCallbackStage`
/// (upstream `VideoCore::LoadCallbackStage`). Duplicated here so the launcher's
/// UI does not pull the whole `video_core` crate; the boot layer maps between
/// the two.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoadStage {
    Prepare,
    Build,
    Complete,
}

/// Mutable progress-tracking state, matching the private members of upstream
/// `LoadingScreen` used by `OnLoadProgress`.
struct ProgressState {
    previous_total: usize,
    previous_stage: LoadStage,
    previous_time: Instant,
    // Newly generated shaders are appended, so compilation starts fast and ends
    // slow. These fields detect the slowdown to produce an ETA, exactly as
    // upstream does.
    slow_shader_compile_start: bool,
    slow_shader_start: Instant,
    slow_shader_first_value: usize,
}

/// The loading-screen widget and its progress state.
pub struct LoadingScreen {
    root: gtk::Overlay,
    fade_parent: gtk::Overlay,
    logo: gtk::Picture,
    banner: gtk::Image,
    stage: gtk::Label,
    progress_bar: gtk::ProgressBar,
    value: gtk::Label,
    state: RefCell<ProgressState>,
    pulse_source: RefCell<Option<glib::SourceId>>,
    banner_animation_source: RefCell<Option<glib::SourceId>>,
}

impl LoadingScreen {
    /// Build the widget tree. Mirrors `loading_screen.ui`: logo at top-left,
    /// banner at bottom-right, and centered stage/progress/estimate widgets.
    pub fn new() -> Self {
        install_css();

        let root = gtk::Overlay::new();
        root.add_css_class("ruzu-loading-screen");
        root.set_hexpand(true);
        root.set_vexpand(true);

        // Upstream fades this inner widget while retaining the root's black
        // background, producing a fade to black rather than to the parent.
        let fade_parent = gtk::Overlay::new();
        let backdrop = gtk::Box::new(gtk::Orientation::Vertical, 0);
        backdrop.set_hexpand(true);
        backdrop.set_vexpand(true);
        fade_parent.set_child(Some(&backdrop));
        root.set_child(Some(&fade_parent));

        let logo = gtk::Picture::new();
        logo.set_content_fit(gtk::ContentFit::Contain);
        logo.set_can_shrink(true);
        logo.set_halign(gtk::Align::Start);
        logo.set_valign(gtk::Align::Start);
        logo.set_margin_start(30);
        logo.set_margin_top(30);
        logo.set_visible(false);
        fade_parent.add_overlay(&logo);

        let column = gtk::Box::new(gtk::Orientation::Vertical, 15);
        column.add_css_class("ruzu-loading-column");
        column.set_halign(gtk::Align::Center);
        column.set_valign(gtk::Align::Center);

        let stage = gtk::Label::new(Some("Loading..."));
        stage.add_css_class("ruzu-loading-stage");
        stage.set_halign(gtk::Align::Center);
        column.append(&stage);

        let progress_bar = gtk::ProgressBar::new();
        progress_bar.add_css_class("ruzu-loading-progress");
        progress_bar.add_css_class("prepare");
        progress_bar.set_show_text(false);
        progress_bar.set_size_request(500, 40);
        progress_bar.set_hexpand(true);
        column.append(&progress_bar);

        let value = gtk::Label::new(None);
        value.add_css_class("ruzu-loading-value");
        value.set_halign(gtk::Align::Center);
        column.append(&value);
        fade_parent.add_overlay(&column);

        let banner = gtk::Image::new();
        banner.set_halign(gtk::Align::End);
        banner.set_valign(gtk::Align::End);
        banner.set_margin_end(30);
        banner.set_margin_bottom(30);
        banner.set_visible(false);
        fade_parent.add_overlay(&banner);

        Self {
            root,
            fade_parent,
            logo,
            banner,
            stage,
            progress_bar,
            value,
            state: RefCell::new(ProgressState {
                previous_total: 0,
                previous_stage: LoadStage::Complete,
                previous_time: Instant::now(),
                slow_shader_compile_start: false,
                slow_shader_start: Instant::now(),
                slow_shader_first_value: 0,
            }),
            pulse_source: RefCell::new(None),
            banner_animation_source: RefCell::new(None),
        }
    }

    /// The widget to embed in the window (upstream `LoadingScreen` is itself a
    /// `QWidget`).
    pub fn widget(&self) -> &gtk::Overlay {
        &self.root
    }

    /// Reset state before showing. The boot thread supplies loader assets as
    /// soon as `System::load` has created the `AppLoader`.
    pub fn prepare(&self) {
        self.clear();
        self.root.set_visible(true);
        self.fade_parent.set_opacity(1.0);
        {
            let mut state = self.state.borrow_mut();
            state.slow_shader_compile_start = false;
            state.previous_stage = LoadStage::Complete;
            state.previous_total = 0;
            state.previous_time = Instant::now();
        }
        self.on_load_progress(LoadStage::Prepare, 0, 0);
    }

    /// Set the title-provided logo and banner read by `AppLoader`.
    pub fn set_assets(&self, logo: Option<&[u8]>, banner: Option<&[u8]>) {
        set_picture_bytes(&self.logo, logo);
        self.set_banner_bytes(banner);
    }

    /// Progress feed. Faithful port of upstream `LoadingScreen::OnLoadProgress`,
    /// including the ETA heuristic.
    pub fn on_load_progress(&self, stage: LoadStage, value: usize, total: usize) {
        let now = Instant::now();
        let mut state = self.state.borrow_mut();

        // Reset per-stage presentation when the stage changes.
        if stage != state.previous_stage {
            self.stop_pulsing();
            for class in ["prepare", "build", "complete"] {
                self.progress_bar.remove_css_class(class);
            }
            self.progress_bar.add_css_class(match stage {
                LoadStage::Prepare => "prepare",
                LoadStage::Build => "build",
                LoadStage::Complete => "complete",
            });
            // Hide the progress bar during Prepare; show it otherwise.
            self.progress_bar.set_visible(stage != LoadStage::Prepare);
            state.previous_stage = stage;
            state.slow_shader_compile_start = false;
        }

        if total != state.previous_total {
            state.previous_total = total;
        }

        let mut estimate = String::new();
        // If there's a drastic slowdown in the rate, display an estimate.
        if now.duration_since(state.previous_time) > Duration::from_millis(50)
            || state.slow_shader_compile_start
        {
            if !state.slow_shader_compile_start {
                state.slow_shader_start = now;
                state.slow_shader_compile_start = true;
                state.slow_shader_first_value = value;
            }
            // Only estimate after a second has passed since the stage change.
            let diff = now.duration_since(state.slow_shader_start);
            if diff > Duration::from_secs(1) && value > state.slow_shader_first_value {
                let diff_ms = diff.as_millis() as f64;
                let eta_ms = (total - state.slow_shader_first_value) as f64
                    / (value - state.slow_shader_first_value) as f64
                    * diff_ms;
                let shown_ms = (eta_ms - diff_ms + 1000.0).max(1000.0) as u64;
                estimate = format!("Estimated Time {}", format_mm_ss(shown_ms));
            }
        }

        // Update labels and the progress bar.
        self.stage.set_text(&stage_text(stage, value, total));
        self.value.set_text(&estimate);

        if stage == LoadStage::Complete {
            // Upstream sets range(0,0) → indeterminate/marquee.
            self.progress_bar.pulse();
            self.start_pulsing();
        } else if total > 0 {
            let fraction = (value as f64 / total as f64).clamp(0.0, 1.0);
            self.progress_bar.set_fraction(fraction);
        } else {
            self.progress_bar.set_fraction(0.0);
        }

        state.previous_time = now;
    }

    /// Fade to black over 500 ms after the first framebuffer, then notify the
    /// owner so it can reveal the render window. Mirrors upstream
    /// `OnLoadComplete` and its `Hidden` signal.
    pub fn on_load_complete(&self, on_hidden: impl FnOnce() + 'static) {
        let root = self.root.clone();
        let fade_parent = self.fade_parent.clone();
        let logo = self.logo.clone();
        let banner = self.banner.clone();
        let pulse_source = self.pulse_source.take();
        let mut banner_animation_source = self.banner_animation_source.take();
        if let Some(source) = pulse_source {
            source.remove();
        }
        let started = Instant::now();
        let mut on_hidden = Some(on_hidden);
        glib::timeout_add_local(Duration::from_millis(16), move || {
            let elapsed = started.elapsed().as_secs_f64();
            let progress = (elapsed / 0.5).clamp(0.0, 1.0);
            fade_parent.set_opacity((1.0 - ease_out_back(progress)).clamp(0.0, 1.0));
            if progress < 1.0 {
                return glib::ControlFlow::Continue;
            }

            root.set_visible(false);
            fade_parent.set_opacity(1.0);
            logo.set_paintable(gtk::gdk::Paintable::NONE);
            logo.set_visible(false);
            banner.set_from_pixbuf(None);
            banner.set_visible(false);
            if let Some(source) = banner_animation_source.take() {
                source.remove();
            }
            if let Some(callback) = on_hidden.take() {
                callback();
            }
            glib::ControlFlow::Break
        });
    }

    /// Release title-specific image resources. Mirrors upstream `Clear`.
    pub fn clear(&self) {
        self.stop_pulsing();
        if let Some(source) = self.banner_animation_source.borrow_mut().take() {
            source.remove();
        }
        self.logo.set_paintable(gtk::gdk::Paintable::NONE);
        self.logo.set_visible(false);
        self.banner.set_from_pixbuf(None);
        self.banner.set_visible(false);
    }

    /// Decode the title banner and keep animated formats advancing while the
    /// loading screen is visible. This is the GTK counterpart of upstream's
    /// `QMovie` ownership in `LoadingScreen::Prepare`.
    fn set_banner_bytes(&self, bytes: Option<&[u8]>) {
        if let Some(source) = self.banner_animation_source.borrow_mut().take() {
            source.remove();
        }
        self.banner.set_from_pixbuf(None);
        self.banner.set_visible(false);

        let Some(bytes) = bytes else {
            return;
        };
        let loader = gtk::gdk_pixbuf::PixbufLoader::new();
        if loader.write(bytes).is_err() || loader.close().is_err() {
            return;
        }
        let Some(animation) = loader.animation() else {
            return;
        };
        if animation.is_static_image() {
            self.banner
                .set_from_pixbuf(animation.static_image().as_ref());
            self.banner.set_visible(true);
            return;
        }

        let iterator = animation.iter(Some(SystemTime::now()));
        self.banner.set_from_pixbuf(Some(&iterator.pixbuf()));
        self.banner.set_visible(true);

        let banner = self.banner.clone();
        let source = glib::timeout_add_local(Duration::from_millis(16), move || {
            if iterator.advance(SystemTime::now()) {
                banner.set_from_pixbuf(Some(&iterator.pixbuf()));
            }
            // Keep the animation object alive for the iterator's lifetime.
            let _ = &animation;
            glib::ControlFlow::Continue
        });
        *self.banner_animation_source.borrow_mut() = Some(source);
    }

    fn start_pulsing(&self) {
        if self.pulse_source.borrow().is_some() {
            return;
        }
        let progress_bar = self.progress_bar.clone();
        let source = glib::timeout_add_local(Duration::from_millis(80), move || {
            progress_bar.pulse();
            glib::ControlFlow::Continue
        });
        *self.pulse_source.borrow_mut() = Some(source);
    }

    fn stop_pulsing(&self) {
        if let Some(source) = self.pulse_source.borrow_mut().take() {
            source.remove();
        }
    }
}

impl Default for LoadingScreen {
    fn default() -> Self {
        Self::new()
    }
}

/// Format a millisecond duration as `mm:ss`, matching upstream's
/// `QTime(...).toString("mm:ss")`.
fn format_mm_ss(ms: u64) -> String {
    let total_secs = ms / 1000;
    let minutes = (total_secs / 60) % 60;
    let seconds = total_secs % 60;
    format!("{minutes:02}:{seconds:02}")
}

fn stage_text(stage: LoadStage, value: usize, total: usize) -> String {
    match stage {
        LoadStage::Prepare => "Loading...".to_owned(),
        LoadStage::Build => format!("Loading Shaders {value} / {total}"),
        LoadStage::Complete => "Launching...".to_owned(),
    }
}

/// Qt's `QEasingCurve::OutBack`, used by upstream's 500 ms fade animation.
fn ease_out_back(progress: f64) -> f64 {
    const BACK: f64 = 1.70158;
    const CUBIC_BACK: f64 = BACK + 1.0;
    let shifted = progress - 1.0;
    1.0 + CUBIC_BACK * shifted.powi(3) + BACK * shifted.powi(2)
}

fn set_picture_bytes(picture: &gtk::Picture, bytes: Option<&[u8]>) {
    let texture =
        bytes.and_then(|bytes| gtk::gdk::Texture::from_bytes(&glib::Bytes::from(bytes)).ok());
    picture.set_paintable(texture.as_ref());
    picture.set_visible(texture.is_some());
}

fn install_css() {
    let provider = gtk::CssProvider::new();
    provider.load_from_data(
        "
        .ruzu-loading-screen {
            background-color: #000000;
        }
        .ruzu-loading-stage {
            color: #ffffff;
            font-family: Arial, sans-serif;
            font-size: 20pt;
            font-weight: 600;
            letter-spacing: 0;
        }
        .ruzu-loading-value {
            color: #ffffff;
            font-family: Arial, sans-serif;
            font-size: 15pt;
            font-weight: 600;
            letter-spacing: 0;
        }
        progressbar.ruzu-loading-progress trough {
            min-width: 500px;
            min-height: 32px;
            padding: 2px;
            background-color: #000000;
            border: 2px solid #ffffff;
            border-radius: 4px;
        }
        progressbar.ruzu-loading-progress progress {
            min-height: 32px;
            background-color: #ff3c28;
            border-radius: 0;
        }
        progressbar.ruzu-loading-progress.complete trough {
            background-color: #0ab9e6;
        }
        ",
    );
    if let Some(display) = gtk::gdk::Display::default() {
        gtk::style_context_add_provider_for_display(
            &display,
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stage_labels_match_upstream() {
        assert_eq!(stage_text(LoadStage::Prepare, 0, 0), "Loading...");
        assert_eq!(
            stage_text(LoadStage::Build, 907, 929),
            "Loading Shaders 907 / 929"
        );
        assert_eq!(stage_text(LoadStage::Complete, 0, 0), "Launching...");
    }

    #[test]
    fn estimated_time_uses_upstream_mm_ss_format() {
        assert_eq!(format_mm_ss(1_000), "00:01");
        assert_eq!(format_mm_ss(65_000), "01:05");
    }

    #[test]
    fn fade_curve_matches_qt_out_back_endpoints() {
        const TOLERANCE: f64 = 1.0e-12;
        assert!((ease_out_back(0.0) - 0.0).abs() < TOLERANCE);
        assert!((ease_out_back(1.0) - 1.0).abs() < TOLERANCE);
        assert!(ease_out_back(0.8) > 1.0);
    }
}
