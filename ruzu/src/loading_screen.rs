// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of the upstream `LoadingScreen` widget in
// `/Users/vricosti/Dev/emulators/zuyu/src/yuzu/loading_screen.{h,cpp,ui}`.
//
// Shown while the emulator loads disk resources (chiefly shader compilation).
// It displays a logo, a stage label, a progress bar, and an estimated-time
// label. The progress feed is `OnLoadProgress(stage, value, total)`, driven by
// `VideoCore`'s `DiskResourceLoadCallback`.

use std::cell::RefCell;
use std::time::{Duration, Instant};

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
    root: gtk::Box,
    stage: gtk::Label,
    progress_bar: gtk::ProgressBar,
    value: gtk::Label,
    state: RefCell<ProgressState>,
}

impl LoadingScreen {
    /// Build the widget tree. Mirrors `loading_screen.ui`: a centered logo, a
    /// stage label, a progress bar, and an estimate label.
    pub fn new() -> Self {
        let root = gtk::Box::new(gtk::Orientation::Vertical, 0);
        root.set_hexpand(true);
        root.set_vexpand(true);

        // Logo (upstream shows the game's logo pixmap; text placeholder here).
        let logo = gtk::Label::new(Some("ruzu"));
        logo.add_css_class("title-1");
        logo.set_margin_top(24);
        logo.set_margin_bottom(24);
        root.append(&logo);

        // Centered column holding stage / progress / estimate, with flexible
        // space above and below (upstream stretch = 1,0,1).
        let column = gtk::Box::new(gtk::Orientation::Vertical, 8);
        column.set_valign(gtk::Align::Center);
        column.set_vexpand(true);
        column.set_margin_start(48);
        column.set_margin_end(48);

        let stage = gtk::Label::new(Some("Loading..."));
        stage.set_halign(gtk::Align::Center);
        column.append(&stage);

        let progress_bar = gtk::ProgressBar::new();
        progress_bar.set_show_text(true);
        progress_bar.set_hexpand(true);
        column.append(&progress_bar);

        let value = gtk::Label::new(None);
        value.set_halign(gtk::Align::Center);
        value.add_css_class("dim-label");
        column.append(&value);

        root.append(&column);

        Self {
            root,
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
        }
    }

    /// The widget to embed in the window (upstream `LoadingScreen` is itself a
    /// `QWidget`).
    pub fn widget(&self) -> &gtk::Box {
        &self.root
    }

    /// Reset state before showing. Mirrors upstream `Prepare` (minus the logo /
    /// banner pixmap loading, which needs an `AppLoader`).
    pub fn prepare(&self) {
        {
            let mut state = self.state.borrow_mut();
            state.slow_shader_compile_start = false;
            state.previous_stage = LoadStage::Complete;
            state.previous_total = 0;
            state.previous_time = Instant::now();
        }
        self.on_load_progress(LoadStage::Prepare, 0, 0);
    }

    /// Progress feed. Faithful port of upstream `LoadingScreen::OnLoadProgress`,
    /// including the ETA heuristic.
    pub fn on_load_progress(&self, stage: LoadStage, value: usize, total: usize) {
        let now = Instant::now();
        let mut state = self.state.borrow_mut();

        // Reset per-stage presentation when the stage changes.
        if stage != state.previous_stage {
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
        match stage {
            LoadStage::Prepare => self.stage.set_text("Loading..."),
            LoadStage::Build => self
                .stage
                .set_text(&format!("Loading Shaders {value} / {total}")),
            LoadStage::Complete => self.stage.set_text("Launching..."),
        }
        self.value.set_text(&estimate);

        if stage == LoadStage::Complete {
            // Upstream sets range(0,0) → indeterminate/marquee.
            self.progress_bar.pulse();
            self.progress_bar.set_text(Some("Launching..."));
        } else if total > 0 {
            let fraction = (value as f64 / total as f64).clamp(0.0, 1.0);
            self.progress_bar.set_fraction(fraction);
            self.progress_bar
                .set_text(Some(&format!("Loading Shaders {value} out of {total}")));
        } else {
            self.progress_bar.set_fraction(0.0);
            self.progress_bar.set_text(None);
        }

        state.previous_time = now;
    }

    /// Called after loading finishes. Upstream fades out then emits `Hidden`;
    /// here we simply hide the widget (the owner swaps to the render page).
    pub fn on_load_complete(&self) {
        self.root.set_visible(false);
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
