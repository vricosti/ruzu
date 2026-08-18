// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/present.h
//!
//! Presentation filter settings for display and applet capture.

// Upstream `PresentFilters` returns the `Settings` enums directly. Re-exporting
// the common definitions preserves that single owner and prevents renderer-local
// copies from silently falling out of sync when Eden adds a filter.
pub use common::settings_enums::{AntiAliasing, ScalingFilter};

/// Get the current scaling filter from settings.
/// Upstream: reads `Settings::values.scaling_filter.GetValue()`.
pub fn get_scaling_filter() -> ScalingFilter {
    let settings = common::settings::values();
    *settings.scaling_filter.get_value()
}

/// Get the current anti-aliasing mode from settings.
/// Upstream: reads `Settings::values.anti_aliasing.GetValue()`.
pub fn get_anti_aliasing() -> AntiAliasing {
    let settings = common::settings::values();
    *settings.anti_aliasing.get_value()
}

/// Get the scaling filter for applet capture (always Bilinear).
pub fn get_scaling_filter_for_applet_capture() -> ScalingFilter {
    ScalingFilter::Bilinear
}

/// Get the anti-aliasing mode for applet capture (always None).
pub fn get_anti_aliasing_for_applet_capture() -> AntiAliasing {
    AntiAliasing::None
}

/// Function pointers for presentation filter selection.
pub struct PresentFilters {
    pub get_scaling_filter: fn() -> ScalingFilter,
    pub get_anti_aliasing: fn() -> AntiAliasing,
}

/// Filters for normal display.
pub const PRESENT_FILTERS_FOR_DISPLAY: PresentFilters = PresentFilters {
    get_scaling_filter: get_scaling_filter,
    get_anti_aliasing: get_anti_aliasing,
};

/// Filters for applet capture.
pub const PRESENT_FILTERS_FOR_APPLET_CAPTURE: PresentFilters = PresentFilters {
    get_scaling_filter: get_scaling_filter_for_applet_capture,
    get_anti_aliasing: get_anti_aliasing_for_applet_capture,
};
