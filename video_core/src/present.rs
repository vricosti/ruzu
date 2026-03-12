// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/present.h
//!
//! Presentation filter settings for display and applet capture.

/// Scaling filter types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalingFilter {
    NearestNeighbor,
    Bilinear,
    Bicubic,
    Gaussian,
    ScaleForce,
    Fsr,
}

/// Anti-aliasing modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AntiAliasing {
    None,
    Fxaa,
    Smaa,
}

/// Get the current scaling filter from settings.
///
/// In the full port, this reads from Settings::values.scaling_filter.
pub fn get_scaling_filter() -> ScalingFilter {
    // TODO: Read from settings
    ScalingFilter::Bilinear
}

/// Get the current anti-aliasing mode from settings.
///
/// In the full port, this reads from Settings::values.anti_aliasing.
pub fn get_anti_aliasing() -> AntiAliasing {
    // TODO: Read from settings
    AntiAliasing::None
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
