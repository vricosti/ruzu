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
/// Upstream: reads `Settings::values.scaling_filter.GetValue()`.
pub fn get_scaling_filter() -> ScalingFilter {
    let settings = common::settings::Values::default();
    match *settings.scaling_filter.get_value() {
        common::settings_enums::ScalingFilter::NearestNeighbor => ScalingFilter::NearestNeighbor,
        common::settings_enums::ScalingFilter::Bilinear => ScalingFilter::Bilinear,
        common::settings_enums::ScalingFilter::Bicubic => ScalingFilter::Bicubic,
        common::settings_enums::ScalingFilter::Gaussian => ScalingFilter::Gaussian,
        common::settings_enums::ScalingFilter::ScaleForce => ScalingFilter::ScaleForce,
        common::settings_enums::ScalingFilter::Fsr => ScalingFilter::Fsr,
        _ => ScalingFilter::Bilinear,
    }
}

/// Get the current anti-aliasing mode from settings.
/// Upstream: reads `Settings::values.anti_aliasing.GetValue()`.
pub fn get_anti_aliasing() -> AntiAliasing {
    let settings = common::settings::Values::default();
    match *settings.anti_aliasing.get_value() {
        common::settings_enums::AntiAliasing::None => AntiAliasing::None,
        common::settings_enums::AntiAliasing::Fxaa => AntiAliasing::Fxaa,
        common::settings_enums::AntiAliasing::Smaa => AntiAliasing::Smaa,
        _ => AntiAliasing::None,
    }
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
