// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/input_converter.h and input_converter.cpp
//!
//! Provides conversion functions from raw input data into valid HID status types.
//! These functions depend on common::input types which define callback structures,
//! analog properties, battery levels, etc.

/// Sanitizes an analog value by applying deadzone, range, offset and invert properties.
///
/// Port of `SanitizeAnalog` from upstream.
///
/// # Arguments
/// * `raw_value` - The raw analog input value
/// * `value` - Output: the sanitized value
/// * `deadzone` - Deadzone threshold
/// * `range` - Range scaling factor
/// * `offset` - Center offset
/// * `inverted` - Whether to invert the output direction
/// * `clamp_value` - Whether to clamp the result to [-1.0, 1.0]
pub fn sanitize_analog(
    raw_value: &mut f32,
    value: &mut f32,
    deadzone: f32,
    range: f32,
    offset: f32,
    inverted: bool,
    clamp_value: bool,
) {
    if !raw_value.is_normal() {
        *raw_value = 0.0;
    }

    // Apply center offset
    *raw_value -= offset;

    // Set initial values to be formatted
    *value = *raw_value;

    // Calculate vector size
    let r = value.abs();

    // Return zero if value is smaller than the deadzone
    if r <= deadzone || deadzone == 1.0 {
        *value = 0.0;
        return;
    }

    // Adjust range of value
    let deadzone_factor = 1.0 / r * (r - deadzone) / (1.0 - deadzone);
    *value = *value * deadzone_factor / range;

    // Invert direction if needed
    if inverted {
        *value = -*value;
    }

    // Clamp value
    if clamp_value {
        *value = value.clamp(-1.0, 1.0);
    }
}

/// Sanitizes stick input by applying deadzone, range, offset and invert properties
/// to both X and Y axes, then optionally normalizing to the unit circle.
///
/// Port of `SanitizeStick` from upstream.
///
/// # Arguments
/// * `raw_x` / `raw_y` - Raw input values (modified in place)
/// * `x` / `y` - Output sanitized values
/// * `properties_x` / `properties_y` - Analog properties for each axis
/// * `clamp_value` - Whether to normalize if outside unit circle
pub fn sanitize_stick(
    raw_x: &mut f32,
    raw_y: &mut f32,
    x: &mut f32,
    y: &mut f32,
    deadzone_x: f32,
    range_x: f32,
    offset_x: f32,
    inverted_x: bool,
    offset_y: f32,
    inverted_y: bool,
    clamp_value: bool,
) {
    if !raw_x.is_normal() {
        *raw_x = 0.0;
    }
    if !raw_y.is_normal() {
        *raw_y = 0.0;
    }

    // Apply center offset
    *raw_x += offset_x;
    *raw_y += offset_y;

    // Apply X scale correction from offset
    if offset_x.abs() < 0.75 {
        if *raw_x > 0.0 {
            *raw_x /= 1.0 + offset_x;
        } else {
            *raw_x /= 1.0 - offset_x;
        }
    }

    // Apply Y scale correction from offset
    if offset_y.abs() < 0.75 {
        if *raw_y > 0.0 {
            *raw_y /= 1.0 + offset_y;
        } else {
            *raw_y /= 1.0 - offset_y;
        }
    }

    // Invert direction if needed
    if inverted_x {
        *raw_x = -*raw_x;
    }
    if inverted_y {
        *raw_y = -*raw_y;
    }

    // Set initial values to be formatted
    *x = *raw_x;
    *y = *raw_y;

    // Calculate vector size
    let mut r = (*x * *x + *y * *y).sqrt();

    // Return zero if values are smaller than the deadzone
    if r <= deadzone_x || deadzone_x >= 1.0 {
        *x = 0.0;
        *y = 0.0;
        return;
    }

    // Adjust range of joystick
    let deadzone_factor = 1.0 / r * (r - deadzone_x) / (1.0 - deadzone_x);
    *x = *x * deadzone_factor / range_x;
    *y = *y * deadzone_factor / range_x;
    r = r * deadzone_factor / range_x;

    // Normalize joystick
    if clamp_value && r > 1.0 {
        *x /= r;
        *y /= r;
    }
}

/// Converts a trigger analog value to battery level.
///
/// Port of the analog/trigger case from `TransformToBattery`.
pub fn analog_value_to_battery_level(value: f32) -> u32 {
    if value >= 0.95 {
        5 // Charging
    } else if value > 0.8 {
        4 // Full
    } else if value > 0.6 {
        3 // Medium
    } else if value > 0.4 {
        2 // Low
    } else if value > 0.2 {
        1 // Critical
    } else {
        0 // Empty
    }
}
