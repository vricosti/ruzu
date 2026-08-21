// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/input_converter.h and input_converter.cpp
//!
//! Provides conversion functions from raw input data into valid HID status types.
//! These functions depend on common::input types which define callback structures,
//! analog properties, battery levels, etc.

use common::input::{
    AnalogProperties, AnalogStatus, ButtonStatus, CallbackStatus, CameraStatus, InputType,
    MotionStatus, NfcStatus, StickStatus, TouchStatus, TriggerStatus,
};

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

/// Converts callback data to a normalized trigger status.
///
/// Port of upstream `TransformToTrigger`.
pub fn transform_to_trigger(callback: &CallbackStatus) -> TriggerStatus {
    let mut status = TriggerStatus::default();
    let mut calculate_button_value = true;

    match callback.input_type {
        InputType::Analog => {
            status.analog.properties = callback.analog_status.properties;
            status.analog.raw_value = callback.analog_status.raw_value;
        }
        InputType::Button => {
            status.analog.properties.range = 1.0;
            status.analog.properties.inverted = callback.button_status.inverted;
            status.analog.raw_value = if callback.button_status.value {
                1.0
            } else {
                0.0
            };
        }
        InputType::Trigger => {
            status = callback.trigger_status;
            calculate_button_value = false;
        }
        InputType::Motion => {
            status.analog.properties.range = 1.0;
            status.analog.raw_value = callback.motion_status.accel.x.raw_value;
        }
        input_type => {
            log::error!(
                "Conversion from input type {:?} to trigger not implemented",
                input_type
            );
        }
    }

    let properties = status.analog.properties;
    sanitize_analog(
        &mut status.analog.raw_value,
        &mut status.analog.value,
        properties.deadzone,
        properties.range,
        properties.offset,
        properties.inverted,
        true,
    );

    if calculate_button_value {
        status.pressed.value = status.analog.value > properties.threshold;
    }

    if properties.inverted {
        status.analog.value = 1.0 + status.analog.value;
    }
    status.analog.value = status.analog.value.clamp(0.0, 1.0);
    status
}

/// Converts callback data to a normalized analog status.
///
/// Port of upstream `TransformToAnalog`.
pub fn transform_to_analog(callback: &CallbackStatus) -> AnalogStatus {
    let mut status = AnalogStatus::default();
    if callback.input_type == InputType::Analog {
        status.properties = callback.analog_status.properties;
        status.raw_value = callback.analog_status.raw_value;
    } else {
        log::error!(
            "Conversion from input type {:?} to analog not implemented",
            callback.input_type
        );
    }

    let properties = status.properties;
    sanitize_analog(
        &mut status.raw_value,
        &mut status.value,
        properties.deadzone,
        properties.range,
        properties.offset,
        properties.inverted,
        false,
    );
    if properties.inverted {
        status.value = -status.value;
    }
    status
}

/// Converts callback data to a button status.
///
/// Port of upstream `TransformToButton`.
pub fn transform_to_button(callback: &CallbackStatus) -> ButtonStatus {
    let mut status = ButtonStatus::default();

    match callback.input_type {
        InputType::Analog => {
            status.value = transform_to_trigger(callback).pressed.value;
            status.toggle = callback.analog_status.properties.toggle;
            status.inverted = callback.analog_status.properties.inverted_button;
        }
        InputType::Trigger => {
            status.value = transform_to_trigger(callback).pressed.value;
        }
        InputType::Button => {
            status = callback.button_status;
        }
        InputType::Motion => {
            status.value = callback.motion_status.gyro.x.raw_value.abs() > 1.0;
        }
        input_type => {
            log::error!(
                "Conversion from input type {:?} to button not implemented",
                input_type
            );
        }
    }

    if status.inverted {
        status.value = !status.value;
    }
    status
}

/// Converts callback data to motion input.
///
/// Port of upstream `TransformToMotion`.
pub fn transform_to_motion(callback: &CallbackStatus) -> MotionStatus {
    let mut status = match callback.input_type {
        InputType::Button => {
            let properties = AnalogProperties {
                deadzone: 0.0,
                range: 1.0,
                offset: 0.0,
                ..Default::default()
            };
            let mut status = MotionStatus {
                delta_timestamp: 1000,
                force_update: true,
                ..Default::default()
            };
            for axis in [
                &mut status.accel.x,
                &mut status.accel.y,
                &mut status.accel.z,
                &mut status.gyro.x,
                &mut status.gyro.y,
                &mut status.gyro.z,
            ] {
                axis.properties = properties;
            }
            status.accel.z.raw_value = -1.0;
            if transform_to_button(callback).value {
                for axis in [
                    &mut status.accel.x,
                    &mut status.accel.y,
                    &mut status.accel.z,
                    &mut status.gyro.x,
                    &mut status.gyro.y,
                    &mut status.gyro.z,
                ] {
                    axis.raw_value = f32::from(fastrand::i16(-5000..=5000)) * 0.001;
                }
            }
            status
        }
        InputType::Motion => callback.motion_status,
        input_type => {
            log::error!(
                "Conversion from input type {:?} to motion not implemented",
                input_type
            );
            MotionStatus::default()
        }
    };

    for axis in [
        &mut status.accel.x,
        &mut status.accel.y,
        &mut status.accel.z,
        &mut status.gyro.x,
        &mut status.gyro.y,
        &mut status.gyro.z,
    ] {
        let properties = axis.properties;
        sanitize_analog(
            &mut axis.raw_value,
            &mut axis.value,
            properties.deadzone,
            properties.range,
            properties.offset,
            properties.inverted,
            false,
        );
    }
    status
}

/// Converts callback data to a normalized stick status.
///
/// Port of upstream `TransformToStick`.
pub fn transform_to_stick(callback: &CallbackStatus) -> StickStatus {
    let mut status = match callback.input_type {
        InputType::Stick => callback.stick_status,
        input_type => {
            log::error!(
                "Conversion from input type {:?} to stick not implemented",
                input_type
            );
            StickStatus::default()
        }
    };

    let properties_x = status.x.properties;
    let properties_y = status.y.properties;
    sanitize_stick(
        &mut status.x.raw_value,
        &mut status.y.raw_value,
        &mut status.x.value,
        &mut status.y.value,
        properties_x.deadzone,
        properties_x.range,
        properties_x.offset,
        properties_x.inverted,
        properties_y.offset,
        properties_y.inverted,
        true,
    );

    // Set directional buttons
    status.right = status.x.value > properties_x.threshold;
    status.left = status.x.value < -properties_x.threshold;
    status.up = status.y.value > properties_y.threshold;
    status.down = status.y.value < -properties_y.threshold;
    status
}

/// Converts callback data to a normalized touch status.
///
/// Port of upstream `TransformToTouch`.
pub fn transform_to_touch(callback: &CallbackStatus) -> TouchStatus {
    let mut status = match callback.input_type {
        InputType::Touch => callback.touch_status,
        InputType::Stick => TouchStatus {
            x: callback.stick_status.x,
            y: callback.stick_status.y,
            ..Default::default()
        },
        input_type => {
            log::error!(
                "Conversion from input type {:?} to touch not implemented",
                input_type
            );
            TouchStatus::default()
        }
    };

    let x_properties = status.x.properties;
    sanitize_analog(
        &mut status.x.raw_value,
        &mut status.x.value,
        x_properties.deadzone,
        x_properties.range,
        x_properties.offset,
        x_properties.inverted,
        true,
    );
    let y_properties = status.y.properties;
    sanitize_analog(
        &mut status.y.raw_value,
        &mut status.y.value,
        y_properties.deadzone,
        y_properties.range,
        y_properties.offset,
        y_properties.inverted,
        true,
    );

    if status.x.properties.inverted {
        status.x.value = 1.0 + status.x.value;
    }
    if status.y.properties.inverted {
        status.y.value = 1.0 + status.y.value;
    }
    status.x.value = status.x.value.clamp(0.0, 1.0);
    status.y.value = status.y.value.clamp(0.0, 1.0);
    if status.pressed.inverted {
        status.pressed.value = !status.pressed.value;
    }
    status
}

/// Converts callback data to an infrared-camera frame.
///
/// Port of upstream `TransformToCamera`.
pub fn transform_to_camera(callback: &CallbackStatus) -> CameraStatus {
    if callback.input_type == InputType::IrSensor {
        return CameraStatus {
            format: callback.camera_status,
            data: callback.raw_data.clone(),
        };
    }
    log::error!(
        "Conversion from input type {:?} to camera not implemented",
        callback.input_type
    );
    CameraStatus::default()
}

/// Converts callback data to an NFC status.
///
/// Port of upstream `TransformToNfc`.
pub fn transform_to_nfc(callback: &CallbackStatus) -> NfcStatus {
    if callback.input_type == InputType::Nfc {
        return callback.nfc_status.clone();
    }
    log::error!(
        "Conversion from input type {:?} to NFC not implemented",
        callback.input_type
    );
    NfcStatus::default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transform_to_analog_preserves_upstream_inversion_order() {
        let mut callback = CallbackStatus {
            input_type: InputType::Analog,
            ..Default::default()
        };
        callback.analog_status.raw_value = 0.5;
        callback.analog_status.properties.inverted = true;

        let status = transform_to_analog(&callback);

        assert_eq!(status.raw_value, 0.5);
        assert_eq!(status.value, 0.5);
        assert!(status.properties.inverted);
    }

    #[test]
    fn transform_to_analog_applies_deadzone_without_clamping() {
        let mut callback = CallbackStatus {
            input_type: InputType::Analog,
            ..Default::default()
        };
        callback.analog_status.raw_value = 2.0;
        callback.analog_status.properties.range = 0.5;

        assert_eq!(transform_to_analog(&callback).value, 4.0);

        callback.analog_status.raw_value = 0.1;
        callback.analog_status.properties.deadzone = 0.2;
        assert_eq!(transform_to_analog(&callback).value, 0.0);
    }
}
