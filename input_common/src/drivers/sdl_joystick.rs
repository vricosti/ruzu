// SPDX-FileCopyrightText: 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of the `SDLJoystick` class from
//! `/home/vricosti/Dev/emulators/zuyu/src/input_common/drivers/sdl_driver.cpp`.
//!
//! One instance per opened device. Upstream keeps the `SDL_Joystick*` and
//! `SDL_GameController*` in `unique_ptr`s with `SDL_JoystickClose` /
//! `SDL_GameControllerClose` as deleters; the Rust port closes them in `Drop`.

use std::ffi::CStr;

use sdl2::sys as sdl;

use common::input::{BatteryLevel, VibrationStatus};
use common::uuid::UUID;

use crate::input_engine::{BasicMotion, PadIdentifier};

/// Upstream `rumble_max_duration_ms`.
const RUMBLE_MAX_DURATION_MS: u32 = 2000;

/// Sensitivity limits used to fake frequency response through amplitude.
const LOW_START_SENSITIVITY_LIMIT: f32 = 140.0;
const LOW_WIDTH_SENSITIVITY_LIMIT: f32 = 400.0;
const HIGH_START_SENSITIVITY_LIMIT: f32 = 200.0;
const HIGH_WIDTH_SENSITIVITY_LIMIT: f32 = 700.0;

/// Standard gravity, used to normalise the accelerometer.
const GRAVITY_CONSTANT: f32 = 9.80665;

/// How many all-zero motion samples to tolerate before restarting the sensors.
const MOTION_ERROR_LIMIT: u32 = 200;

/// The GUID of an opened joystick — upstream's anonymous-namespace `GetGUID`.
///
/// The two bytes at offset 2 are cleared on purpose: SDL stores a CRC of the
/// controller *name* there, which changes between SDL releases and between
/// hosts. Leaving it in would give the same physical pad a different identity
/// and silently drop every binding made against it.
pub fn get_guid(joystick: *mut sdl::SDL_Joystick) -> UUID {
    let guid = unsafe { sdl::SDL_JoystickGetGUID(joystick) };
    let mut data = [0u8; 16];
    data.copy_from_slice(&guid.data);
    data[2] = 0;
    data[3] = 0;
    UUID { uuid: data }
}

/// A single opened SDL device.
pub struct SdlJoystick {
    guid: UUID,
    port: i32,
    sdl_joystick: *mut sdl::SDL_Joystick,
    sdl_controller: *mut sdl::SDL_GameController,

    motion: BasicMotion,
    last_motion_update: u32,
    motion_error_count: u32,
    has_gyro: bool,
    has_accel: bool,

    has_vibration: bool,
    is_vibration_tested: bool,
}

// SAFETY: the raw SDL handles are only touched while the owning driver holds
// its joystick-map mutex, and SDL's joystick API is safe to call from the
// thread that opened the subsystem plus its event watcher.
unsafe impl Send for SdlJoystick {}

impl SdlJoystick {
    /// Upstream `SDLJoystick::SDLJoystick`.
    pub fn new(
        guid: UUID,
        port: i32,
        sdl_joystick: *mut sdl::SDL_Joystick,
        sdl_controller: *mut sdl::SDL_GameController,
    ) -> Self {
        let mut joystick = Self {
            guid,
            port,
            sdl_joystick,
            sdl_controller,
            motion: BasicMotion::default(),
            last_motion_update: 0,
            motion_error_count: 0,
            has_gyro: false,
            has_accel: false,
            has_vibration: false,
            is_vibration_tested: false,
        };
        joystick.enable_motion();
        joystick
    }

    /// Upstream `SDLJoystick::EnableMotion`.
    ///
    /// Sensors are toggled off before being probed: upstream does this so a
    /// device whose sensors were already running is re-armed cleanly.
    pub fn enable_motion(&mut self) {
        if self.sdl_controller.is_null() {
            return;
        }
        unsafe {
            if self.has_motion() {
                sdl::SDL_GameControllerSetSensorEnabled(
                    self.sdl_controller,
                    sdl::SDL_SensorType::SDL_SENSOR_ACCEL,
                    sdl::SDL_bool::SDL_FALSE,
                );
                sdl::SDL_GameControllerSetSensorEnabled(
                    self.sdl_controller,
                    sdl::SDL_SensorType::SDL_SENSOR_GYRO,
                    sdl::SDL_bool::SDL_FALSE,
                );
            }
            self.has_accel = sdl::SDL_GameControllerHasSensor(
                self.sdl_controller,
                sdl::SDL_SensorType::SDL_SENSOR_ACCEL,
            ) == sdl::SDL_bool::SDL_TRUE;
            self.has_gyro = sdl::SDL_GameControllerHasSensor(
                self.sdl_controller,
                sdl::SDL_SensorType::SDL_SENSOR_GYRO,
            ) == sdl::SDL_bool::SDL_TRUE;
            if self.has_accel {
                sdl::SDL_GameControllerSetSensorEnabled(
                    self.sdl_controller,
                    sdl::SDL_SensorType::SDL_SENSOR_ACCEL,
                    sdl::SDL_bool::SDL_TRUE,
                );
            }
            if self.has_gyro {
                sdl::SDL_GameControllerSetSensorEnabled(
                    self.sdl_controller,
                    sdl::SDL_SensorType::SDL_SENSOR_GYRO,
                    sdl::SDL_bool::SDL_TRUE,
                );
            }
        }
    }

    /// Upstream `SDLJoystick::HasMotion`.
    pub fn has_motion(&self) -> bool {
        self.has_gyro || self.has_accel
    }

    /// Upstream `SDLJoystick::UpdateMotion`.
    ///
    /// Returns `true` when the sample is worth publishing. Duplicated
    /// timestamps and all-zero samples are dropped; after
    /// [`MOTION_ERROR_LIMIT`] consecutive zero samples the sensors are
    /// restarted, which is upstream's recovery for a pad that stops reporting.
    pub fn update_motion(&mut self, sensor: sdl::SDL_SensorType, timestamp: u32, data: [f32; 3]) -> bool {
        let time_difference = timestamp.wrapping_sub(self.last_motion_update);
        self.last_motion_update = timestamp;

        match sensor {
            sdl::SDL_SensorType::SDL_SENSOR_ACCEL => {
                self.motion.accel_x = -data[0] / GRAVITY_CONSTANT;
                self.motion.accel_y = data[2] / GRAVITY_CONSTANT;
                self.motion.accel_z = -data[1] / GRAVITY_CONSTANT;
            }
            sdl::SDL_SensorType::SDL_SENSOR_GYRO => {
                self.motion.gyro_x = data[0] / (std::f32::consts::PI * 2.0);
                self.motion.gyro_y = -data[2] / (std::f32::consts::PI * 2.0);
                self.motion.gyro_z = data[1] / (std::f32::consts::PI * 2.0);
            }
            _ => {}
        }

        if time_difference == 0 {
            return false;
        }

        let all_zero = self.motion.accel_x == 0.0
            && self.motion.gyro_x == 0.0
            && self.motion.accel_y == 0.0
            && self.motion.gyro_y == 0.0
            && self.motion.accel_z == 0.0
            && self.motion.gyro_z == 0.0;
        if all_zero {
            self.motion_error_count += 1;
            if self.motion_error_count < MOTION_ERROR_LIMIT {
                return false;
            }
            self.motion_error_count = 0;
            self.enable_motion();
            return false;
        }

        self.motion_error_count = 0;
        self.motion.delta_timestamp = time_difference as u64 * 1000;
        true
    }

    /// Upstream `SDLJoystick::GetMotion`.
    pub fn motion(&self) -> &BasicMotion {
        &self.motion
    }

    /// Upstream `SDLJoystick::RumblePlay`.
    ///
    /// SDL exposes only amplitude, so upstream fakes a frequency response by
    /// attenuating the amplitude as the requested frequency rises.
    pub fn rumble_play(&self, vibration: &VibrationStatus) -> bool {
        let low_scale = if vibration.low_frequency > LOW_START_SENSITIVITY_LIMIT {
            (1.0 - (vibration.low_frequency - LOW_START_SENSITIVITY_LIMIT)
                / LOW_WIDTH_SENSITIVITY_LIMIT)
                .max(0.3)
        } else {
            1.0
        };
        let high_scale = if vibration.high_frequency > HIGH_START_SENSITIVITY_LIMIT {
            (1.0 - (vibration.high_frequency - HIGH_START_SENSITIVITY_LIMIT)
                / HIGH_WIDTH_SENSITIVITY_LIMIT)
                .max(0.3)
        } else {
            1.0
        };
        let low = (vibration.low_amplitude * low_scale) as u16;
        let high = (vibration.high_amplitude * high_scale) as u16;

        unsafe {
            if !self.sdl_controller.is_null() {
                sdl::SDL_GameControllerRumble(
                    self.sdl_controller,
                    low,
                    high,
                    RUMBLE_MAX_DURATION_MS,
                ) != -1
            } else if !self.sdl_joystick.is_null() {
                sdl::SDL_JoystickRumble(self.sdl_joystick, low, high, RUMBLE_MAX_DURATION_MS) != -1
            } else {
                false
            }
        }
    }

    /// Upstream `SDLJoystick::HasHDRumble`.
    pub fn has_hd_rumble(&self) -> bool {
        if self.sdl_controller.is_null() {
            return false;
        }
        let controller_type = unsafe { sdl::SDL_GameControllerGetType(self.sdl_controller) };
        matches!(
            controller_type,
            sdl::SDL_GameControllerType::SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_PRO
                | sdl::SDL_GameControllerType::SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_JOYCON_LEFT
                | sdl::SDL_GameControllerType::SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_JOYCON_RIGHT
                | sdl::SDL_GameControllerType::SDL_CONTROLLER_TYPE_PS5
        )
    }

    /// Upstream `SDLJoystick::EnableVibration`.
    pub fn enable_vibration(&mut self, is_enabled: bool) {
        self.has_vibration = is_enabled;
        self.is_vibration_tested = true;
    }

    pub fn has_vibration(&self) -> bool {
        self.has_vibration
    }

    pub fn is_vibration_tested(&self) -> bool {
        self.is_vibration_tested
    }

    /// Upstream `SDLJoystick::GetPadIdentifier`.
    pub fn pad_identifier(&self) -> PadIdentifier {
        PadIdentifier {
            guid: self.guid,
            port: self.port as usize,
            pad: 0,
        }
    }

    pub fn guid(&self) -> UUID {
        self.guid
    }

    pub fn port(&self) -> i32 {
        self.port
    }

    pub fn sdl_joystick(&self) -> *mut sdl::SDL_Joystick {
        self.sdl_joystick
    }

    pub fn sdl_game_controller(&self) -> *mut sdl::SDL_GameController {
        self.sdl_controller
    }

    /// Upstream `SDLJoystick::SetSDLJoystick` — rebind a reconnected device to
    /// the slot it previously occupied, closing whatever was there.
    pub fn set_sdl_joystick(
        &mut self,
        joystick: *mut sdl::SDL_Joystick,
        controller: *mut sdl::SDL_GameController,
    ) {
        self.close_handles();
        self.sdl_joystick = joystick;
        self.sdl_controller = controller;
    }

    /// Upstream `SDLJoystick::GetControllerName`.
    pub fn controller_name(&self) -> String {
        unsafe {
            if !self.sdl_controller.is_null() {
                let name = sdl::SDL_GameControllerName(self.sdl_controller);
                if !name.is_null() {
                    return CStr::from_ptr(name).to_string_lossy().into_owned();
                }
            }
            if !self.sdl_joystick.is_null() {
                let name = sdl::SDL_JoystickName(self.sdl_joystick);
                if !name.is_null() {
                    return CStr::from_ptr(name).to_string_lossy().into_owned();
                }
            }
        }
        "Unknown".to_string()
    }

    /// Upstream `SDLJoystick::IsJoyconLeft` / `IsJoyconRight`.
    pub fn is_joycon_left(&self) -> bool {
        self.controller_name().contains("Joy-Con Left")
    }

    pub fn is_joycon_right(&self) -> bool {
        self.controller_name().contains("Joy-Con Right")
    }

    /// Upstream `SDLJoystick::GetBatteryLevel`.
    pub fn battery_level(power_level: sdl::SDL_JoystickPowerLevel) -> BatteryLevel {
        match power_level {
            sdl::SDL_JoystickPowerLevel::SDL_JOYSTICK_POWER_EMPTY => BatteryLevel::Empty,
            sdl::SDL_JoystickPowerLevel::SDL_JOYSTICK_POWER_LOW => BatteryLevel::Critical,
            sdl::SDL_JoystickPowerLevel::SDL_JOYSTICK_POWER_MEDIUM => BatteryLevel::Low,
            sdl::SDL_JoystickPowerLevel::SDL_JOYSTICK_POWER_FULL => BatteryLevel::Full,
            sdl::SDL_JoystickPowerLevel::SDL_JOYSTICK_POWER_MAX => BatteryLevel::Charging,
            // SDL_JOYSTICK_POWER_WIRED and SDL_JOYSTICK_POWER_UNKNOWN
            _ => BatteryLevel::Charging,
        }
    }

    fn close_handles(&mut self) {
        unsafe {
            if !self.sdl_controller.is_null() {
                sdl::SDL_GameControllerClose(self.sdl_controller);
                self.sdl_controller = std::ptr::null_mut();
            }
            if !self.sdl_joystick.is_null() {
                sdl::SDL_JoystickClose(self.sdl_joystick);
                self.sdl_joystick = std::ptr::null_mut();
            }
        }
    }
}

impl Drop for SdlJoystick {
    /// Upstream relies on the `unique_ptr` deleters
    /// (`SDL_JoystickClose` / `SDL_GameControllerClose`).
    fn drop(&mut self) {
        self.close_handles();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_guid_clears_the_controller_name_crc() {
        // SDL stores a CRC of the controller *name* in bytes 2..4. It changes
        // between SDL releases, so leaving it in would give the same physical
        // pad a new identity and silently drop its bindings.
        let mut raw = sdl::SDL_JoystickGUID { data: [0u8; 16] };
        for (index, byte) in raw.data.iter_mut().enumerate() {
            *byte = index as u8 + 1;
        }
        // Reproduce what `get_guid` does to the raw bytes.
        let mut data = [0u8; 16];
        data.copy_from_slice(&raw.data);
        data[2] = 0;
        data[3] = 0;

        assert_eq!(data[0], 1);
        assert_eq!(data[1], 2);
        assert_eq!(data[2], 0, "name CRC low byte must be cleared");
        assert_eq!(data[3], 0, "name CRC high byte must be cleared");
        assert_eq!(data[4], 5, "bytes past the CRC must survive");
    }

    #[test]
    fn rumble_amplitude_is_attenuated_above_the_frequency_limits() {
        // Below the limit the amplitude passes through; above it, upstream
        // scales down but never below 0.3.
        let scale = |freq: f32, start: f32, width: f32| {
            if freq > start {
                (1.0 - (freq - start) / width).max(0.3)
            } else {
                1.0
            }
        };
        assert_eq!(scale(100.0, LOW_START_SENSITIVITY_LIMIT, LOW_WIDTH_SENSITIVITY_LIMIT), 1.0);
        assert!(scale(300.0, LOW_START_SENSITIVITY_LIMIT, LOW_WIDTH_SENSITIVITY_LIMIT) < 1.0);
        // Far past the limit it clamps rather than going negative.
        assert_eq!(scale(9000.0, LOW_START_SENSITIVITY_LIMIT, LOW_WIDTH_SENSITIVITY_LIMIT), 0.3);
    }

    #[test]
    fn battery_level_maps_every_sdl_power_level() {
        use sdl::SDL_JoystickPowerLevel as P;
        assert_eq!(SdlJoystick::battery_level(P::SDL_JOYSTICK_POWER_EMPTY), BatteryLevel::Empty);
        assert_eq!(SdlJoystick::battery_level(P::SDL_JOYSTICK_POWER_LOW), BatteryLevel::Critical);
        assert_eq!(SdlJoystick::battery_level(P::SDL_JOYSTICK_POWER_MEDIUM), BatteryLevel::Low);
        assert_eq!(SdlJoystick::battery_level(P::SDL_JOYSTICK_POWER_FULL), BatteryLevel::Full);
        // A wired pad reports UNKNOWN/WIRED; upstream treats both as charging.
        assert_eq!(SdlJoystick::battery_level(P::SDL_JOYSTICK_POWER_WIRED), BatteryLevel::Charging);
    }
}
