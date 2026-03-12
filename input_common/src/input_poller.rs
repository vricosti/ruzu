// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/input_poller.h` and `input_common/input_poller.cpp`.
//!
//! Provides InputFactory and OutputFactory that create input/output device
//! instances from ParamPackage parameters and an InputEngine.

use std::sync::Arc;

use parking_lot::Mutex;

use common::input::{
    AnalogProperties, AnalogStatus, BatteryLevel, BatteryStatus, BodyColorStatus, ButtonStatus,
    CallbackStatus, CameraFormat, CameraStatus, DriverResult, InputCallback, InputDevice,
    InputType, LedStatus, MifareRequest, MotionStatus, NfcState, NfcStatus, OutputDevice,
    PollingMode, StickStatus, TouchStatus, TriggerStatus, VibrationStatus,
};
use common::param_package::ParamPackage;
use common::uuid::UUID;

use crate::input_engine::{
    EngineInputType, InputEngine, InputIdentifier, PadIdentifier, UpdateCallback,
};

// ---- Helper: extract identifier from params ----

fn identifier_from_params(params: &ParamPackage) -> PadIdentifier {
    PadIdentifier {
        guid: UUID::from_string(&params.get_str("guid", "")),
        port: params.get_int("port", 0) as usize,
        pad: params.get_int("pad", 0) as usize,
    }
}

fn make_analog(raw_value: f32, properties: AnalogProperties) -> AnalogStatus {
    AnalogStatus {
        value: 0.0,
        raw_value,
        properties,
    }
}

// ---- DummyInput ----
// Port of DummyInput class from input_poller.cpp

struct DummyInput {
    callback: InputCallback,
}

impl DummyInput {
    fn new() -> Self {
        Self {
            callback: InputCallback { on_change: None },
        }
    }
}

impl InputDevice for DummyInput {
    fn set_callback(&mut self, callback: InputCallback) {
        self.callback = callback;
    }

    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change {
            on_change(status);
        }
    }
}

// ---- InputFromButton ----
// Port of InputFromButton class from input_poller.cpp

struct InputFromButton {
    identifier: PadIdentifier,
    button: i32,
    turbo: bool,
    toggle: bool,
    inverted: bool,
    callback_key: i32,
    last_button_value: bool,
    input_engine: Arc<Mutex<InputEngine>>,
    callback: InputCallback,
}

impl InputFromButton {
    fn new(
        identifier: PadIdentifier,
        button: i32,
        turbo: bool,
        toggle: bool,
        inverted: bool,
        input_engine: Arc<Mutex<InputEngine>>,
    ) -> Self {
        let callback_key = {
            let mut engine = input_engine.lock();
            engine.set_callback(InputIdentifier {
                identifier: identifier.clone(),
                r#type: EngineInputType::Button,
                index: button,
                callback: UpdateCallback { on_change: None },
            })
        };
        Self {
            identifier,
            button,
            turbo,
            toggle,
            inverted,
            callback_key,
            last_button_value: false,
            input_engine,
            callback: InputCallback { on_change: None },
        }
    }

    fn get_status(&self) -> ButtonStatus {
        let engine = self.input_engine.lock();
        ButtonStatus {
            value: engine.get_button(&self.identifier, self.button),
            inverted: self.inverted,
            toggle: self.toggle,
            turbo: self.turbo,
            ..Default::default()
        }
    }
}

impl InputDevice for InputFromButton {
    fn force_update(&mut self) {
        let status = CallbackStatus {
            input_type: InputType::Button,
            button_status: self.get_status(),
            ..Default::default()
        };
        self.last_button_value = status.button_status.value;
        self.trigger_on_change(&status);
    }

    fn set_callback(&mut self, callback: InputCallback) {
        self.callback = callback;
    }

    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change {
            on_change(status);
        }
    }
}

impl Drop for InputFromButton {
    fn drop(&mut self) {
        let mut engine = self.input_engine.lock();
        engine.delete_callback(self.callback_key);
    }
}

// ---- InputFromHatButton ----
// Port of InputFromHatButton class from input_poller.cpp

struct InputFromHatButton {
    identifier: PadIdentifier,
    button: i32,
    direction: u8,
    turbo: bool,
    toggle: bool,
    inverted: bool,
    callback_key: i32,
    last_button_value: bool,
    input_engine: Arc<Mutex<InputEngine>>,
    callback: InputCallback,
}

impl InputFromHatButton {
    fn new(
        identifier: PadIdentifier,
        button: i32,
        direction: u8,
        turbo: bool,
        toggle: bool,
        inverted: bool,
        input_engine: Arc<Mutex<InputEngine>>,
    ) -> Self {
        let callback_key = {
            let mut engine = input_engine.lock();
            engine.set_callback(InputIdentifier {
                identifier: identifier.clone(),
                r#type: EngineInputType::HatButton,
                index: button,
                callback: UpdateCallback { on_change: None },
            })
        };
        Self {
            identifier,
            button,
            direction,
            turbo,
            toggle,
            inverted,
            callback_key,
            last_button_value: false,
            input_engine,
            callback: InputCallback { on_change: None },
        }
    }

    fn get_status(&self) -> ButtonStatus {
        let engine = self.input_engine.lock();
        ButtonStatus {
            value: engine.get_hat_button(&self.identifier, self.button, self.direction),
            inverted: self.inverted,
            toggle: self.toggle,
            turbo: self.turbo,
            ..Default::default()
        }
    }
}

impl InputDevice for InputFromHatButton {
    fn force_update(&mut self) {
        let status = CallbackStatus {
            input_type: InputType::Button,
            button_status: self.get_status(),
            ..Default::default()
        };
        self.last_button_value = status.button_status.value;
        self.trigger_on_change(&status);
    }

    fn set_callback(&mut self, callback: InputCallback) {
        self.callback = callback;
    }

    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change {
            on_change(status);
        }
    }
}

impl Drop for InputFromHatButton {
    fn drop(&mut self) {
        let mut engine = self.input_engine.lock();
        engine.delete_callback(self.callback_key);
    }
}

// ---- InputFromStick ----
// Port of InputFromStick class from input_poller.cpp

struct InputFromStick {
    identifier: PadIdentifier,
    axis_x: i32,
    axis_y: i32,
    properties_x: AnalogProperties,
    properties_y: AnalogProperties,
    callback_key_x: i32,
    callback_key_y: i32,
    last_axis_x_value: f32,
    last_axis_y_value: f32,
    input_engine: Arc<Mutex<InputEngine>>,
    invert_axis_y: bool,
    callback: InputCallback,
}

impl InputFromStick {
    fn new(
        identifier: PadIdentifier,
        axis_x: i32,
        axis_y: i32,
        properties_x: AnalogProperties,
        properties_y: AnalogProperties,
        input_engine: Arc<Mutex<InputEngine>>,
    ) -> Self {
        let invert_axis_y = {
            let engine = input_engine.lock();
            engine.get_engine_name() == "sdl"
        };
        let (callback_key_x, callback_key_y) = {
            let mut engine = input_engine.lock();
            let kx = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(),
                r#type: EngineInputType::Analog,
                index: axis_x,
                callback: UpdateCallback { on_change: None },
            });
            let ky = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(),
                r#type: EngineInputType::Analog,
                index: axis_y,
                callback: UpdateCallback { on_change: None },
            });
            (kx, ky)
        };
        Self {
            identifier, axis_x, axis_y, properties_x, properties_y,
            callback_key_x, callback_key_y,
            last_axis_x_value: 0.0, last_axis_y_value: 0.0,
            input_engine, invert_axis_y,
            callback: InputCallback { on_change: None },
        }
    }

    fn get_status(&self) -> StickStatus {
        let engine = self.input_engine.lock();
        let mut status = StickStatus::default();
        status.x = make_analog(engine.get_axis(&self.identifier, self.axis_x), self.properties_x.clone());
        let mut raw_y = engine.get_axis(&self.identifier, self.axis_y);
        if self.invert_axis_y { raw_y = -raw_y; }
        status.y = make_analog(raw_y, self.properties_y.clone());
        status
    }
}

impl InputDevice for InputFromStick {
    fn force_update(&mut self) {
        let status = CallbackStatus {
            input_type: InputType::Stick, stick_status: self.get_status(), ..Default::default()
        };
        self.last_axis_x_value = status.stick_status.x.raw_value;
        self.last_axis_y_value = status.stick_status.y.raw_value;
        self.trigger_on_change(&status);
    }
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromStick {
    fn drop(&mut self) {
        let mut engine = self.input_engine.lock();
        engine.delete_callback(self.callback_key_x);
        engine.delete_callback(self.callback_key_y);
    }
}

// ---- InputFromTouch ----
// Port of InputFromTouch class from input_poller.cpp

struct InputFromTouch {
    identifier: PadIdentifier, button: i32, toggle: bool, inverted: bool,
    axis_x: i32, axis_y: i32,
    properties_x: AnalogProperties, properties_y: AnalogProperties,
    callback_key_button: i32, callback_key_x: i32, callback_key_y: i32,
    last_button_value: bool, last_axis_x_value: f32, last_axis_y_value: f32,
    input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromTouch {
    fn new(
        identifier: PadIdentifier, button: i32, toggle: bool, inverted: bool,
        axis_x: i32, axis_y: i32,
        properties_x: AnalogProperties, properties_y: AnalogProperties,
        input_engine: Arc<Mutex<InputEngine>>,
    ) -> Self {
        let (kb, kx, ky) = {
            let mut engine = input_engine.lock();
            let kb = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Button, index: button,
                callback: UpdateCallback { on_change: None },
            });
            let kx = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Analog, index: axis_x,
                callback: UpdateCallback { on_change: None },
            });
            let ky = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Analog, index: axis_y,
                callback: UpdateCallback { on_change: None },
            });
            (kb, kx, ky)
        };
        Self {
            identifier, button, toggle, inverted, axis_x, axis_y, properties_x, properties_y,
            callback_key_button: kb, callback_key_x: kx, callback_key_y: ky,
            last_button_value: false, last_axis_x_value: 0.0, last_axis_y_value: 0.0,
            input_engine, callback: InputCallback { on_change: None },
        }
    }

    fn get_status(&self) -> TouchStatus {
        let engine = self.input_engine.lock();
        TouchStatus {
            pressed: ButtonStatus {
                value: engine.get_button(&self.identifier, self.button),
                inverted: self.inverted, toggle: self.toggle, ..Default::default()
            },
            x: make_analog(engine.get_axis(&self.identifier, self.axis_x), self.properties_x.clone()),
            y: make_analog(engine.get_axis(&self.identifier, self.axis_y), self.properties_y.clone()),
            ..Default::default()
        }
    }
}

impl InputDevice for InputFromTouch {
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromTouch {
    fn drop(&mut self) {
        let mut engine = self.input_engine.lock();
        engine.delete_callback(self.callback_key_button);
        engine.delete_callback(self.callback_key_x);
        engine.delete_callback(self.callback_key_y);
    }
}

// ---- InputFromTrigger ----
// Port of InputFromTrigger class from input_poller.cpp

struct InputFromTrigger {
    identifier: PadIdentifier, button: i32, toggle: bool, inverted: bool,
    axis: i32, properties: AnalogProperties,
    callback_key_button: i32, axis_callback_key: i32,
    last_button_value: bool, last_axis_value: f32,
    input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromTrigger {
    fn new(
        identifier: PadIdentifier, button: i32, toggle: bool, inverted: bool,
        axis: i32, properties: AnalogProperties, input_engine: Arc<Mutex<InputEngine>>,
    ) -> Self {
        let (kb, ka) = {
            let mut engine = input_engine.lock();
            let kb = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Button, index: button,
                callback: UpdateCallback { on_change: None },
            });
            let ka = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Analog, index: axis,
                callback: UpdateCallback { on_change: None },
            });
            (kb, ka)
        };
        Self {
            identifier, button, toggle, inverted, axis, properties,
            callback_key_button: kb, axis_callback_key: ka,
            last_button_value: false, last_axis_value: 0.0,
            input_engine, callback: InputCallback { on_change: None },
        }
    }

    fn get_status(&self) -> TriggerStatus {
        let engine = self.input_engine.lock();
        TriggerStatus {
            analog: make_analog(engine.get_axis(&self.identifier, self.axis), self.properties.clone()),
            pressed: ButtonStatus {
                value: engine.get_button(&self.identifier, self.button),
                inverted: self.inverted, toggle: self.toggle, ..Default::default()
            },
            ..Default::default()
        }
    }
}

impl InputDevice for InputFromTrigger {
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromTrigger {
    fn drop(&mut self) {
        let mut engine = self.input_engine.lock();
        engine.delete_callback(self.callback_key_button);
        engine.delete_callback(self.axis_callback_key);
    }
}

// ---- InputFromAnalog ----
// Port of InputFromAnalog class from input_poller.cpp

struct InputFromAnalog {
    identifier: PadIdentifier, axis: i32, properties: AnalogProperties,
    callback_key: i32, last_axis_value: f32,
    input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromAnalog {
    fn new(
        identifier: PadIdentifier, axis: i32, properties: AnalogProperties,
        input_engine: Arc<Mutex<InputEngine>>,
    ) -> Self {
        let callback_key = {
            let mut engine = input_engine.lock();
            engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Analog, index: axis,
                callback: UpdateCallback { on_change: None },
            })
        };
        Self {
            identifier, axis, properties, callback_key, last_axis_value: 0.0,
            input_engine, callback: InputCallback { on_change: None },
        }
    }

    fn get_status(&self) -> AnalogStatus {
        let engine = self.input_engine.lock();
        make_analog(engine.get_axis(&self.identifier, self.axis), self.properties.clone())
    }
}

impl InputDevice for InputFromAnalog {
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromAnalog {
    fn drop(&mut self) {
        let mut engine = self.input_engine.lock();
        engine.delete_callback(self.callback_key);
    }
}

// ---- InputFromBattery ----
// Port of InputFromBattery class from input_poller.cpp

struct InputFromBattery {
    identifier: PadIdentifier, callback_key: i32,
    last_battery_value: BatteryStatus,
    input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromBattery {
    fn new(identifier: PadIdentifier, input_engine: Arc<Mutex<InputEngine>>) -> Self {
        let callback_key = {
            let mut engine = input_engine.lock();
            engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Battery, index: 0,
                callback: UpdateCallback { on_change: None },
            })
        };
        Self {
            identifier, callback_key, last_battery_value: BatteryLevel::Charging,
            input_engine, callback: InputCallback { on_change: None },
        }
    }
    fn get_status(&self) -> BatteryStatus {
        let engine = self.input_engine.lock();
        engine.get_battery(&self.identifier)
    }
}

impl InputDevice for InputFromBattery {
    fn force_update(&mut self) {
        let status = CallbackStatus {
            input_type: InputType::Battery, battery_status: self.get_status(), ..Default::default()
        };
        self.last_battery_value = status.battery_status;
        self.trigger_on_change(&status);
    }
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromBattery {
    fn drop(&mut self) { self.input_engine.lock().delete_callback(self.callback_key); }
}

// ---- InputFromColor ----
// Port of InputFromColor class from input_poller.cpp

struct InputFromColor {
    identifier: PadIdentifier, callback_key: i32,
    last_color_value: BodyColorStatus,
    input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromColor {
    fn new(identifier: PadIdentifier, input_engine: Arc<Mutex<InputEngine>>) -> Self {
        let callback_key = {
            let mut engine = input_engine.lock();
            engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Color, index: 0,
                callback: UpdateCallback { on_change: None },
            })
        };
        Self {
            identifier, callback_key, last_color_value: BodyColorStatus::default(),
            input_engine, callback: InputCallback { on_change: None },
        }
    }
    fn get_status(&self) -> BodyColorStatus {
        self.input_engine.lock().get_color(&self.identifier)
    }
}

impl InputDevice for InputFromColor {
    fn force_update(&mut self) {
        let status = CallbackStatus {
            input_type: InputType::Color, color_status: self.get_status(), ..Default::default()
        };
        self.last_color_value = status.color_status;
        self.trigger_on_change(&status);
    }
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromColor {
    fn drop(&mut self) { self.input_engine.lock().delete_callback(self.callback_key); }
}

// ---- InputFromMotion ----
// Port of InputFromMotion class from input_poller.cpp

struct InputFromMotion {
    identifier: PadIdentifier, motion_sensor: i32, gyro_threshold: f32,
    callback_key: i32, input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromMotion {
    fn new(
        identifier: PadIdentifier, motion_sensor: i32, gyro_threshold: f32,
        input_engine: Arc<Mutex<InputEngine>>,
    ) -> Self {
        let callback_key = {
            let mut engine = input_engine.lock();
            engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Motion, index: motion_sensor,
                callback: UpdateCallback { on_change: None },
            })
        };
        Self { identifier, motion_sensor, gyro_threshold, callback_key, input_engine,
               callback: InputCallback { on_change: None } }
    }

    fn get_status(&self) -> MotionStatus {
        let engine = self.input_engine.lock();
        let bm = engine.get_motion(&self.identifier, self.motion_sensor);
        let props = AnalogProperties {
            deadzone: 0.0, range: 1.0, threshold: self.gyro_threshold, offset: 0.0,
            ..Default::default()
        };
        let mut s = MotionStatus::default();
        s.accel.x = make_analog(bm.accel_x, props.clone());
        s.accel.y = make_analog(bm.accel_y, props.clone());
        s.accel.z = make_analog(bm.accel_z, props.clone());
        s.gyro.x = make_analog(bm.gyro_x, props.clone());
        s.gyro.y = make_analog(bm.gyro_y, props.clone());
        s.gyro.z = make_analog(bm.gyro_z, props);
        s.delta_timestamp = bm.delta_timestamp;
        s
    }
}

impl InputDevice for InputFromMotion {
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromMotion {
    fn drop(&mut self) { self.input_engine.lock().delete_callback(self.callback_key); }
}

// ---- InputFromAxisMotion ----
// Port of InputFromAxisMotion class from input_poller.cpp

struct InputFromAxisMotion {
    identifier: PadIdentifier,
    axis_x: i32, axis_y: i32, axis_z: i32,
    properties_x: AnalogProperties, properties_y: AnalogProperties, properties_z: AnalogProperties,
    callback_key_x: i32, callback_key_y: i32, callback_key_z: i32,
    last_axis_x_value: f32, last_axis_y_value: f32, last_axis_z_value: f32,
    input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromAxisMotion {
    fn new(
        identifier: PadIdentifier, axis_x: i32, axis_y: i32, axis_z: i32,
        properties_x: AnalogProperties, properties_y: AnalogProperties, properties_z: AnalogProperties,
        input_engine: Arc<Mutex<InputEngine>>,
    ) -> Self {
        let (kx, ky, kz) = {
            let mut engine = input_engine.lock();
            let kx = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Analog, index: axis_x,
                callback: UpdateCallback { on_change: None },
            });
            let ky = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Analog, index: axis_y,
                callback: UpdateCallback { on_change: None },
            });
            let kz = engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Analog, index: axis_z,
                callback: UpdateCallback { on_change: None },
            });
            (kx, ky, kz)
        };
        Self {
            identifier, axis_x, axis_y, axis_z, properties_x, properties_y, properties_z,
            callback_key_x: kx, callback_key_y: ky, callback_key_z: kz,
            last_axis_x_value: 0.0, last_axis_y_value: 0.0, last_axis_z_value: 0.0,
            input_engine, callback: InputCallback { on_change: None },
        }
    }

    fn get_status(&self) -> MotionStatus {
        let engine = self.input_engine.lock();
        let mut s = MotionStatus::default();
        s.gyro.x = make_analog(engine.get_axis(&self.identifier, self.axis_x), self.properties_x.clone());
        s.gyro.y = make_analog(engine.get_axis(&self.identifier, self.axis_y), self.properties_y.clone());
        s.gyro.z = make_analog(engine.get_axis(&self.identifier, self.axis_z), self.properties_z.clone());
        s.delta_timestamp = 1000;
        s.force_update = true;
        s
    }
}

impl InputDevice for InputFromAxisMotion {
    fn force_update(&mut self) {
        let status = CallbackStatus {
            input_type: InputType::Motion, motion_status: self.get_status(), ..Default::default()
        };
        self.last_axis_x_value = status.motion_status.gyro.x.raw_value;
        self.last_axis_y_value = status.motion_status.gyro.y.raw_value;
        self.last_axis_z_value = status.motion_status.gyro.z.raw_value;
        self.trigger_on_change(&status);
    }
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromAxisMotion {
    fn drop(&mut self) {
        let mut engine = self.input_engine.lock();
        engine.delete_callback(self.callback_key_x);
        engine.delete_callback(self.callback_key_y);
        engine.delete_callback(self.callback_key_z);
    }
}

// ---- InputFromCamera ----
// Port of InputFromCamera class from input_poller.cpp

struct InputFromCamera {
    identifier: PadIdentifier, callback_key: i32,
    input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromCamera {
    fn new(identifier: PadIdentifier, input_engine: Arc<Mutex<InputEngine>>) -> Self {
        let callback_key = {
            let mut engine = input_engine.lock();
            engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Camera, index: 0,
                callback: UpdateCallback { on_change: None },
            })
        };
        Self { identifier, callback_key, input_engine, callback: InputCallback { on_change: None } }
    }
    fn get_status(&self) -> CameraStatus { self.input_engine.lock().get_camera(&self.identifier) }
    fn on_change(&self) {
        let cs = self.get_status();
        let status = CallbackStatus {
            input_type: InputType::IrSensor, camera_status: cs.format, raw_data: cs.data,
            ..Default::default()
        };
        self.trigger_on_change(&status);
    }
}

impl InputDevice for InputFromCamera {
    fn force_update(&mut self) { self.on_change(); }
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromCamera {
    fn drop(&mut self) { self.input_engine.lock().delete_callback(self.callback_key); }
}

// ---- InputFromNfc ----
// Port of InputFromNfc class from input_poller.cpp

struct InputFromNfc {
    identifier: PadIdentifier, callback_key: i32,
    input_engine: Arc<Mutex<InputEngine>>, callback: InputCallback,
}

impl InputFromNfc {
    fn new(identifier: PadIdentifier, input_engine: Arc<Mutex<InputEngine>>) -> Self {
        let callback_key = {
            let mut engine = input_engine.lock();
            engine.set_callback(InputIdentifier {
                identifier: identifier.clone(), r#type: EngineInputType::Nfc, index: 0,
                callback: UpdateCallback { on_change: None },
            })
        };
        Self { identifier, callback_key, input_engine, callback: InputCallback { on_change: None } }
    }
    fn get_status(&self) -> NfcStatus { self.input_engine.lock().get_nfc(&self.identifier) }
    fn on_change(&self) {
        let nfc_status = self.get_status();
        let status = CallbackStatus {
            input_type: InputType::Nfc, nfc_status, ..Default::default()
        };
        self.trigger_on_change(&status);
    }
}

impl InputDevice for InputFromNfc {
    fn force_update(&mut self) { self.on_change(); }
    fn set_callback(&mut self, callback: InputCallback) { self.callback = callback; }
    fn trigger_on_change(&self, status: &CallbackStatus) {
        if let Some(ref on_change) = self.callback.on_change { on_change(status); }
    }
}

impl Drop for InputFromNfc {
    fn drop(&mut self) { self.input_engine.lock().delete_callback(self.callback_key); }
}

// ---- OutputFromIdentifier ----
// Port of OutputFromIdentifier class from input_poller.cpp

struct OutputFromIdentifier {
    _identifier: PadIdentifier,
    _input_engine: Arc<Mutex<InputEngine>>,
}

impl OutputDevice for OutputFromIdentifier {}

// ---- OutputFactory ----
// Port of `OutputFactory` class from input_poller.h

pub struct OutputFactory {
    input_engine: Arc<Mutex<InputEngine>>,
}

impl OutputFactory {
    pub fn new(input_engine: Arc<Mutex<InputEngine>>) -> Self { Self { input_engine } }

    /// Port of OutputFactory::Create
    pub fn create(&self, params: &ParamPackage) -> Box<dyn OutputDevice> {
        let identifier = identifier_from_params(params);
        self.input_engine.lock().pre_set_controller(&identifier);
        Box::new(OutputFromIdentifier {
            _identifier: identifier, _input_engine: Arc::clone(&self.input_engine),
        })
    }
}

// ---- InputFactory ----
// Port of `InputFactory` class from input_poller.h

pub struct InputFactory {
    input_engine: Arc<Mutex<InputEngine>>,
}

impl InputFactory {
    pub fn new(input_engine: Arc<Mutex<InputEngine>>) -> Self { Self { input_engine } }

    /// Port of InputFactory::Create
    pub fn create(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        if params.has("battery") { return self.create_battery_device(params); }
        if params.has("color") { return self.create_color_device(params); }
        if params.has("camera") { return self.create_camera_device(params); }
        if params.has("nfc") { return self.create_nfc_device(params); }
        if params.has("button") && params.has("axis") { return self.create_trigger_device(params); }
        if params.has("button") && params.has("axis_x") && params.has("axis_y") {
            return self.create_touch_device(params);
        }
        if params.has("button") || params.has("code") { return self.create_button_device(params); }
        if params.has("hat") { return self.create_hat_button_device(params); }
        if params.has("axis_x") && params.has("axis_y") && params.has("axis_z") {
            return self.create_motion_device(params.clone());
        }
        if params.has("motion") { return self.create_motion_device(params.clone()); }
        if params.has("axis_x") && params.has("axis_y") { return self.create_stick_device(params); }
        if params.has("axis") { return self.create_analog_device(params); }
        log::error!("Invalid parameters given");
        Box::new(DummyInput::new())
    }

    fn create_button_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(params);
        let button_id = params.get_int("button", 0);
        let keyboard_key = params.get_int("code", 0);
        let toggle = params.get_int("toggle", 0) != 0;
        let inverted = params.get_int("inverted", 0) != 0;
        let turbo = params.get_int("turbo", 0) != 0;
        { let mut e = self.input_engine.lock(); e.pre_set_controller(&id); e.pre_set_button(&id, button_id); e.pre_set_button(&id, keyboard_key); }
        let key = if keyboard_key != 0 { keyboard_key } else { button_id };
        Box::new(InputFromButton::new(id, key, turbo, toggle, inverted, Arc::clone(&self.input_engine)))
    }

    fn create_hat_button_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(params);
        let button_id = params.get_int("hat", 0);
        let _direction_str = params.get_str("direction", "");
        let direction: u8 = 0; // Would call engine.get_hat_button_id in full port
        let toggle = params.get_int("toggle", 0) != 0;
        let inverted = params.get_int("inverted", 0) != 0;
        let turbo = params.get_int("turbo", 0) != 0;
        { let mut e = self.input_engine.lock(); e.pre_set_controller(&id); e.pre_set_hat_button(&id, button_id); }
        Box::new(InputFromHatButton::new(id, button_id, direction, turbo, toggle, inverted, Arc::clone(&self.input_engine)))
    }

    fn create_stick_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let dz = params.get_float("deadzone", 0.15).clamp(0.0, 1.0);
        let rng = params.get_float("range", 0.95).clamp(0.25, 1.50);
        let thr = params.get_float("threshold", 0.5).clamp(0.0, 1.0);
        let id = identifier_from_params(params);
        let ax = params.get_int("axis_x", 0);
        let px = AnalogProperties { deadzone: dz, range: rng, threshold: thr,
            offset: params.get_float("offset_x", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_str("invert_x", "+") == "-", ..Default::default() };
        let ay = params.get_int("axis_y", 1);
        let py = AnalogProperties { deadzone: dz, range: rng, threshold: thr,
            offset: params.get_float("offset_y", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_str("invert_y", "+") != "+", ..Default::default() };
        { let mut e = self.input_engine.lock(); e.pre_set_controller(&id); e.pre_set_axis(&id, ax); e.pre_set_axis(&id, ay); }
        Box::new(InputFromStick::new(id, ax, ay, px, py, Arc::clone(&self.input_engine)))
    }

    fn create_analog_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(params);
        let axis = params.get_int("axis", 0);
        let props = AnalogProperties {
            deadzone: params.get_float("deadzone", 0.0).clamp(0.0, 1.0),
            range: params.get_float("range", 1.0).clamp(0.25, 1.50),
            threshold: params.get_float("threshold", 0.5).clamp(0.0, 1.0),
            offset: params.get_float("offset", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_str("invert", "+") == "-",
            inverted_button: params.get_int("inverted", 0) != 0,
            toggle: params.get_int("toggle", 0) != 0,
        };
        { let mut e = self.input_engine.lock(); e.pre_set_controller(&id); e.pre_set_axis(&id, axis); }
        Box::new(InputFromAnalog::new(id, axis, props, Arc::clone(&self.input_engine)))
    }

    fn create_trigger_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(params);
        let button = params.get_int("button", 0);
        let toggle = params.get_int("toggle", 0) != 0;
        let inverted = params.get_int("inverted", 0) != 0;
        let axis = params.get_int("axis", 0);
        let props = AnalogProperties {
            deadzone: params.get_float("deadzone", 0.0).clamp(0.0, 1.0),
            range: params.get_float("range", 1.0).clamp(0.25, 2.50),
            threshold: params.get_float("threshold", 0.5).clamp(0.0, 1.0),
            offset: params.get_float("offset", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_int("invert", 0) != 0, ..Default::default()
        };
        { let mut e = self.input_engine.lock(); e.pre_set_controller(&id); e.pre_set_axis(&id, axis); e.pre_set_button(&id, button); }
        Box::new(InputFromTrigger::new(id, button, toggle, inverted, axis, props, Arc::clone(&self.input_engine)))
    }

    fn create_touch_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let dz = params.get_float("deadzone", 0.0).clamp(0.0, 1.0);
        let rng = params.get_float("range", 1.0).clamp(0.25, 1.50);
        let thr = params.get_float("threshold", 0.5).clamp(0.0, 1.0);
        let id = identifier_from_params(params);
        let button = params.get_int("button", 0);
        let toggle = params.get_int("toggle", 0) != 0;
        let inverted = params.get_int("inverted", 0) != 0;
        let ax = params.get_int("axis_x", 0);
        let px = AnalogProperties { deadzone: dz, range: rng, threshold: thr,
            offset: params.get_float("offset_x", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_str("invert_x", "+") == "-", ..Default::default() };
        let ay = params.get_int("axis_y", 1);
        let py = AnalogProperties { deadzone: dz, range: rng, threshold: thr,
            offset: params.get_float("offset_y", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_int("invert_y", 0) != 0, ..Default::default() };
        { let mut e = self.input_engine.lock(); e.pre_set_controller(&id); e.pre_set_axis(&id, ax); e.pre_set_axis(&id, ay); e.pre_set_button(&id, button); }
        Box::new(InputFromTouch::new(id, button, toggle, inverted, ax, ay, px, py, Arc::clone(&self.input_engine)))
    }

    fn create_battery_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(params);
        self.input_engine.lock().pre_set_controller(&id);
        Box::new(InputFromBattery::new(id, Arc::clone(&self.input_engine)))
    }

    fn create_color_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(params);
        self.input_engine.lock().pre_set_controller(&id);
        Box::new(InputFromColor::new(id, Arc::clone(&self.input_engine)))
    }

    fn create_motion_device(&self, params: ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(&params);
        if params.has("motion") {
            let ms = params.get_int("motion", 0);
            let gt = params.get_float("threshold", 0.007);
            { let mut e = self.input_engine.lock(); e.pre_set_controller(&id); e.pre_set_motion(&id, ms); }
            return Box::new(InputFromMotion::new(id, ms, gt, Arc::clone(&self.input_engine)));
        }
        let dz = params.get_float("deadzone", 0.15).clamp(0.0, 1.0);
        let rng = params.get_float("range", 1.0).clamp(0.25, 1.50);
        let thr = params.get_float("threshold", 0.5).clamp(0.0, 1.0);
        let ax = params.get_int("axis_x", 0);
        let px = AnalogProperties { deadzone: dz, range: rng, threshold: thr,
            offset: params.get_float("offset_x", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_str("invert_x", "+") == "-", ..Default::default() };
        let ay = params.get_int("axis_y", 1);
        let py = AnalogProperties { deadzone: dz, range: rng, threshold: thr,
            offset: params.get_float("offset_y", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_str("invert_y", "+") != "+", ..Default::default() };
        let az = params.get_int("axis_z", 1);
        let pz = AnalogProperties { deadzone: dz, range: rng, threshold: thr,
            offset: params.get_float("offset_z", 0.0).clamp(-1.0, 1.0),
            inverted: params.get_str("invert_z", "+") != "+", ..Default::default() };
        { let mut e = self.input_engine.lock(); e.pre_set_controller(&id); e.pre_set_axis(&id, ax); e.pre_set_axis(&id, ay); e.pre_set_axis(&id, az); }
        Box::new(InputFromAxisMotion::new(id, ax, ay, az, px, py, pz, Arc::clone(&self.input_engine)))
    }

    fn create_camera_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(params);
        self.input_engine.lock().pre_set_controller(&id);
        Box::new(InputFromCamera::new(id, Arc::clone(&self.input_engine)))
    }

    fn create_nfc_device(&self, params: &ParamPackage) -> Box<dyn InputDevice> {
        let id = identifier_from_params(params);
        self.input_engine.lock().pre_set_controller(&id);
        Box::new(InputFromNfc::new(id, Arc::clone(&self.input_engine)))
    }
}
