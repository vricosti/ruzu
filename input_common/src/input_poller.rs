// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/input_poller.h` and `input_common/input_poller.cpp`.
//!
//! Provides InputFactory and OutputFactory that create input/output device
//! instances from ParamPackage parameters and an InputEngine.

use std::sync::Arc;

use parking_lot::Mutex;

use common::input::{InputDevice, OutputDevice};
use common::param_package::ParamPackage;

use crate::input_engine::InputEngine;

/// Port of `OutputFactory` class from input_poller.h
/// Creates output devices from parameters given.
pub struct OutputFactory {
    input_engine: Arc<Mutex<InputEngine>>,
}

impl OutputFactory {
    pub fn new(input_engine: Arc<Mutex<InputEngine>>) -> Self {
        Self { input_engine }
    }

    /// Creates an output device from the parameters given.
    /// Port of OutputFactory::Create
    pub fn create(&self, _params: &ParamPackage) -> Box<dyn OutputDevice> {
        todo!()
    }
}

/// Port of `InputFactory` class from input_poller.h
/// Creates input devices from parameters and an InputEngine.
pub struct InputFactory {
    input_engine: Arc<Mutex<InputEngine>>,
}

impl InputFactory {
    pub fn new(input_engine: Arc<Mutex<InputEngine>>) -> Self {
        Self { input_engine }
    }

    /// Creates an input device from the parameters given.
    /// Identifies the type of input to be returned based on parameter contents.
    /// Port of InputFactory::Create
    pub fn create(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateButtonDevice
    fn create_button_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateHatButtonDevice
    fn create_hat_button_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateStickDevice
    fn create_stick_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateAnalogDevice
    fn create_analog_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateTriggerDevice
    fn create_trigger_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateTouchDevice
    fn create_touch_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateBatteryDevice
    fn create_battery_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateColorDevice
    fn create_color_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateMotionDevice
    fn create_motion_device(&self, _params: ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateCameraDevice
    fn create_camera_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }

    /// Port of InputFactory::CreateNfcDevice
    fn create_nfc_device(&self, _params: &ParamPackage) -> Box<dyn InputDevice> {
        todo!()
    }
}
