// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/main.h` and `input_common/main.cpp`.
//!
//! Provides the InputSubsystem that manages all input device factories and drivers.

use std::collections::HashMap;
use std::sync::Arc;

use parking_lot::Mutex;

use common::input::ButtonNames;
use common::param_package::ParamPackage;
use common::uuid::UUID;

use crate::drivers::camera::Camera;
use crate::drivers::keyboard::Keyboard;
use crate::drivers::mouse::Mouse;
use crate::drivers::tas_input;
use crate::drivers::touch_screen::TouchScreen;
use crate::drivers::virtual_amiibo::VirtualAmiibo;
use crate::drivers::virtual_gamepad::VirtualGamepad;
use crate::input_engine::{InputEngine, MappingCallback, MappingData, PadIdentifier};
use crate::input_mapping::MappingFactory;

/// Port of `Polling` namespace from main.h
pub mod Polling {
    /// Type of input desired for mapping purposes.
    /// Port of Polling::InputType enum from main.h
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum InputType {
        None,
        Button,
        Stick,
        Motion,
        Touch,
    }

    impl Default for InputType {
        fn default() -> Self {
            InputType::None
        }
    }
}

// Port of type aliases from main.h
// Using i32 as placeholder for Settings::NativeAnalog::Values etc.
pub type AnalogMapping = HashMap<i32, ParamPackage>;
pub type ButtonMapping = HashMap<i32, ParamPackage>;
pub type MotionMapping = HashMap<i32, ParamPackage>;

/// Dummy engine to get periodic updates.
/// Port of UpdateEngine from main.cpp
struct UpdateEngine {
    engine: InputEngine,
    last_state: bool,
}

impl UpdateEngine {
    const IDENTIFIER: PadIdentifier = PadIdentifier {
        guid: UUID::new(), // UUID{} in C++ is a zero UUID
        port: 0,
        pad: 0,
    };

    fn new(input_engine: String) -> Self {
        let mut engine = InputEngine::new(input_engine);
        engine.pre_set_controller(&Self::IDENTIFIER);
        Self {
            engine,
            last_state: false,
        }
    }

    fn pump_events(&mut self) {
        self.engine
            .set_button(&Self::IDENTIFIER, 0, self.last_state);
        self.last_state = !self.last_state;
    }
}

/// Port of InputSubsystem::Impl from main.cpp
struct InputSubsystemImpl {
    mapping_factory: Option<MappingFactory>,

    update_engine: Option<UpdateEngine>,
    keyboard: Option<Keyboard>,
    mouse: Option<Mouse>,
    touch_screen: Option<TouchScreen>,
    tas_input: Option<tas_input::Tas>,
    camera: Option<Camera>,
    virtual_amiibo: Option<VirtualAmiibo>,
    virtual_gamepad: Option<VirtualGamepad>,
    // GCAdapter, SDLDriver, Joycons, Android omitted (feature-gated in C++)
}

impl InputSubsystemImpl {
    fn new() -> Self {
        Self {
            mapping_factory: None,
            update_engine: None,
            keyboard: None,
            mouse: None,
            touch_screen: None,
            tas_input: None,
            camera: None,
            virtual_amiibo: None,
            virtual_gamepad: None,
        }
    }

    /// Port of Impl::Initialize
    fn initialize(&mut self) {
        self.mapping_factory = Some(MappingFactory::new());

        self.update_engine = Some(UpdateEngine::new("updater".to_string()));
        self.keyboard = Some(Keyboard::new("keyboard".to_string()));
        self.mouse = Some(Mouse::new("mouse".to_string()));
        self.touch_screen = Some(TouchScreen::new("touch".to_string()));
        self.tas_input = Some(tas_input::Tas::new("tas".to_string()));
        self.camera = Some(Camera::new("camera".to_string()));
        self.virtual_amiibo = Some(VirtualAmiibo::new("virtual_amiibo".to_string()));
        self.virtual_gamepad = Some(VirtualGamepad::new("virtual_gamepad".to_string()));
    }

    /// Port of Impl::Shutdown
    fn shutdown(&mut self) {
        self.update_engine = None;
        self.keyboard = None;
        self.mouse = None;
        self.touch_screen = None;
        self.tas_input = None;
        self.camera = None;
        self.virtual_amiibo = None;
        self.virtual_gamepad = None;
        self.mapping_factory = None;
    }

    /// Port of Impl::GetInputDevices
    fn get_input_devices(&self) -> Vec<ParamPackage> {
        let mut devices = vec![{
            let mut p = ParamPackage::default();
            p.set_str("display", "Any".to_string());
            p.set_str("engine", "any".to_string());
            p
        }];

        if let Some(ref keyboard) = self.keyboard {
            devices.extend(keyboard.get_input_devices());
        }
        if let Some(ref mouse) = self.mouse {
            devices.extend(mouse.get_input_devices());
        }

        devices
    }

    /// Port of Impl::PumpEvents
    fn pump_events(&mut self) {
        if let Some(ref mut update_engine) = self.update_engine {
            update_engine.pump_events();
        }
    }

    /// Port of Impl::RegisterInput
    fn register_input(&mut self, data: &MappingData) {
        if let Some(ref mut mapping_factory) = self.mapping_factory {
            mapping_factory.register_input(data);
        }
    }
}

/// Port of `InputSubsystem` class from main.h / main.cpp
pub struct InputSubsystem {
    imp: InputSubsystemImpl,
}

impl InputSubsystem {
    /// Port of InputSubsystem::InputSubsystem
    pub fn new() -> Self {
        Self {
            imp: InputSubsystemImpl::new(),
        }
    }

    /// Initializes and registers all built-in input device factories.
    /// Port of InputSubsystem::Initialize
    pub fn initialize(&mut self) {
        self.imp.initialize();
    }

    /// Unregisters all built-in input device factories and shuts them down.
    /// Port of InputSubsystem::Shutdown
    pub fn shutdown(&mut self) {
        self.imp.shutdown();
    }

    /// Retrieves the underlying keyboard device.
    /// Port of InputSubsystem::GetKeyboard
    pub fn get_keyboard(&self) -> Option<&Keyboard> {
        self.imp.keyboard.as_ref()
    }

    /// Retrieves the underlying keyboard device (mutable).
    pub fn get_keyboard_mut(&mut self) -> Option<&mut Keyboard> {
        self.imp.keyboard.as_mut()
    }

    /// Retrieves the underlying mouse device.
    /// Port of InputSubsystem::GetMouse
    pub fn get_mouse(&self) -> Option<&Mouse> {
        self.imp.mouse.as_ref()
    }

    /// Retrieves the underlying mouse device (mutable).
    pub fn get_mouse_mut(&mut self) -> Option<&mut Mouse> {
        self.imp.mouse.as_mut()
    }

    /// Retrieves the underlying touch screen device.
    /// Port of InputSubsystem::GetTouchScreen
    pub fn get_touch_screen(&self) -> Option<&TouchScreen> {
        self.imp.touch_screen.as_ref()
    }

    /// Retrieves the underlying touch screen device (mutable).
    pub fn get_touch_screen_mut(&mut self) -> Option<&mut TouchScreen> {
        self.imp.touch_screen.as_mut()
    }

    /// Retrieves the underlying TAS input device.
    /// Port of InputSubsystem::GetTas
    pub fn get_tas(&self) -> Option<&tas_input::Tas> {
        self.imp.tas_input.as_ref()
    }

    /// Retrieves the underlying TAS input device (mutable).
    pub fn get_tas_mut(&mut self) -> Option<&mut tas_input::Tas> {
        self.imp.tas_input.as_mut()
    }

    /// Retrieves the underlying camera input device.
    /// Port of InputSubsystem::GetCamera
    pub fn get_camera(&self) -> Option<&Camera> {
        self.imp.camera.as_ref()
    }

    /// Retrieves the underlying camera input device (mutable).
    pub fn get_camera_mut(&mut self) -> Option<&mut Camera> {
        self.imp.camera.as_mut()
    }

    /// Retrieves the underlying virtual amiibo input device.
    /// Port of InputSubsystem::GetVirtualAmiibo
    pub fn get_virtual_amiibo(&self) -> Option<&VirtualAmiibo> {
        self.imp.virtual_amiibo.as_ref()
    }

    /// Retrieves the underlying virtual amiibo input device (mutable).
    pub fn get_virtual_amiibo_mut(&mut self) -> Option<&mut VirtualAmiibo> {
        self.imp.virtual_amiibo.as_mut()
    }

    /// Retrieves the underlying virtual gamepad input device.
    /// Port of InputSubsystem::GetVirtualGamepad
    pub fn get_virtual_gamepad(&self) -> Option<&VirtualGamepad> {
        self.imp.virtual_gamepad.as_ref()
    }

    /// Retrieves the underlying virtual gamepad input device (mutable).
    pub fn get_virtual_gamepad_mut(&mut self) -> Option<&mut VirtualGamepad> {
        self.imp.virtual_gamepad.as_mut()
    }

    /// Returns all available input devices.
    /// Port of InputSubsystem::GetInputDevices
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        self.imp.get_input_devices()
    }

    /// Retrieves the analog mappings for the given device.
    /// Port of InputSubsystem::GetAnalogMappingForDevice
    pub fn get_analog_mapping_for_device(&self, _device: &ParamPackage) -> AnalogMapping {
        todo!()
    }

    /// Retrieves the button mappings for the given device.
    /// Port of InputSubsystem::GetButtonMappingForDevice
    pub fn get_button_mapping_for_device(&self, _device: &ParamPackage) -> ButtonMapping {
        todo!()
    }

    /// Retrieves the motion mappings for the given device.
    /// Port of InputSubsystem::GetMotionMappingForDevice
    pub fn get_motion_mapping_for_device(&self, _device: &ParamPackage) -> MotionMapping {
        todo!()
    }

    /// Returns an enum containing the name to be displayed from the input engine.
    /// Port of InputSubsystem::GetButtonName
    pub fn get_button_name(&self, _params: &ParamPackage) -> ButtonNames {
        todo!()
    }

    /// Returns true if device is a controller.
    /// Port of InputSubsystem::IsController
    pub fn is_controller(&self, _params: &ParamPackage) -> bool {
        todo!()
    }

    /// Returns true if axis of a stick aren't mapped in the correct direction.
    /// Port of InputSubsystem::IsStickInverted
    pub fn is_stick_inverted(&self, _device: &ParamPackage) -> bool {
        todo!()
    }

    /// Reloads the input devices.
    /// Port of InputSubsystem::ReloadInputDevices
    pub fn reload_input_devices(&mut self) {
        // impl->udp_client->ReloadSockets() in C++
        todo!()
    }

    /// Start polling from all backends for a desired input type.
    /// Port of InputSubsystem::BeginMapping
    pub fn begin_mapping(&mut self, input_type: Polling::InputType) {
        // Begin configuration on all engines, then start mapping
        if let Some(ref mut mapping_factory) = self.imp.mapping_factory {
            mapping_factory.begin_mapping(input_type);
        }
    }

    /// Returns an input event with mapping information.
    /// Port of InputSubsystem::GetNextInput
    pub fn get_next_input(&mut self) -> ParamPackage {
        if let Some(ref mut mapping_factory) = self.imp.mapping_factory {
            mapping_factory.get_next_input()
        } else {
            ParamPackage::default()
        }
    }

    /// Stop polling from all backends.
    /// Port of InputSubsystem::StopMapping
    pub fn stop_mapping(&mut self) {
        if let Some(ref mut mapping_factory) = self.imp.mapping_factory {
            mapping_factory.stop_mapping();
        }
    }

    /// Signals SDL driver for new input events.
    /// Port of InputSubsystem::PumpEvents
    pub fn pump_events(&mut self) {
        self.imp.pump_events();
    }
}

impl Default for InputSubsystem {
    fn default() -> Self {
        Self::new()
    }
}

/// Generates a serialized param package for creating a keyboard button device.
/// Port of GenerateKeyboardParam from main.cpp
pub fn generate_keyboard_param(key_code: i32) -> String {
    let mut param = ParamPackage::default();
    param.set_str("engine", "keyboard".to_string());
    param.set_int("code", key_code);
    param.set_str("toggle", "false".to_string());
    param.serialize()
}

/// Generates a serialized param package for creating an analog device taking input from keyboard.
/// Port of GenerateAnalogParamFromKeys from main.cpp
pub fn generate_analog_param_from_keys(
    key_up: i32,
    key_down: i32,
    key_left: i32,
    key_right: i32,
    key_modifier: i32,
    modifier_scale: f32,
) -> String {
    let mut circle_pad_param = ParamPackage::default();
    circle_pad_param.set_str("engine", "analog_from_button".to_string());
    circle_pad_param.set_str("up", generate_keyboard_param(key_up));
    circle_pad_param.set_str("down", generate_keyboard_param(key_down));
    circle_pad_param.set_str("left", generate_keyboard_param(key_left));
    circle_pad_param.set_str("right", generate_keyboard_param(key_right));
    circle_pad_param.set_str("modifier", generate_keyboard_param(key_modifier));
    circle_pad_param.set_str("modifier_scale", modifier_scale.to_string());
    circle_pad_param.serialize()
}
