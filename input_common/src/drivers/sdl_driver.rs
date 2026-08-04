// SPDX-FileCopyrightText: 2018 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/sdl_driver.h` and `input_common/drivers/sdl_driver.cpp`.
//!
//! SDL-based input driver for joysticks and game controllers.

use std::collections::{HashMap, VecDeque};
use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::JoinHandle;

use parking_lot::Mutex;
use sdl2::sys as sdl;

use common::input::{ButtonNames, DriverResult, VibrationStatus};
use common::param_package::ParamPackage;
use common::settings_input::{native_analog, native_button, native_motion};
use common::uuid::UUID;

use crate::drivers::sdl_joystick::{get_guid, SdlJoystick, SdlJoystickHandles};
use crate::input_engine::{
    InputEngine, InputEngineMetadata, InputEngineOutput, PadIdentifier, VibrationRequest,
};
use crate::main_common::{AnalogMapping, ButtonMapping, MotionMapping};

/// SDL HAT direction constants (matching SDL_HAT_* values).
const SDL_HAT_UP: u8 = 0x01;
const SDL_HAT_RIGHT: u8 = 0x02;
const SDL_HAT_DOWN: u8 = 0x04;
const SDL_HAT_LEFT: u8 = 0x08;

/// Upstream's vibration thread cadence.
const VIBRATION_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(10);

/// State shared between the driver and SDL's event watcher.
///
/// `SDL_AddEventWatch` takes a `void*` user pointer, so the state it points at
/// must not move for as long as the watch is installed. Keeping it behind an
/// `Arc` gives a stable address; the driver holds one reference and the watch
/// holds a leaked one, released in `Drop` after `SDL_DelEventWatch`.
struct SdlState {
    engine: Arc<Mutex<InputEngine>>,
    /// Devices grouped by GUID; the index within a group is the port, which is
    /// how upstream distinguishes several identical pads.
    joystick_map: Mutex<HashMap<UUID, Vec<Arc<Mutex<SdlJoystick>>>>>,
    vibration_queue: Mutex<VecDeque<VibrationRequest>>,
    initialized: AtomicBool,
}

/// `SDL_GameControllerGetBindForButton`.
fn bind_button(
    controller: *mut sdl::SDL_GameController,
    button: sdl::SDL_GameControllerButton,
) -> sdl::SDL_GameControllerButtonBind {
    unsafe { sdl::SDL_GameControllerGetBindForButton(controller, button) }
}

/// `SDL_GameControllerGetBindForAxis`.
fn bind_axis_raw(
    controller: *mut sdl::SDL_GameController,
    axis: sdl::SDL_GameControllerAxis,
) -> sdl::SDL_GameControllerButtonBind {
    unsafe { sdl::SDL_GameControllerGetBindForAxis(controller, axis) }
}

/// The hardware axis index behind a game-controller axis.
///
/// Upstream reads `binding.value.axis` straight out of the union without
/// checking `bindType`; when a stick axis is unbound SDL leaves the union zero,
/// so this reads 0 there too, matching upstream.
fn bind_axis(controller: *mut sdl::SDL_GameController, axis: sdl::SDL_GameControllerAxis) -> i32 {
    unsafe { bind_axis_raw(controller, axis).value.axis }
}

fn are_stick_axes_inverted(
    axis_x: i32,
    axis_y: i32,
    left_x: i32,
    right_x: i32,
    left_y: i32,
    right_y: i32,
) -> bool {
    (axis_x == left_y || axis_x == right_y) && (axis_y == left_x || axis_y == right_x)
}

/// Upstream `SDLDriver::IsButtonOnLeftSide` — which half of a dual Joy-Con
/// pair owns a given Switch button.
fn is_button_on_left_side(button: i32) -> bool {
    use native_button::Values as B;
    matches!(
        button,
        x if x == B::DDown as i32
            || x == B::DLeft as i32
            || x == B::DRight as i32
            || x == B::DUp as i32
            || x == B::L as i32
            || x == B::LStick as i32
            || x == B::Minus as i32
            || x == B::Screenshot as i32
            || x == B::ZL as i32
    )
}

/// Upstream's `SDLEventWatcher` free function.
///
/// SDL calls this for every event, on whichever thread pushed it.
unsafe extern "C" fn sdl_event_watcher(user_data: *mut c_void, event: *mut sdl::SDL_Event) -> i32 {
    if user_data.is_null() || event.is_null() {
        return 0;
    }
    let state = &*(user_data as *const SdlState);
    state.handle_game_controller_event(&*event);
    0
}

impl SdlState {
    /// Upstream `SDLDriver::GetSDLJoystickByGUID`.
    fn joystick_by_identifier(&self, identifier: &PadIdentifier) -> Arc<Mutex<SdlJoystick>> {
        let mut map = self.joystick_map.lock();
        let joysticks = map.entry(identifier.guid).or_default();
        while joysticks.len() <= identifier.port {
            joysticks.push(Arc::new(Mutex::new(SdlJoystick::new(
                identifier.guid,
                joysticks.len() as i32,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            ))));
        }
        Arc::clone(&joysticks[identifier.port])
    }

    /// Execute an SDL operation using a pointer snapshot after releasing the
    /// Rust joystick mutex. Upstream stores these pointers directly in the
    /// shared `SDLJoystick`; this helper only adapts that access to Rust's
    /// reconnect-state mutex without extending its lifetime across SDL calls.
    fn with_joystick_handles<R>(
        &self,
        identifier: &PadIdentifier,
        operation: impl FnOnce(SdlJoystickHandles) -> R,
    ) -> R {
        let joystick = self.joystick_by_identifier(identifier);
        let handles = {
            let guard = joystick.lock();
            guard.handles()
        };
        operation(handles)
    }

    /// Upstream `SDLDriver::GetSDLJoystickBySDLID`.
    fn joystick_by_sdl_id(&self, sdl_id: sdl::SDL_JoystickID) -> Option<Arc<Mutex<SdlJoystick>>> {
        let map = self.joystick_map.lock();
        for joysticks in map.values() {
            for joystick in joysticks {
                let handle = joystick.lock().sdl_joystick();
                if handle.is_null() {
                    continue;
                }
                if unsafe { sdl::SDL_JoystickInstanceID(handle) } == sdl_id {
                    return Some(Arc::clone(joystick));
                }
            }
        }
        None
    }

    /// Upstream `SDLDriver::HandleGameControllerEvent`.
    fn handle_game_controller_event(&self, event: &sdl::SDL_Event) {
        unsafe {
            match std::mem::transmute::<u32, sdl::SDL_EventType>(event.type_) {
                sdl::SDL_EventType::SDL_JOYBUTTONUP | sdl::SDL_EventType::SDL_JOYBUTTONDOWN => {
                    let pressed = event.type_ == sdl::SDL_EventType::SDL_JOYBUTTONDOWN as u32;
                    if let Some(joystick) = self.joystick_by_sdl_id(event.jbutton.which) {
                        let identifier = joystick.lock().pad_identifier();
                        let pending = self.engine.lock().set_button(
                            &identifier,
                            event.jbutton.button as i32,
                            pressed,
                        );
                        pending.dispatch();
                    }
                }
                sdl::SDL_EventType::SDL_JOYHATMOTION => {
                    if let Some(joystick) = self.joystick_by_sdl_id(event.jhat.which) {
                        let identifier = joystick.lock().pad_identifier();
                        let pending = self.engine.lock().set_hat_button(
                            &identifier,
                            event.jhat.hat as i32,
                            event.jhat.value,
                        );
                        pending.dispatch();
                    }
                }
                sdl::SDL_EventType::SDL_JOYAXISMOTION => {
                    if let Some(joystick) = self.joystick_by_sdl_id(event.jaxis.which) {
                        let identifier = joystick.lock().pad_identifier();
                        // Upstream divides by 32767 rather than 32768, so a
                        // full-scale axis reads exactly 1.0.
                        let pending = self.engine.lock().set_axis(
                            &identifier,
                            event.jaxis.axis as i32,
                            event.jaxis.value as f32 / 32767.0,
                        );
                        pending.dispatch();
                    }
                }
                sdl::SDL_EventType::SDL_CONTROLLERSENSORUPDATE => {
                    if let Some(joystick) = self.joystick_by_sdl_id(event.csensor.which) {
                        let sensor =
                            std::mem::transmute::<i32, sdl::SDL_SensorType>(event.csensor.sensor);
                        let mut guard = joystick.lock();
                        if guard.update_motion(sensor, event.csensor.timestamp, event.csensor.data)
                        {
                            let identifier = guard.pad_identifier();
                            let motion = guard.motion().clone();
                            drop(guard);
                            let pending = self.engine.lock().set_motion(&identifier, 0, &motion);
                            pending.dispatch();
                        }
                    }
                }
                sdl::SDL_EventType::SDL_JOYDEVICEREMOVED => {
                    log::debug!(
                        "Controller removed with instance id {}",
                        event.jdevice.which
                    );
                    self.close_joystick_by_instance_id(event.jdevice.which);
                }
                sdl::SDL_EventType::SDL_JOYDEVICEADDED => {
                    log::debug!(
                        "Controller connected with device index {}",
                        event.jdevice.which
                    );
                    self.init_joystick(event.jdevice.which);
                }
                _ => {}
            }
        }
    }

    /// Upstream `SDLDriver::InitJoystick`.
    fn init_joystick(&self, joystick_index: i32) {
        let sdl_joystick = unsafe { sdl::SDL_JoystickOpen(joystick_index) };
        if sdl_joystick.is_null() {
            log::error!("Failed to open joystick {joystick_index}");
            return;
        }
        let sdl_controller = unsafe {
            if sdl::SDL_IsGameController(joystick_index) == sdl::SDL_bool::SDL_TRUE {
                sdl::SDL_GameControllerOpen(joystick_index)
            } else {
                std::ptr::null_mut()
            }
        };

        let guid = get_guid(sdl_joystick);

        // Upstream hands Nintendo pads to the dedicated joycon/procon drivers
        // when those are enabled, identified by the vendor bytes in the GUID.
        let settings = common::settings::values();
        let is_nintendo = guid.uuid[5] == 0x05 && guid.uuid[4] == 0x7e;
        if *settings.enable_joycon_driver.get_value()
            && is_nintendo
            && (guid.uuid[8] == 0x06 || guid.uuid[8] == 0x07)
        {
            log::warn!("Preferring joycon driver for device index {joystick_index}");
            unsafe { sdl::SDL_JoystickClose(sdl_joystick) };
            return;
        }
        if *settings.enable_procon_driver.get_value() && is_nintendo && guid.uuid[8] == 0x09 {
            log::warn!("Preferring joycon driver for device index {joystick_index}");
            unsafe { sdl::SDL_JoystickClose(sdl_joystick) };
            return;
        }
        drop(settings);

        let mut map = self.joystick_map.lock();
        let group = map.entry(guid).or_default();

        // Reuse the slot of a device with this GUID that went away, so a
        // reconnected pad keeps the port its bindings refer to.
        if let Some(slot) = group
            .iter()
            .find(|joystick| joystick.lock().sdl_joystick().is_null())
        {
            let mut guard = slot.lock();
            guard.set_sdl_joystick(sdl_joystick, sdl_controller);
            guard.enable_motion();
            return;
        }

        let port = group.len() as i32;
        let joystick = SdlJoystick::new(guid, port, sdl_joystick, sdl_controller);
        let identifier = joystick.pad_identifier();
        group.push(Arc::new(Mutex::new(joystick)));
        drop(map);

        self.engine.lock().pre_set_controller(&identifier);
        log::info!(
            "Opened controller \"{}\" guid {} port {port}",
            unsafe {
                let name = sdl::SDL_JoystickName(sdl_joystick);
                if name.is_null() {
                    "Unknown".to_string()
                } else {
                    std::ffi::CStr::from_ptr(name)
                        .to_string_lossy()
                        .into_owned()
                }
            },
            guid.raw_string()
        );
    }

    /// Upstream `SDLDriver::CloseJoystick`, reached from `SDL_JOYDEVICEREMOVED`.
    ///
    /// The entry is kept but its handles are dropped, so the port stays
    /// reserved for the same physical device when it comes back.
    fn close_joystick_by_instance_id(&self, instance_id: sdl::SDL_JoystickID) {
        let map = self.joystick_map.lock();
        for joysticks in map.values() {
            for joystick in joysticks {
                let mut guard = joystick.lock();
                let handle = guard.sdl_joystick();
                if handle.is_null() {
                    continue;
                }
                if unsafe { sdl::SDL_JoystickInstanceID(handle) } == instance_id {
                    guard.set_sdl_joystick(std::ptr::null_mut(), std::ptr::null_mut());
                    return;
                }
            }
        }
    }

    /// Upstream `SDLDriver::SendVibrations`.
    fn send_vibrations(&self) {
        let mut filtered = Vec::<VibrationRequest>::new();
        {
            let mut queue = self.vibration_queue.lock();
            while let Some(request) = queue.pop_front() {
                if let Some(existing) = filtered
                    .iter_mut()
                    .find(|existing| existing.identifier == request.identifier)
                {
                    *existing = request;
                } else {
                    filtered.push(request);
                }
            }
        }
        for request in filtered {
            self.with_joystick_handles(&request.identifier, |handles| {
                handles.rumble_play(&request.vibration)
            });
        }
    }

    fn set_vibration(
        &self,
        identifier: &PadIdentifier,
        vibration: &VibrationStatus,
    ) -> DriverResult {
        let factor = if self.with_joystick_handles(identifier, |handles| handles.has_hd_rumble()) {
            1.0
        } else if vibration.amplification_type == common::input::VibrationAmplificationType::Linear
        {
            0.5
        } else {
            0.35
        };
        let process_amplitude_exp =
            |amplitude: f32| (amplitude + amplitude.powf(factor)) * 0.5 * u16::MAX as f32;
        self.vibration_queue.lock().push_back(VibrationRequest {
            identifier: identifier.clone(),
            vibration: VibrationStatus {
                low_amplitude: process_amplitude_exp(vibration.low_amplitude),
                low_frequency: vibration.low_frequency,
                high_amplitude: process_amplitude_exp(vibration.high_amplitude),
                high_frequency: vibration.high_frequency,
                amplification_type: common::input::VibrationAmplificationType::Exponential,
            },
        });
        DriverResult::Success
    }

    fn is_vibration_enabled(&self, identifier: &PadIdentifier) -> bool {
        let joystick = self.joystick_by_identifier(identifier);
        if joystick.lock().is_vibration_tested() {
            return joystick.lock().has_vibration();
        }

        let test_vibration = VibrationStatus {
            low_amplitude: 1.0,
            low_frequency: 160.0,
            high_amplitude: 1.0,
            high_frequency: 320.0,
            amplification_type: common::input::VibrationAmplificationType::Exponential,
        };
        let mut zero_vibration = test_vibration;
        zero_vibration.low_amplitude = 0.0;
        zero_vibration.high_amplitude = 0.0;

        self.with_joystick_handles(identifier, |handles| handles.rumble_play(&test_vibration));
        std::thread::sleep(std::time::Duration::from_millis(15));
        let enabled =
            self.with_joystick_handles(identifier, |handles| handles.rumble_play(&zero_vibration));
        joystick.lock().enable_vibration(enabled);
        enabled
    }
}

struct SdlOutput {
    state: std::sync::Weak<SdlState>,
}

struct SdlMetadata;

impl InputEngineMetadata for SdlMetadata {
    fn get_hat_button_name(&self, direction_value: u8) -> String {
        match direction_value {
            SDL_HAT_UP => "up".to_string(),
            SDL_HAT_DOWN => "down".to_string(),
            SDL_HAT_LEFT => "left".to_string(),
            SDL_HAT_RIGHT => "right".to_string(),
            _ => String::new(),
        }
    }

    fn get_hat_button_id(&self, direction_name: &str) -> u8 {
        match direction_name {
            "up" => SDL_HAT_UP,
            "down" => SDL_HAT_DOWN,
            "left" => SDL_HAT_LEFT,
            "right" => SDL_HAT_RIGHT,
            _ => 0,
        }
    }
}

impl InputEngineOutput for SdlOutput {
    fn set_vibration(
        &self,
        identifier: &PadIdentifier,
        vibration: &VibrationStatus,
    ) -> DriverResult {
        self.state
            .upgrade()
            .map_or(DriverResult::NoDeviceDetected, |state| {
                state.set_vibration(identifier, vibration)
            })
    }

    fn is_vibration_enabled(&self, identifier: &PadIdentifier) -> bool {
        self.state
            .upgrade()
            .is_some_and(|state| state.is_vibration_enabled(identifier))
    }
}

/// Port of `SDLDriver` class from sdl_driver.h / sdl_driver.cpp
pub struct SDLDriver {
    state: Arc<SdlState>,
    /// Set when this driver owns the SDL joystick subsystem, i.e. the frontend
    /// had not already initialised it.
    start_thread: bool,
    vibration_thread: Option<JoinHandle<()>>,
    /// The leaked `Arc` handed to `SDL_AddEventWatch`, reclaimed in `Drop`.
    watch_user_data: *const SdlState,
}

// SAFETY: every field is either `Send` or an `Arc` whose contents are guarded
// by mutexes; the raw pointer is only used to unregister the event watch.
unsafe impl Send for SDLDriver {}

impl SDLDriver {
    /// Port of SDLDriver::SDLDriver
    pub fn new(input_engine: String) -> Self {
        let state = Arc::new(SdlState {
            engine: Arc::new(Mutex::new(InputEngine::new(input_engine))),
            joystick_map: Mutex::new(HashMap::new()),
            vibration_queue: Mutex::new(VecDeque::new()),
            initialized: AtomicBool::new(false),
        });
        state.engine.lock().set_output_handler(Arc::new(SdlOutput {
            state: Arc::downgrade(&state),
        }));
        state
            .engine
            .lock()
            .set_metadata_handler(Arc::new(SdlMetadata));

        Self::set_hints();

        // If the frontend already runs an SDL event loop we must not start a
        // second one — upstream makes the same check.
        let already_initialized =
            unsafe { sdl::SDL_WasInit(sdl::SDL_INIT_JOYSTICK | sdl::SDL_INIT_GAMECONTROLLER) != 0 };
        let start_thread = !already_initialized;
        if start_thread {
            let result =
                unsafe { sdl::SDL_Init(sdl::SDL_INIT_JOYSTICK | sdl::SDL_INIT_GAMECONTROLLER) };
            if result < 0 {
                let error = unsafe { std::ffi::CStr::from_ptr(sdl::SDL_GetError()) };
                log::error!("SDL_Init failed with: {}", error.to_string_lossy());
                return Self {
                    state,
                    start_thread,
                    vibration_thread: None,
                    watch_user_data: std::ptr::null(),
                };
            }
        }

        // Hand the watch a stable pointer that outlives this call.
        let watch_user_data = Arc::into_raw(Arc::clone(&state));
        unsafe {
            sdl::SDL_AddEventWatch(Some(sdl_event_watcher), watch_user_data as *mut c_void);
        }

        state.initialized.store(true, Ordering::Release);

        let vibration_thread = start_thread.then(|| {
            let state = Arc::clone(&state);
            std::thread::Builder::new()
                .name("SDL_Vibration".to_string())
                .spawn(move || {
                    while state.initialized.load(Ordering::Acquire) {
                        state.send_vibrations();
                        std::thread::sleep(VIBRATION_POLL_INTERVAL);
                    }
                })
                .expect("failed to spawn SDL_Vibration thread")
        });

        // Connection events for pads plugged in before the watch was installed
        // have already been consumed, so open everything present right now.
        let count = unsafe { sdl::SDL_NumJoysticks() };
        for index in 0..count {
            state.init_joystick(index);
        }

        Self {
            state,
            start_thread,
            vibration_thread,
            watch_user_data,
        }
    }

    /// The `SDL_SetHint` block from upstream's constructor.
    fn set_hints() {
        let hint = |name: &str, value: &str| {
            let name = std::ffi::CString::new(name).unwrap();
            let value = std::ffi::CString::new(value).unwrap();
            unsafe { sdl::SDL_SetHint(name.as_ptr(), value.as_ptr()) };
        };

        hint("SDL_APP_NAME", "ruzu");

        let settings = common::settings::values();
        if !*settings.enable_raw_input.get_value() {
            // Raw input makes SDL die when a web applet opens.
            hint("SDL_JOYSTICK_RAWINPUT", "0");
        }
        // Prevent SDL from adding an undesired axis.
        hint("SDL_ACCELEROMETER_AS_JOYSTICK", "0");
        // Keep motion alive on PS4/PS5 pads.
        hint("SDL_JOYSTICK_HIDAPI_PS4_RUMBLE", "1");
        hint("SDL_JOYSTICK_HIDAPI_PS5_RUMBLE", "1");
        hint("SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS", "1");

        if *settings.enable_joycon_driver.get_value() {
            hint("SDL_JOYSTICK_HIDAPI_JOY_CONS", "0");
        } else {
            hint("SDL_JOYSTICK_HIDAPI_JOY_CONS", "1");
            hint("SDL_JOYSTICK_HIDAPI_JOYCON_HOME_LED", "0");
            hint("SDL_JOYSTICK_HIDAPI_COMBINE_JOY_CONS", "0");
            hint("SDL_JOYSTICK_HIDAPI_VERTICAL_JOY_CONS", "1");
        }
        if *settings.enable_procon_driver.get_value() {
            hint("SDL_JOYSTICK_HIDAPI_SWITCH", "0");
        } else {
            hint("SDL_JOYSTICK_HIDAPI_SWITCH", "1");
            hint("SDL_JOYSTICK_HIDAPI_SWITCH_HOME_LED", "0");
        }
        hint("SDL_JOYSTICK_HIDAPI_SWITCH_PLAYER_LED", "1");
        // Share the same button mapping with non-Nintendo controllers.
        hint("SDL_GAMECONTROLLER_USE_BUTTON_LABELS", "0");
    }

    /// Port of SDLDriver::PumpEvents
    pub fn pump_events(&self) {
        if self.state.initialized.load(Ordering::Acquire) {
            unsafe { sdl::SDL_PumpEvents() };
        }
    }

    /// Port of SDLDriver::GetInputDevices (override)
    pub fn get_input_devices(&self) -> Vec<ParamPackage> {
        let mut devices = Vec::new();
        // Upstream keeps a per-name counter so two controllers of the same
        // model get distinct display names ("Xbox One Controller 0/1").
        let mut name_counter: HashMap<String, i32> = HashMap::new();
        let mut joycon_pairs: HashMap<i32, (UUID, i32)> = HashMap::new();

        let map = self.state.joystick_map.lock();
        for joysticks in map.values() {
            for joystick in joysticks {
                let guard = joystick.lock();
                if guard.sdl_joystick().is_null() {
                    continue;
                }
                let controller_name = guard.controller_name();
                let display_index = name_counter.entry(controller_name.clone()).or_insert(0);
                let name = format!("{controller_name} {display_index}");
                *display_index += 1;

                let mut params = ParamPackage::default();
                params.set_str("engine", self.engine_name());
                params.set_str("display", name);
                params.set_str("guid", guard.guid().raw_string());
                params.set_str("port", guard.port().to_string());
                devices.push(params);

                if guard.is_joycon_left() {
                    joycon_pairs.insert(guard.port(), (guard.guid(), guard.port()));
                }
            }
        }

        // Upstream then adds a synthetic "Nintendo Dual Joy-Con" entry for each
        // right Joy-Con whose port already holds a left one.
        for joysticks in map.values() {
            for joystick in joysticks {
                let guard = joystick.lock();
                if !guard.is_joycon_right() {
                    continue;
                }
                let Some((left_guid, _)) = joycon_pairs.get(&guard.port()) else {
                    continue;
                };
                let mut params = ParamPackage::default();
                params.set_str("engine", self.engine_name());
                params.set_str("display", format!("Nintendo Dual Joy-Con {}", guard.port()));
                params.set_str("guid", guard.guid().raw_string());
                params.set_str("guid2", left_guid.raw_string());
                params.set_str("port", guard.port().to_string());
                devices.push(params);
            }
        }

        devices
    }

    /// Port of SDLDriver::GetButtonMappingForDevice (override)
    pub fn get_button_mapping_for_device(&self, params: &ParamPackage) -> ButtonMapping {
        if !params.has("guid") || !params.has("port") {
            return ButtonMapping::new();
        }
        let port = params.get_int("port", 0) as i32;
        let Some(joystick) = self.joystick_by_guid(&params.get_str("guid", ""), port) else {
            return ButtonMapping::new();
        };
        let controller = joystick.lock().sdl_game_controller();
        if controller.is_null() {
            return ButtonMapping::new();
        }

        let switch_to_sdl_button = self.default_button_binding(&joystick);

        // ZL/ZR are axes, not buttons, in SDL's game-controller model.
        const SWITCH_TO_SDL_AXIS: [(i32, sdl::SDL_GameControllerAxis); 2] = [
            (
                native_button::Values::ZL as i32,
                sdl::SDL_GameControllerAxis::SDL_CONTROLLER_AXIS_TRIGGERLEFT,
            ),
            (
                native_button::Values::ZR as i32,
                sdl::SDL_GameControllerAxis::SDL_CONTROLLER_AXIS_TRIGGERRIGHT,
            ),
        ];

        // A dual Joy-Con device carries a second GUID; the left-hand buttons
        // then come from the second controller.
        if params.has("guid2") {
            if let Some(joystick2) = self.joystick_by_guid(&params.get_str("guid2", ""), port) {
                if !joystick2.lock().sdl_game_controller().is_null() {
                    return self.dual_controller_mapping(
                        &joystick,
                        &joystick2,
                        &switch_to_sdl_button,
                        &SWITCH_TO_SDL_AXIS,
                    );
                }
            }
        }

        self.single_controller_mapping(&joystick, &switch_to_sdl_button, &SWITCH_TO_SDL_AXIS)
    }

    /// Port of SDLDriver::GetAnalogMappingForDevice (override)
    pub fn get_analog_mapping_for_device(&self, params: &ParamPackage) -> AnalogMapping {
        if !params.has("guid") || !params.has("port") {
            return AnalogMapping::new();
        }
        let port = params.get_int("port", 0) as i32;
        let Some(joystick) = self.joystick_by_guid(&params.get_str("guid", ""), port) else {
            return AnalogMapping::new();
        };
        let controller = joystick.lock().sdl_game_controller();
        if controller.is_null() {
            return AnalogMapping::new();
        }

        let mut mapping = AnalogMapping::new();
        use sdl::SDL_GameControllerAxis as Axis;

        let left_x = bind_axis(controller, Axis::SDL_CONTROLLER_AXIS_LEFTX);
        let left_y = bind_axis(controller, Axis::SDL_CONTROLLER_AXIS_LEFTY);

        // The left stick belongs to the second device on a dual Joy-Con.
        let left_source = if params.has("guid2") {
            self.joystick_by_guid(&params.get_str("guid2", ""), port)
                .unwrap_or_else(|| Arc::clone(&joystick))
        } else {
            Arc::clone(&joystick)
        };
        let left_identifier = left_source.lock().pad_identifier();
        mapping.insert(
            native_analog::Values::LStick as i32,
            self.build_analog_param(&left_identifier, left_x, left_y),
        );

        let right_x = bind_axis(controller, Axis::SDL_CONTROLLER_AXIS_RIGHTX);
        let right_y = bind_axis(controller, Axis::SDL_CONTROLLER_AXIS_RIGHTY);
        let right_identifier = joystick.lock().pad_identifier();
        mapping.insert(
            native_analog::Values::RStick as i32,
            self.build_analog_param(&right_identifier, right_x, right_y),
        );

        mapping
    }

    /// Port of SDLDriver::GetMotionMappingForDevice (override)
    pub fn get_motion_mapping_for_device(&self, params: &ParamPackage) -> MotionMapping {
        if !params.has("guid") || !params.has("port") {
            return MotionMapping::new();
        }
        let port = params.get_int("port", 0) as i32;
        let Some(joystick) = self.joystick_by_guid(&params.get_str("guid", ""), port) else {
            return MotionMapping::new();
        };
        if joystick.lock().sdl_game_controller().is_null() {
            return MotionMapping::new();
        }

        let mut mapping = MotionMapping::new();
        joystick.lock().enable_motion();

        let (has_motion, guid, port_value) = {
            let guard = joystick.lock();
            (guard.has_motion(), guard.guid(), guard.port())
        };
        if has_motion {
            mapping.insert(
                native_motion::Values::MotionRight as i32,
                self.build_motion_param(port_value, &guid),
            );
        }

        if params.has("guid2") {
            if let Some(joystick2) = self.joystick_by_guid(&params.get_str("guid2", ""), port) {
                joystick2.lock().enable_motion();
                let guard = joystick2.lock();
                if guard.has_motion() {
                    let param = self.build_motion_param(guard.port(), &guard.guid());
                    drop(guard);
                    mapping.insert(native_motion::Values::MotionLeft as i32, param);
                }
            }
        } else if has_motion {
            mapping.insert(
                native_motion::Values::MotionLeft as i32,
                self.build_motion_param(port_value, &guid),
            );
        }

        mapping
    }

    /// The engine name this driver registered under, upstream's
    /// `InputEngine::GetEngineName()`.
    fn engine_name(&self) -> String {
        self.state.engine.lock().get_engine_name().to_string()
    }

    /// Upstream `SDLDriver::GetSDLJoystickByGUID`.
    fn joystick_by_guid(&self, guid: &str, port: i32) -> Option<Arc<Mutex<SdlJoystick>>> {
        let uuid = UUID::from_string(guid);
        let map = self.state.joystick_map.lock();
        map.get(&uuid)
            .and_then(|joysticks| joysticks.get(port as usize))
            .map(Arc::clone)
    }

    /// Upstream `SDLDriver::GetDefaultButtonBinding`.
    ///
    /// Note A/B and X/Y are crossed: the Switch layout puts A where SDL puts B.
    fn default_button_binding(
        &self,
        joystick: &Arc<Mutex<SdlJoystick>>,
    ) -> Vec<(i32, sdl::SDL_GameControllerButton)> {
        use native_button::Values as B;
        use sdl::SDL_GameControllerButton as S;

        let (is_left, is_right) = {
            let guard = joystick.lock();
            (guard.is_joycon_left(), guard.is_joycon_right())
        };

        // Joy-Cons expose SL/SR as paddles; everything else falls back to the
        // shoulder buttons.
        let mut sll = S::SDL_CONTROLLER_BUTTON_LEFTSHOULDER;
        let mut srl = S::SDL_CONTROLLER_BUTTON_RIGHTSHOULDER;
        let mut slr = S::SDL_CONTROLLER_BUTTON_LEFTSHOULDER;
        let mut srr = S::SDL_CONTROLLER_BUTTON_RIGHTSHOULDER;
        if is_left {
            sll = S::SDL_CONTROLLER_BUTTON_PADDLE2;
            srl = S::SDL_CONTROLLER_BUTTON_PADDLE4;
        }
        if is_right {
            slr = S::SDL_CONTROLLER_BUTTON_PADDLE3;
            srr = S::SDL_CONTROLLER_BUTTON_PADDLE1;
        }

        vec![
            (B::A as i32, S::SDL_CONTROLLER_BUTTON_B),
            (B::B as i32, S::SDL_CONTROLLER_BUTTON_A),
            (B::X as i32, S::SDL_CONTROLLER_BUTTON_Y),
            (B::Y as i32, S::SDL_CONTROLLER_BUTTON_X),
            (B::LStick as i32, S::SDL_CONTROLLER_BUTTON_LEFTSTICK),
            (B::RStick as i32, S::SDL_CONTROLLER_BUTTON_RIGHTSTICK),
            (B::L as i32, S::SDL_CONTROLLER_BUTTON_LEFTSHOULDER),
            (B::R as i32, S::SDL_CONTROLLER_BUTTON_RIGHTSHOULDER),
            (B::Plus as i32, S::SDL_CONTROLLER_BUTTON_START),
            (B::Minus as i32, S::SDL_CONTROLLER_BUTTON_BACK),
            (B::DLeft as i32, S::SDL_CONTROLLER_BUTTON_DPAD_LEFT),
            (B::DUp as i32, S::SDL_CONTROLLER_BUTTON_DPAD_UP),
            (B::DRight as i32, S::SDL_CONTROLLER_BUTTON_DPAD_RIGHT),
            (B::DDown as i32, S::SDL_CONTROLLER_BUTTON_DPAD_DOWN),
            (B::SLLeft as i32, sll),
            (B::SRLeft as i32, srl),
            (B::SLRight as i32, slr),
            (B::SRRight as i32, srr),
            (B::Home as i32, S::SDL_CONTROLLER_BUTTON_GUIDE),
            (B::Screenshot as i32, S::SDL_CONTROLLER_BUTTON_MISC1),
        ]
    }

    /// Upstream `SDLDriver::GetSingleControllerMapping`.
    fn single_controller_mapping(
        &self,
        joystick: &Arc<Mutex<SdlJoystick>>,
        switch_to_sdl_button: &[(i32, sdl::SDL_GameControllerButton)],
        switch_to_sdl_axis: &[(i32, sdl::SDL_GameControllerAxis)],
    ) -> ButtonMapping {
        let mut mapping = ButtonMapping::new();
        let (controller, port, guid) = {
            let guard = joystick.lock();
            (guard.sdl_game_controller(), guard.port(), guard.guid())
        };

        for &(switch_button, sdl_button) in switch_to_sdl_button {
            let binding = bind_button(controller, sdl_button);
            mapping.insert(
                switch_button,
                self.build_param_for_binding(port, &guid, binding),
            );
        }
        for &(switch_button, sdl_axis) in switch_to_sdl_axis {
            let binding = bind_axis_raw(controller, sdl_axis);
            mapping.insert(
                switch_button,
                self.build_param_for_binding(port, &guid, binding),
            );
        }

        mapping
    }

    /// Upstream `SDLDriver::GetDualControllerMapping`.
    fn dual_controller_mapping(
        &self,
        joystick: &Arc<Mutex<SdlJoystick>>,
        joystick2: &Arc<Mutex<SdlJoystick>>,
        switch_to_sdl_button: &[(i32, sdl::SDL_GameControllerButton)],
        switch_to_sdl_axis: &[(i32, sdl::SDL_GameControllerAxis)],
    ) -> ButtonMapping {
        let mut mapping = ButtonMapping::new();
        let (controller, port, guid) = {
            let guard = joystick.lock();
            (guard.sdl_game_controller(), guard.port(), guard.guid())
        };
        let (controller2, port2, guid2) = {
            let guard = joystick2.lock();
            (guard.sdl_game_controller(), guard.port(), guard.guid())
        };

        for &(switch_button, sdl_button) in switch_to_sdl_button {
            let left = is_button_on_left_side(switch_button);
            let binding = bind_button(if left { controller2 } else { controller }, sdl_button);
            let (p, g) = if left { (port2, &guid2) } else { (port, &guid) };
            mapping.insert(switch_button, self.build_param_for_binding(p, g, binding));
        }
        for &(switch_button, sdl_axis) in switch_to_sdl_axis {
            let left = is_button_on_left_side(switch_button);
            let binding = bind_axis_raw(if left { controller2 } else { controller }, sdl_axis);
            let (p, g) = if left { (port2, &guid2) } else { (port, &guid) };
            mapping.insert(switch_button, self.build_param_for_binding(p, g, binding));
        }

        mapping
    }

    /// Upstream `SDLDriver::BuildParamPackageForBinding`.
    fn build_param_for_binding(
        &self,
        port: i32,
        guid: &UUID,
        binding: sdl::SDL_GameControllerButtonBind,
    ) -> ParamPackage {
        unsafe {
            match binding.bindType {
                sdl::SDL_GameControllerBindType::SDL_CONTROLLER_BINDTYPE_AXIS => {
                    // Upstream calls the one-argument overload here, whose
                    // `value` defaults to 0 — so `invert` is always "+".
                    self.build_analog_param_for_button(port, guid, binding.value.axis, 0.0)
                }
                sdl::SDL_GameControllerBindType::SDL_CONTROLLER_BINDTYPE_BUTTON => {
                    self.build_button_param_for_button(port, guid, binding.value.button)
                }
                sdl::SDL_GameControllerBindType::SDL_CONTROLLER_BINDTYPE_HAT => self
                    .build_hat_param_for_button(
                        port,
                        guid,
                        binding.value.hat.hat,
                        binding.value.hat.hat_mask as u8,
                    ),
                // SDL_CONTROLLER_BINDTYPE_NONE: upstream returns an empty
                // package, which the UI renders as "[not set]".
                _ => ParamPackage::default(),
            }
        }
    }

    /// Upstream `SDLDriver::BuildAnalogParamPackageForButton`.
    fn build_analog_param_for_button(
        &self,
        port: i32,
        guid: &UUID,
        axis: i32,
        value: f32,
    ) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str("engine", self.engine_name());
        params.set_str("port", port.to_string());
        params.set_str("guid", guid.raw_string());
        params.set_str("axis", axis.to_string());
        params.set_str("threshold", "0.5".to_string());
        params.set_str("invert", if value < 0.0 { "-" } else { "+" }.to_string());
        params
    }

    /// Upstream `SDLDriver::BuildButtonParamPackageForButton`.
    fn build_button_param_for_button(&self, port: i32, guid: &UUID, button: i32) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str("engine", self.engine_name());
        params.set_str("port", port.to_string());
        params.set_str("guid", guid.raw_string());
        params.set_str("button", button.to_string());
        params
    }

    /// Upstream `SDLDriver::BuildHatParamPackageForButton`.
    fn build_hat_param_for_button(
        &self,
        port: i32,
        guid: &UUID,
        hat: i32,
        value: u8,
    ) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str("engine", self.engine_name());
        params.set_str("port", port.to_string());
        params.set_str("guid", guid.raw_string());
        params.set_str("hat", hat.to_string());
        params.set_str("direction", self.get_hat_button_name(value));
        params
    }

    /// Upstream `SDLDriver::BuildParamPackageForAnalog`.
    ///
    /// The offsets are the axis readings at rest, so a stick that does not
    /// centre exactly still reads zero. Sampling them needs the engine's
    /// current axis state, which upstream primes with `PreSetAxis` first.
    fn build_analog_param(
        &self,
        identifier: &PadIdentifier,
        axis_x: i32,
        axis_y: i32,
    ) -> ParamPackage {
        let (offset_x, offset_y) = {
            let mut engine = self.state.engine.lock();
            engine.pre_set_controller(identifier);
            engine.pre_set_axis(identifier, axis_x);
            engine.pre_set_axis(identifier, axis_y);
            (
                -engine.get_axis(identifier, axis_x),
                engine.get_axis(identifier, axis_y),
            )
        };

        let mut params = ParamPackage::default();
        params.set_str("engine", self.engine_name());
        params.set_str("port", identifier.port.to_string());
        params.set_str("guid", identifier.guid.raw_string());
        params.set_str("axis_x", axis_x.to_string());
        params.set_str("axis_y", axis_y.to_string());
        params.set_str("offset_x", offset_x.to_string());
        params.set_str("offset_y", offset_y.to_string());
        params.set_str("invert_x", "+".to_string());
        params.set_str("invert_y", "+".to_string());
        params
    }

    /// Port of SDLDriver::GetUIName (override)
    pub fn get_ui_name(&self, params: &ParamPackage) -> ButtonNames {
        if params.has("button") {
            // Upstream TODO(German77): Find how to substitute the values for real button names
            return ButtonNames::Value;
        }
        if params.has("hat") {
            return ButtonNames::Value;
        }
        if params.has("axis") {
            return ButtonNames::Value;
        }
        if params.has("axis_x") && params.has("axis_y") && params.has("axis_z") {
            return ButtonNames::Value;
        }
        if params.has("motion") {
            return ButtonNames::Engine;
        }
        ButtonNames::Invalid
    }

    /// Port of SDLDriver::GetHatButtonName (override)
    pub fn get_hat_button_name(&self, direction_value: u8) -> String {
        SdlMetadata.get_hat_button_name(direction_value)
    }

    /// Port of SDLDriver::GetHatButtonId (override)
    pub fn get_hat_button_id(&self, direction_name: &str) -> u8 {
        SdlMetadata.get_hat_button_id(direction_name)
    }

    /// Port of SDLDriver::IsStickInverted (override)
    pub fn is_stick_inverted(&self, params: &ParamPackage) -> bool {
        if !params.has("guid") || !params.has("port") {
            return false;
        }
        let Some(joystick) =
            self.joystick_by_guid(&params.get_str("guid", ""), params.get_int("port", 0))
        else {
            return false;
        };
        let controller = {
            let guard = joystick.lock();
            guard.sdl_game_controller()
        };
        if controller.is_null() {
            return false;
        }

        let axis_x = params.get_int("axis_x", 0);
        let axis_y = params.get_int("axis_y", 0);
        are_stick_axes_inverted(
            axis_x,
            axis_y,
            bind_axis(
                controller,
                sdl::SDL_GameControllerAxis::SDL_CONTROLLER_AXIS_LEFTX,
            ),
            bind_axis(
                controller,
                sdl::SDL_GameControllerAxis::SDL_CONTROLLER_AXIS_RIGHTX,
            ),
            bind_axis(
                controller,
                sdl::SDL_GameControllerAxis::SDL_CONTROLLER_AXIS_LEFTY,
            ),
            bind_axis(
                controller,
                sdl::SDL_GameControllerAxis::SDL_CONTROLLER_AXIS_RIGHTY,
            ),
        )
    }

    /// Port of SDLDriver::SetVibration (override)
    pub fn set_vibration(
        &mut self,
        identifier: &PadIdentifier,
        vibration: &VibrationStatus,
    ) -> DriverResult {
        self.state.set_vibration(identifier, vibration)
    }

    /// Port of SDLDriver::IsVibrationEnabled (override)
    pub fn is_vibration_enabled(&self, identifier: &PadIdentifier) -> bool {
        self.state.is_vibration_enabled(identifier)
    }

    // ---- Private methods ----

    /// Port of SDLDriver::CloseJoysticks
    fn close_joysticks(&mut self) {
        // Dropping each `SdlJoystick` runs its `Drop`, which is where upstream's
        // `unique_ptr` deleters call SDL_JoystickClose / SDL_GameControllerClose.
        self.state.joystick_map.lock().clear();
    }

    /// Port of SDLDriver::IsButtonOnLeftSide
    fn is_button_on_left_side(&self, button: i32) -> bool {
        matches!(
            button,
            x if x == native_button::Values::DDown as i32
                || x == native_button::Values::DLeft as i32
                || x == native_button::Values::DRight as i32
                || x == native_button::Values::DUp as i32
                || x == native_button::Values::L as i32
                || x == native_button::Values::LStick as i32
                || x == native_button::Values::Minus as i32
                || x == native_button::Values::Screenshot as i32
                || x == native_button::Values::ZL as i32
        )
    }

    /// Helper: build an analog param package for a button.
    /// Port of SDLDriver::BuildAnalogParamPackageForButton
    fn build_analog_param_package_for_button(
        &self,
        port: i32,
        guid: &UUID,
        axis: i32,
        value: f32,
    ) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str(
            "engine",
            self.state.engine.lock().get_engine_name().to_string(),
        );
        params.set_str("guid", guid.raw_string());
        params.set_int("port", port);
        params.set_int("axis", axis);
        if value > 0.0 {
            params.set_str("direction", "+".to_string());
        } else {
            params.set_str("direction", "-".to_string());
        }
        params.set_float("threshold", 0.5);
        params
    }

    /// Helper: build a button param package.
    /// Port of SDLDriver::BuildButtonParamPackageForButton
    fn build_button_param_package_for_button(
        &self,
        port: i32,
        guid: &UUID,
        button: i32,
    ) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str(
            "engine",
            self.state.engine.lock().get_engine_name().to_string(),
        );
        params.set_str("guid", guid.raw_string());
        params.set_int("port", port);
        params.set_int("button", button);
        params
    }

    /// Helper: build a hat param package.
    /// Port of SDLDriver::BuildHatParamPackageForButton
    fn build_hat_param_package_for_button(
        &self,
        port: i32,
        guid: &UUID,
        hat: i32,
        value: u8,
    ) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str(
            "engine",
            self.state.engine.lock().get_engine_name().to_string(),
        );
        params.set_str("guid", guid.raw_string());
        params.set_int("port", port);
        params.set_int("hat", hat);
        params.set_str("direction", self.get_hat_button_name(value));
        params
    }

    /// Helper: build a motion param package.
    /// Port of SDLDriver::BuildMotionParam
    fn build_motion_param(&self, port: i32, guid: &UUID) -> ParamPackage {
        let mut params = ParamPackage::default();
        params.set_str(
            "engine",
            self.state.engine.lock().get_engine_name().to_string(),
        );
        params.set_str("guid", guid.raw_string());
        params.set_int("port", port);
        params.set_int("motion", 0);
        params
    }

    /// Shared handle to the underlying input engine, for factory registration.
    pub fn engine(&self) -> Arc<Mutex<InputEngine>> {
        Arc::clone(&self.state.engine)
    }
}

impl Drop for SDLDriver {
    /// Mirrors upstream's destructor: stop the vibration thread, remove the
    /// event watch, then close every device.
    fn drop(&mut self) {
        self.state.initialized.store(false, Ordering::Release);
        if let Some(thread) = self.vibration_thread.take() {
            let _ = thread.join();
        }
        if !self.watch_user_data.is_null() {
            unsafe {
                sdl::SDL_DelEventWatch(
                    Some(sdl_event_watcher),
                    self.watch_user_data as *mut c_void,
                );
                // Reclaim the reference leaked for the watch.
                drop(Arc::from_raw(self.watch_user_data));
            }
            self.watch_user_data = std::ptr::null();
        }
        self.close_joysticks();
        if self.start_thread {
            unsafe {
                sdl::SDL_QuitSubSystem(sdl::SDL_INIT_JOYSTICK | sdl::SDL_INIT_GAMECONTROLLER)
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::input::VibrationAmplificationType;

    fn test_state() -> SdlState {
        SdlState {
            engine: Arc::new(Mutex::new(InputEngine::new("sdl_test".to_string()))),
            joystick_map: Mutex::new(HashMap::new()),
            vibration_queue: Mutex::new(VecDeque::new()),
            initialized: AtomicBool::new(false),
        }
    }

    #[test]
    fn set_vibration_applies_upstream_sdl_amplitude_curve() {
        let state = test_state();
        let identifier = PadIdentifier::default();
        let vibration = VibrationStatus {
            low_amplitude: 1.0,
            low_frequency: 160.0,
            high_amplitude: 0.25,
            high_frequency: 320.0,
            amplification_type: VibrationAmplificationType::Exponential,
        };

        assert_eq!(
            state.set_vibration(&identifier, &vibration),
            DriverResult::Success
        );
        let queued = state.vibration_queue.lock().pop_front().unwrap();
        assert_eq!(queued.vibration.low_amplitude, u16::MAX as f32);
        let expected_high = (0.25_f32 + 0.25_f32.powf(0.35)) * 0.5 * u16::MAX as f32;
        assert_eq!(queued.vibration.high_amplitude, expected_high);
        assert_eq!(
            queued.vibration.amplification_type,
            VibrationAmplificationType::Exponential
        );
    }

    #[test]
    fn sdl_operation_does_not_hold_joystick_mutex() {
        let state = test_state();
        let identifier = PadIdentifier::default();
        let joystick = state.joystick_by_identifier(&identifier);

        state.with_joystick_handles(&identifier, |_| {
            assert!(
                joystick.try_lock().is_some(),
                "SDL calls must run after releasing the Rust joystick mutex"
            );
        });
    }

    #[test]
    fn inverted_stick_requires_crossed_x_and_y_bindings() {
        assert!(are_stick_axes_inverted(3, 1, 1, 2, 3, 4));
        assert!(are_stick_axes_inverted(4, 2, 1, 2, 3, 4));
        assert!(!are_stick_axes_inverted(1, 3, 1, 2, 3, 4));
        assert!(!are_stick_axes_inverted(3, 8, 1, 2, 3, 4));
    }
}
