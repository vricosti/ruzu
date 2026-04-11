//! Port of zuyu/src/core/hle/service/hid/hid.h and hid.cpp
//!
//! Entry point for the HID service module.

use std::sync::Arc;
use std::time::Duration;

use hid_core::hid_core::HIDCore;
use hid_core::resource_manager::{
    ResourceManager, DEFAULT_UPDATE_NS, MOTION_UPDATE_NS, MOUSE_KEYBOARD_UPDATE_NS, NPAD_UPDATE_NS,
};
use hid_core::resources::hid_firmware_settings::HidFirmwareSettings;

use crate::core_timing;
use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
use crate::hle::service::server_manager::ServerManager;

/// Named services registered by the HID module:
/// - "hid"      -> IHidServer
/// - "hid:dbg"  -> IHidDebugServer
/// - "hid:sys"  -> IHidSystemServer
/// - "hidbus"   -> Hidbus
/// - "irs"      -> IRS
/// - "irs:sys"  -> IRS_SYS
/// - "xcd:sys"  -> XCD_SYS
pub fn loop_process(system: crate::core::SystemRef) {
    let firmware_settings = Arc::new(HidFirmwareSettings::new());
    let hid_core = Arc::new(parking_lot::Mutex::new(HIDCore::new()));
    let resource_manager = Arc::new(parking_lot::Mutex::new(ResourceManager::new(
        firmware_settings.clone(),
        hid_core,
    )));

    resource_manager.lock().initialize();

    let core_timing = system.get().core_timing();
    let npad_update_event = core_timing::create_event(
        "HID::UpdatePadCallback".to_string(),
        Box::new({
            let resource_manager = resource_manager.clone();
            move |_time, ns_late| {
                resource_manager.lock().update_npad(ns_late);
                None
            }
        }),
    );
    let default_update_event = core_timing::create_event(
        "HID::UpdateDefaultCallback".to_string(),
        Box::new({
            let resource_manager = resource_manager.clone();
            move |_time, ns_late| {
                resource_manager.lock().update_controllers(ns_late);
                None
            }
        }),
    );
    let mouse_keyboard_update_event = core_timing::create_event(
        "HID::UpdateMouseKeyboardCallback".to_string(),
        Box::new({
            let resource_manager = resource_manager.clone();
            move |_time, ns_late| {
                resource_manager.lock().update_mouse_keyboard(ns_late);
                None
            }
        }),
    );
    let motion_update_event = core_timing::create_event(
        "HID::UpdateMotionCallback".to_string(),
        Box::new({
            let resource_manager = resource_manager.clone();
            move |_time, ns_late| {
                resource_manager.lock().update_motion(ns_late);
                None
            }
        }),
    );
    let touch_update_event = core_timing::create_event(
        "HID::TouchUpdateCallback".to_string(),
        Box::new({
            let resource_manager = resource_manager.clone();
            move |time, _ns_late| {
                resource_manager.lock().update_touch_screen(time);
                None
            }
        }),
    );

    {
        let mut ct = core_timing.lock().unwrap();
        ct.schedule_looping_event(NPAD_UPDATE_NS, NPAD_UPDATE_NS, &npad_update_event, false);
        ct.schedule_looping_event(
            DEFAULT_UPDATE_NS,
            DEFAULT_UPDATE_NS,
            &default_update_event,
            false,
        );
        ct.schedule_looping_event(
            MOUSE_KEYBOARD_UPDATE_NS,
            MOUSE_KEYBOARD_UPDATE_NS,
            &mouse_keyboard_update_event,
            false,
        );
        ct.schedule_looping_event(
            MOTION_UPDATE_NS,
            MOTION_UPDATE_NS,
            &motion_update_event,
            false,
        );
        ct.schedule_looping_event(
            Duration::from_nanos(
                hid_core::resources::touch_screen::touch_screen_resource::GESTURE_UPDATE_PERIOD_NS,
            ),
            Duration::from_nanos(
                hid_core::resources::touch_screen::touch_screen_resource::GESTURE_UPDATE_PERIOD_NS,
            ),
            &touch_update_event,
            false,
        );
    }

    let _hid_update_events = [
        npad_update_event,
        default_update_event,
        mouse_keyboard_update_event,
        motion_update_event,
        touch_update_event,
    ];

    let mut server_manager = ServerManager::new(system);

    // "hid" -> IHidServer
    {
        let rm = resource_manager.clone();
        let fw = firmware_settings.clone();
        let system_ref = system;
        let factory: SessionRequestHandlerFactory =
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::hid_server::IHidServer::new(
                    system_ref,
                    rm.clone(),
                    fw.clone(),
                ))
            });
        server_manager.register_named_service("hid", factory, 64);
    }

    // "hid:dbg" -> IHidDebugServer
    {
        let rm = resource_manager.clone();
        let fw = firmware_settings.clone();
        let factory: SessionRequestHandlerFactory =
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::hid_debug_server::IHidDebugServer::new(
                    rm.clone(),
                    fw.clone(),
                ))
            });
        server_manager.register_named_service("hid:dbg", factory, 64);
    }

    // "hid:sys" -> IHidSystemServer
    {
        let rm = resource_manager.clone();
        let fw = firmware_settings.clone();
        let factory: SessionRequestHandlerFactory =
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::hid_system_server::IHidSystemServer::new(
                    rm.clone(),
                    fw.clone(),
                ))
            });
        server_manager.register_named_service("hid:sys", factory, 64);
    }

    // "hidbus" -> Hidbus
    {
        let factory: SessionRequestHandlerFactory =
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::hidbus::Hidbus::new())
            });
        server_manager.register_named_service("hidbus", factory, 64);
    }

    // "irs" -> IRS
    {
        let factory: SessionRequestHandlerFactory =
            Box::new(move || -> SessionRequestHandlerPtr { Arc::new(super::irs::Irs::new()) });
        server_manager.register_named_service("irs", factory, 64);
    }

    // "irs:sys" -> IRS_SYS
    {
        let factory: SessionRequestHandlerFactory =
            Box::new(move || -> SessionRequestHandlerPtr { Arc::new(super::irs::IrsSys::new()) });
        server_manager.register_named_service("irs:sys", factory, 64);
    }

    // "xcd:sys" -> XCD_SYS
    {
        let factory: SessionRequestHandlerFactory =
            Box::new(move || -> SessionRequestHandlerPtr { Arc::new(super::xcd::XcdSys::new()) });
        server_manager.register_named_service("xcd:sys", factory, 64);
    }

    ServerManager::run_server(server_manager);
}
