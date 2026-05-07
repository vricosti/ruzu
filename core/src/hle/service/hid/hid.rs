//! Port of zuyu/src/core/hle/service/hid/hid.h and hid.cpp
//!
//! Entry point for the HID service module.

use std::any::Any;
use std::sync::Arc;
use std::time::Duration;

use hid_core::hid_core::HIDCore;
use hid_core::resource_manager::{
    ResourceManager, DEFAULT_UPDATE_NS, MOTION_UPDATE_NS, MOUSE_KEYBOARD_UPDATE_NS, NPAD_UPDATE_NS,
};
use hid_core::resources::hid_firmware_settings::HidFirmwareSettings;
use hid_core::resources::shared_memory_holder::KSharedMemoryBacking;

use crate::core_timing;
use crate::hle::kernel::k_shared_memory::{KSharedMemory, MemoryPermission};
use crate::hle::service::hle_ipc::{SessionRequestHandlerFactory, SessionRequestHandlerPtr};
use crate::hle::service::server_manager::ServerManager;

/// Implementation of `hid_core::KSharedMemoryBacking` that allocates real
/// `KSharedMemory` objects from the kernel.
///
/// Mirrors upstream `SharedMemoryHolder::Initialize`:
/// `KSharedMemory::Create + Initialize + Register`. Passed once into
/// `ResourceManager::set_shared_memory_backing` at HID service startup so
/// every `AppletResource::create_applet_resource` allocates a kernel-backed
/// page that the daemon writes to and the guest maps via
/// `IAppletResource::GetSharedMemoryHandle`.
struct HidKSharedMemoryBacking {
    system: crate::core::SystemRef,
}

impl HidKSharedMemoryBacking {
    fn new(system: crate::core::SystemRef) -> Self {
        Self { system }
    }
}

impl KSharedMemoryBacking for HidKSharedMemoryBacking {
    fn create(&self, size: usize) -> Option<(*mut u8, Arc<dyn Any + Send + Sync>)> {
        let system_ptr =
            self.system.get() as *const crate::core::System as *mut crate::core::System;
        // SAFETY: SystemRef holds a stable pointer for the program lifetime;
        // the &mut access below is serialized by virtue of being called from
        // hid::loop_process initialization (single-threaded path).
        let device_memory_ptr = unsafe { (*system_ptr).device_memory() as *const _ };
        let kernel = unsafe { (*system_ptr).kernel_mut()? };

        let mut shared_memory = KSharedMemory::new();
        if shared_memory
            .initialize(
                unsafe { &*device_memory_ptr },
                kernel.memory_manager_mut(),
                MemoryPermission::None,
                MemoryPermission::Read,
                size,
            )
            .is_error()
        {
            return None;
        }

        let host_ptr = shared_memory.get_pointer_mut(0);
        if host_ptr.is_null() {
            return None;
        }

        // Box the KSharedMemory inside an Arc<dyn Any> for hid_core's opaque
        // keepalive slot. The `core` side recovers it via downcast in
        // IAppletResource::GetSharedMemoryHandle.
        let keepalive: Arc<dyn Any + Send + Sync> = Arc::new(shared_memory);
        Some((host_ptr, keepalive))
    }
}

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

    // Wire up the kernel-backed shared-memory factory before any IPC handler
    // can run. Mirrors upstream's behavior where `SharedMemoryHolder::Initialize`
    // takes a `Core::System&` directly; ruzu cannot do that across the
    // hid_core/core crate boundary so we inject the backing as a trait object.
    resource_manager
        .lock()
        .set_shared_memory_backing(Arc::new(HidKSharedMemoryBacking::new(system.clone())));

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
        core_timing.schedule_looping_event(
            NPAD_UPDATE_NS,
            NPAD_UPDATE_NS,
            &npad_update_event,
            false,
        );
        core_timing.schedule_looping_event(
            DEFAULT_UPDATE_NS,
            DEFAULT_UPDATE_NS,
            &default_update_event,
            false,
        );
        core_timing.schedule_looping_event(
            MOUSE_KEYBOARD_UPDATE_NS,
            MOUSE_KEYBOARD_UPDATE_NS,
            &mouse_keyboard_update_event,
            false,
        );
        core_timing.schedule_looping_event(
            MOTION_UPDATE_NS,
            MOTION_UPDATE_NS,
            &motion_update_event,
            false,
        );
        core_timing.schedule_looping_event(
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
