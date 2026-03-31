//! Port of zuyu/src/core/hle/service/hid/hid.h and hid.cpp
//!
//! Entry point for the HID service module.

use std::sync::Arc;

use hid_core::hid_core::HIDCore;
use hid_core::resource_manager::ResourceManager;
use hid_core::resources::hid_firmware_settings::HidFirmwareSettings;

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
