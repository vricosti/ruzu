//! Port of zuyu/src/core/hle/service/hid/active_vibration_device_list.h and .cpp
//!
//! IActiveVibrationDeviceList service.

use std::collections::BTreeMap;
use std::sync::Arc;

use hid_core::resource_manager::ResourceManager;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IActiveVibrationDeviceList:
///
/// | Cmd | Name                      |
/// |-----|---------------------------|
/// | 0   | ActivateVibrationDevice   |
pub struct IActiveVibrationDeviceList {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
}

impl IActiveVibrationDeviceList {
    /// Upstream: IActiveVibrationDeviceList::ActivateVibrationDevice
    /// Reads a VibrationDeviceHandle and activates the vibration device.
    /// TODO: Full implementation needs resource_manager vibration device activation.
    fn activate_vibration_device(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("(STUBBED) IActiveVibrationDeviceList::ActivateVibrationDevice called");

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new(resource_manager: Arc<parking_lot::Mutex<ResourceManager>>) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::activate_vibration_device), "ActivateVibrationDevice"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            resource_manager,
        }
    }
}

impl SessionRequestHandler for IActiveVibrationDeviceList {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "hid::IActiveVibrationDeviceList"
    }
}

impl ServiceFramework for IActiveVibrationDeviceList {
    fn get_service_name(&self) -> &str {
        "hid::IActiveVibrationDeviceList"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
