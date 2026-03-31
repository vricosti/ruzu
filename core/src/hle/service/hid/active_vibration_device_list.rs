//! Port of zuyu/src/core/hle/service/hid/active_vibration_device_list.h and .cpp
//!
//! IActiveVibrationDeviceList service.

use std::collections::BTreeMap;
use std::sync::Arc;

use hid_core::resource_manager::ResourceManager;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn to_ipc_result(r: common::ResultCode) -> ResultCode {
    ResultCode::new(r.raw())
}
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
    ///
    /// Reads a VibrationDeviceHandle, validates it, and activates the vibration device
    /// via resource_manager. Upstream also tracks handles in a list for deduplication.
    ///
    /// Note: resource_manager.GetVibrationDevice().Activate() is not yet ported,
    /// so the actual device activation is skipped. Handle validation matches upstream.
    fn activate_vibration_device(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        use crate::hle::service::ipc_helpers::RequestParser;
        use hid_core::hid_types::VibrationDeviceHandle;
        use hid_core::hid_util;

        let mut rp = RequestParser::new(ctx);
        let vibration_device_handle: VibrationDeviceHandle = rp.pop_raw();

        log::debug!(
            "IActiveVibrationDeviceList::ActivateVibrationDevice called, \
             npad_type={}, npad_id={}, device_index={}",
            vibration_device_handle.npad_type as u8,
            vibration_device_handle.npad_id as u8,
            vibration_device_handle.device_index as u8,
        );

        let valid = hid_util::is_vibration_handle_valid(&vibration_device_handle);
        if valid.is_error() {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(to_ipc_result(valid));
            return;
        }

        // Upstream: resource_manager->GetVibrationDevice(handle)->Activate()
        // GetVibrationDevice is not yet ported; skip the actual activation.

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new(resource_manager: Arc<parking_lot::Mutex<ResourceManager>>) -> Self {
        let handlers = build_handler_map(&[(
            0,
            Some(Self::activate_vibration_device),
            "ActivateVibrationDevice",
        )]);

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
