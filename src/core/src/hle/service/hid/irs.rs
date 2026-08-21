//! Port of zuyu/src/core/hle/service/hid/irs.h and irs.cpp
//!
//! IRS and IRS_SYS services ("irs", "irs:sys").

use std::collections::BTreeMap;
use std::sync::Arc;

use hid_core::hid_types::{NpadIdType, NpadStyleIndex};
use hid_core::irsensor::irs_types::IrCameraHandle;

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS, RESULT_UNKNOWN};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

fn npad_id_from_raw(raw: u32) -> Option<NpadIdType> {
    Some(match raw {
        0 => NpadIdType::Player1,
        1 => NpadIdType::Player2,
        2 => NpadIdType::Player3,
        3 => NpadIdType::Player4,
        4 => NpadIdType::Player5,
        5 => NpadIdType::Player6,
        6 => NpadIdType::Player7,
        7 => NpadIdType::Player8,
        0x10 => NpadIdType::Other,
        0x20 => NpadIdType::Handheld,
        _ => return None,
    })
}

/// IRS service - IR sensor interface.
pub struct Irs {
    system: SystemRef,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl Irs {
    #[inline]
    fn to_ipc_result(result: common::ResultCode) -> ResultCode {
        ResultCode::new(result.raw())
    }

    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::debug!("(STUBBED) irs command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Upstream: `IRS::GetIrsensorSharedMemoryHandle`.
    fn get_irsensor_shared_memory_handle(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const Irs) };
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_raw::<u64>();

        let handle = (|| -> Option<u32> {
            let (object_id, shared_memory) = service.system.get().kernel()?.get_irs_shared_mem()?;
            let thread = ctx.get_thread()?;
            let parent = thread.lock().unwrap().parent.as_ref()?.upgrade()?;
            let mut process = parent.lock().unwrap();
            process.register_shared_memory_object(object_id, Arc::clone(&shared_memory));
            process.handle_table.add(object_id).ok()
        })();

        let Some(handle) = handle else {
            log::error!(
                "IRS::GetIrsensorSharedMemoryHandle failed, applet_resource_user_id={}",
                aruid
            );
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_UNKNOWN);
            return;
        };

        log::debug!(
            "IRS::GetIrsensorSharedMemoryHandle called, applet_resource_user_id={}, handle={:#x}",
            aruid,
            handle
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    /// Upstream: `IRS::GetNpadIrCameraHandle`.
    fn get_npad_ir_camera_handle(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let npad_id_raw = rp.pop_u32();
        let npad_id = match npad_id_from_raw(npad_id_raw) {
            Some(npad_id) => npad_id,
            None => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(Self::to_ipc_result(
                    hid_core::hid_result::RESULT_INVALID_NPAD_ID,
                ));
                return;
            }
        };

        let camera_handle = IrCameraHandle {
            npad_id: hid_core::hid_util::npad_id_type_to_index(npad_id) as u8,
            npad_type: NpadStyleIndex::None,
            _padding: [0; 2],
        };
        log::debug!(
            "IRS::GetNpadIrCameraHandle called, npad_id={npad_id_raw}, camera_npad_id={}, camera_npad_type={}",
            camera_handle.npad_id,
            camera_handle.npad_type as u8
        );

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&camera_handle);
    }

    pub fn new(system: SystemRef) -> Self {
        let handlers = build_handler_map(&[
            (302, Some(Self::stub_success_handler), "ActivateIrsensor"),
            (303, Some(Self::stub_success_handler), "DeactivateIrsensor"),
            (
                304,
                Some(Self::get_irsensor_shared_memory_handle),
                "GetIrsensorSharedMemoryHandle",
            ),
            (305, Some(Self::stub_success_handler), "StopImageProcessor"),
            (306, Some(Self::stub_success_handler), "RunMomentProcessor"),
            (
                307,
                Some(Self::stub_success_handler),
                "RunClusteringProcessor",
            ),
            (
                308,
                Some(Self::stub_success_handler),
                "RunImageTransferProcessor",
            ),
            (
                309,
                Some(Self::stub_success_handler),
                "GetImageTransferProcessorState",
            ),
            (
                310,
                Some(Self::stub_success_handler),
                "RunTeraPluginProcessor",
            ),
            (
                311,
                Some(Self::get_npad_ir_camera_handle),
                "GetNpadIrCameraHandle",
            ),
            (
                312,
                Some(Self::stub_success_handler),
                "RunPointingProcessor",
            ),
            (
                313,
                Some(Self::stub_success_handler),
                "SuspendImageProcessor",
            ),
            (
                314,
                Some(Self::stub_success_handler),
                "CheckFirmwareVersion",
            ),
            (315, Some(Self::stub_success_handler), "SetFunctionLevel"),
            (
                316,
                Some(Self::stub_success_handler),
                "RunImageTransferExProcessor",
            ),
            (317, Some(Self::stub_success_handler), "RunIrLedProcessor"),
            (
                318,
                Some(Self::stub_success_handler),
                "StopImageProcessorAsync",
            ),
            (
                319,
                Some(Self::stub_success_handler),
                "ActivateIrsensorWithFunctionLevel",
            ),
        ]);

        Self {
            system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn npad_ids_map_to_ir_camera_indices() {
        for (raw, expected_id, expected_index) in [
            (0, NpadIdType::Player1, 0),
            (7, NpadIdType::Player8, 7),
            (0x20, NpadIdType::Handheld, 8),
            (0x10, NpadIdType::Other, 9),
        ] {
            let npad_id = npad_id_from_raw(raw).unwrap();
            assert_eq!(npad_id, expected_id);
            assert!(hid_core::hid_util::is_npad_id_valid(npad_id));
            assert_eq!(
                hid_core::hid_util::npad_id_type_to_index(npad_id),
                expected_index
            );
        }
        assert_eq!(npad_id_from_raw(8), None);
        assert_eq!(npad_id_from_raw(u32::MAX), None);
    }

    #[test]
    fn ir_camera_handle_layout_matches_upstream() {
        assert_eq!(std::mem::size_of::<IrCameraHandle>(), 4);
        let handle = IrCameraHandle {
            npad_id: 8,
            npad_type: NpadStyleIndex::None,
            _padding: [0; 2],
        };
        let bytes = unsafe {
            std::slice::from_raw_parts(
                (&handle as *const IrCameraHandle).cast::<u8>(),
                std::mem::size_of::<IrCameraHandle>(),
            )
        };
        assert_eq!(bytes, &[8, 0, 0, 0]);
    }
}

impl SessionRequestHandler for Irs {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "irs"
    }
}

impl ServiceFramework for Irs {
    fn get_service_name(&self) -> &str {
        "irs"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IRS_SYS service - system-level IR sensor interface.
pub struct IrsSys {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IrsSys {
    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::debug!("(STUBBED) irs:sys command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                500,
                Some(Self::stub_success_handler),
                "SetAppletResourceUserId",
            ),
            (
                501,
                Some(Self::stub_success_handler),
                "RegisterAppletResourceUserId",
            ),
            (
                502,
                Some(Self::stub_success_handler),
                "UnregisterAppletResourceUserId",
            ),
            (
                503,
                Some(Self::stub_success_handler),
                "EnableAppletToGetInput",
            ),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IrsSys {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "irs:sys"
    }
}

impl ServiceFramework for IrsSys {
    fn get_service_name(&self) -> &str {
        "irs:sys"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
