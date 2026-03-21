// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/global_state_controller.h
//! Port of zuyu/src/core/hle/service/am/service/global_state_controller.cpp

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::cradle_firmware_updater::ICradleFirmwareUpdater;

/// IPC command table for IGlobalStateController:
/// - 0: RequestToEnterSleep (unimplemented)
/// - 1: EnterSleep (unimplemented)
/// - 2: StartSleepSequence (unimplemented)
/// - 3: StartShutdownSequence (unimplemented)
/// - 4: StartRebootSequence (unimplemented)
/// - 9: IsAutoPowerDownRequested (unimplemented)
/// - 10: LoadAndApplyIdlePolicySettings
/// - 11: NotifyCecSettingsChanged (unimplemented)
/// - 12: SetDefaultHomeButtonLongPressTime (unimplemented)
/// - 13: UpdateDefaultDisplayResolution (unimplemented)
/// - 14: ShouldSleepOnBoot
/// - 15: GetHdcpAuthenticationFailedEvent
/// - 30: OpenCradleFirmwareUpdater
pub struct IGlobalStateController {
    // TODO: ServiceContext, Event fields
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IGlobalStateController {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, None, "RequestToEnterSleep"),
            (1, None, "EnterSleep"),
            (2, None, "StartSleepSequence"),
            (3, None, "StartShutdownSequence"),
            (4, None, "StartRebootSequence"),
            (9, None, "IsAutoPowerDownRequested"),
            (
                10,
                Some(Self::load_and_apply_idle_policy_settings_handler),
                "LoadAndApplyIdlePolicySettings",
            ),
            (11, None, "NotifyCecSettingsChanged"),
            (12, None, "SetDefaultHomeButtonLongPressTime"),
            (13, None, "UpdateDefaultDisplayResolution"),
            (14, Some(Self::should_sleep_on_boot_handler), "ShouldSleepOnBoot"),
            (15, None, "GetHdcpAuthenticationFailedEvent"), // Needs event handle support
            (
                30,
                Some(Self::open_cradle_firmware_updater_handler),
                "OpenCradleFirmwareUpdater",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(object.clone()).unwrap_or(0)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(object);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    /// Port of IGlobalStateController::LoadAndApplyIdlePolicySettings
    /// Upstream: `LOG_WARNING(Service_AM, "(STUBBED) called"); R_SUCCEED();`
    fn load_and_apply_idle_policy_settings_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("(STUBBED) IGlobalStateController::LoadAndApplyIdlePolicySettings called");

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of IGlobalStateController::ShouldSleepOnBoot
    /// Upstream returns false.
    fn should_sleep_on_boot_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IGlobalStateController::ShouldSleepOnBoot called");

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(false);
    }

    /// Port of IGlobalStateController::OpenCradleFirmwareUpdater
    /// Creates an ICradleFirmwareUpdater and returns it.
    fn open_cradle_firmware_updater_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("IGlobalStateController::OpenCradleFirmwareUpdater called");

        let updater = Arc::new(ICradleFirmwareUpdater::new());
        Self::push_interface_response(ctx, updater);
    }
}

impl SessionRequestHandler for IGlobalStateController {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for IGlobalStateController {
    fn get_service_name(&self) -> &str {
        "am::IGlobalStateController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
