// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of Eden's `core/hle/service/am/service/home_menu_functions.{h,cpp}`.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, Weak};

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::am::am_results;
use crate::hle::service::am::applet::Applet;
use crate::hle::service::am::window_system::WindowSystem;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::storage::IStorage;

/// IPC command table for IHomeMenuFunctions:
/// - 10: RequestToGetForeground
/// - 11: LockForeground
/// - 12: UnlockForeground
/// - 20: PopFromGeneralChannel
/// - 21: GetPopFromGeneralChannelEvent
/// - 30: GetHomeButtonWriterLockAccessor (unimplemented)
/// - 31: GetWriterLockAccessorEx (unimplemented)
/// - 40: IsSleepEnabled
/// - 41: IsRebootEnabled
/// - 50: LaunchSystemApplet (unimplemented)
/// - 51: LaunchStarter (unimplemented)
/// - 100: PopRequestLaunchApplicationForDebug (unimplemented)
/// - 110: IsForceTerminateApplicationDisabledForDebug
/// - 200: LaunchDevMenu (unimplemented)
/// - 1000: SetLastApplicationExitReason (unimplemented)
pub struct IHomeMenuFunctions {
    system: SystemRef,
    /// Eden retains `m_applet` for the lifetime of this interface even though no command
    /// dereferences it. The strong reference is therefore lifecycle ownership, not dead data.
    #[allow(dead_code)]
    applet: Arc<Mutex<Applet>>,
    window_system: Weak<Mutex<WindowSystem>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IHomeMenuFunctions {
    pub fn new(
        system: SystemRef,
        applet: Arc<Mutex<Applet>>,
        window_system: Weak<Mutex<WindowSystem>>,
    ) -> Self {
        let handlers = build_handler_map(&[
            (
                10,
                Some(Self::request_to_get_foreground_handler),
                "RequestToGetForeground",
            ),
            (11, Some(Self::lock_foreground_handler), "LockForeground"),
            (
                12,
                Some(Self::unlock_foreground_handler),
                "UnlockForeground",
            ),
            (
                20,
                Some(Self::pop_from_general_channel_handler),
                "PopFromGeneralChannel",
            ),
            (
                21,
                Some(Self::get_pop_from_general_channel_event_handler),
                "GetPopFromGeneralChannelEvent",
            ),
            (30, None, "GetHomeButtonWriterLockAccessor"),
            (31, None, "GetWriterLockAccessorEx"),
            (40, Some(Self::is_sleep_enabled_handler), "IsSleepEnabled"),
            (41, Some(Self::is_reboot_enabled_handler), "IsRebootEnabled"),
            (50, None, "LaunchSystemApplet"),
            (51, None, "LaunchStarter"),
            (100, None, "PopRequestLaunchApplicationForDebug"),
            (
                110,
                Some(Self::is_force_terminate_application_disabled_for_debug_handler),
                "IsForceTerminateApplicationDisabledForDebug",
            ),
            (200, None, "LaunchDevMenu"),
            (1000, None, "SetLastApplicationExitReason"),
        ]);
        Self {
            system,
            applet,
            window_system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(object);
    }

    /// Port of IHomeMenuFunctions::RequestToGetForeground
    /// Upstream calls m_window_system.RequestHomeMenuToGetForeground() then R_SUCCEED.
    fn request_to_get_foreground_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IHomeMenuFunctions) };
        log::info!("IHomeMenuFunctions::RequestToGetForeground called");
        service
            .window_system
            .upgrade()
            .expect("WindowSystem must outlive active AM services")
            .lock()
            .unwrap()
            .request_home_menu_to_get_foreground();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of IHomeMenuFunctions::LockForeground
    /// Upstream calls m_window_system.RequestLockHomeMenuIntoForeground() then R_SUCCEED.
    fn lock_foreground_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IHomeMenuFunctions) };
        log::info!("IHomeMenuFunctions::LockForeground called");
        service
            .window_system
            .upgrade()
            .expect("WindowSystem must outlive active AM services")
            .lock()
            .unwrap()
            .request_lock_home_menu_into_foreground();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of IHomeMenuFunctions::UnlockForeground
    /// Upstream calls m_window_system.RequestUnlockHomeMenuIntoForeground() then R_SUCCEED.
    fn unlock_foreground_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IHomeMenuFunctions) };
        log::info!("IHomeMenuFunctions::UnlockForeground called");
        service
            .window_system
            .upgrade()
            .expect("WindowSystem must outlive active AM services")
            .lock()
            .unwrap()
            .request_unlock_home_menu_into_foreground();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of `IHomeMenuFunctions::PopFromGeneralChannel`.
    fn pop_from_general_channel(&self) -> Result<Vec<u8>, ResultCode> {
        self.system
            .get()
            .try_pop_general_channel()
            .ok_or(am_results::RESULT_NO_DATA_IN_CHANNEL)
    }

    fn pop_from_general_channel_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IHomeMenuFunctions) };
        log::debug!("IHomeMenuFunctions::PopFromGeneralChannel called");

        match service.pop_from_general_channel() {
            Ok(data) => Self::push_interface_response(
                ctx,
                Arc::new(IStorage::new_with_system(service.system, data)),
            ),
            Err(result) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(result);
            }
        }
    }

    /// Port of IHomeMenuFunctions::GetPopFromGeneralChannelEvent
    /// Upstream returns `system.GetGeneralChannelEvent().GetHandle()` as a copy handle.
    fn get_pop_from_general_channel_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IHomeMenuFunctions) };
        log::info!("IHomeMenuFunctions::GetPopFromGeneralChannelEvent called");

        let object_id = service
            .system
            .get()
            .get_general_channel_event()
            .copy_object_id(ctx)
            .unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_object_id(object_id);
    }

    /// Port of `IHomeMenuFunctions::IsSleepEnabled`.
    fn is_sleep_enabled_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IHomeMenuFunctions::IsSleepEnabled called");

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(false);
    }

    /// Port of IHomeMenuFunctions::IsRebootEnabled
    /// Upstream returns true.
    fn is_reboot_enabled_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IHomeMenuFunctions::IsRebootEnabled called");

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(true);
    }

    /// Port of IHomeMenuFunctions::IsForceTerminateApplicationDisabledForDebug
    /// Upstream returns false.
    fn is_force_terminate_application_disabled_for_debug_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("IHomeMenuFunctions::IsForceTerminateApplicationDisabledForDebug called");

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(false);
    }
}

impl SessionRequestHandler for IHomeMenuFunctions {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for IHomeMenuFunctions {
    fn get_service_name(&self) -> &str {
        "am::IHomeMenuFunctions"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::service::os::process::Process;

    #[test]
    fn home_menu_uses_system_general_channel_and_retains_applet_owner() {
        let system = Box::new(crate::core::System::new());
        let system_ref = SystemRef::from_ref(&system);
        let applet = Arc::new(Mutex::new(Applet::new(system_ref, Process::new(), false)));
        let window_system = Arc::new(Mutex::new(WindowSystem::new(system_ref)));
        let service = IHomeMenuFunctions::new(
            system_ref,
            Arc::clone(&applet),
            Arc::downgrade(&window_system),
        );

        assert!(Arc::ptr_eq(&service.applet, &applet));
        assert!(service
            .handlers
            .get(&20)
            .unwrap()
            .handler_callback
            .is_some());
        assert!(service
            .handlers
            .get(&40)
            .unwrap()
            .handler_callback
            .is_some());
        assert_eq!(
            service.pop_from_general_channel(),
            Err(am_results::RESULT_NO_DATA_IN_CHANNEL)
        );

        system.push_general_channel_data(vec![0x48, 0x42]);
        assert_eq!(service.pop_from_general_channel(), Ok(vec![0x48, 0x42]));
    }
}
