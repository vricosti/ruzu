// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/service_manager.h/.cpp
//!
//! PSC::Time::ServiceManager — the "time:m" service.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::device_memory::DeviceMemory;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_memory_manager::KMemoryManager;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::psc::time::clocks::context_writers::ContextWriter;
use crate::hle::service::psc::time::common::{
    AlarmInfo, ClockSourceId, LocationName, OperationEvent, RuleVersion, StaticServiceSetupInfo,
    SteadyClockTimePoint, SystemClockContext,
};
use crate::hle::service::psc::time::r#static::StaticService;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::clocks::steady_clock_core::SteadyClockCoreImpl;
use super::manager::TimeManager;

/// PSC::Time::ServiceManager — handles clock core setup.
pub struct TimeServiceManager {
    system: SystemRef,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    time: Arc<Mutex<TimeManager>>,
    get_time_ns: Arc<dyn Fn() -> i64 + Send + Sync>,
    local_operation: OperationEvent,
    network_operation: OperationEvent,
    ephemeral_operation: OperationEvent,
    local_operation_readable_event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
    network_operation_readable_event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
    ephemeral_operation_readable_event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
    user_automatic_correction_readable_event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
    closest_alarm_readable_event: Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
}

impl TimeServiceManager {
    pub fn new(
        system: SystemRef,
        device_memory: *const DeviceMemory,
        memory_manager: *mut KMemoryManager,
    ) -> Self {
        let get_time_ns: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(move || {
            if system.is_null() {
                0
            } else {
                system.get().get_core_timing_ticks() as i64
            }
        });

        let time = Arc::new(Mutex::new(unsafe {
            TimeManager::new_with_shared_memory(
                Box::new({
                    let get_time_ns = Arc::clone(&get_time_ns);
                    move || get_time_ns()
                }),
                (!device_memory.is_null()).then_some(&*device_memory),
                (!memory_manager.is_null()).then_some(&mut *memory_manager),
            )
        }));
        let local_operation = OperationEvent::new();
        let network_operation = OperationEvent::new();
        let ephemeral_operation = OperationEvent::new();
        {
            let mut time_guard = time.lock().unwrap();
            time_guard
                .local_system_clock_context_writer
                .link(local_operation.clone());
            time_guard
                .network_system_clock_context_writer
                .link(network_operation.clone());
            time_guard
                .ephemeral_network_clock_context_writer
                .link(ephemeral_operation.clone());
        }

        Self {
            system,
            handlers: build_handler_map(&[
                (0, Some(Self::get_static_service_as_user_handler), "GetStaticServiceAsUser"),
                (5, Some(Self::get_static_service_as_admin_handler), "GetStaticServiceAsAdmin"),
                (6, Some(Self::get_static_service_as_repair_handler), "GetStaticServiceAsRepair"),
                (
                    9,
                    Some(Self::get_static_service_as_service_manager_handler),
                    "GetStaticServiceAsServiceManager",
                ),
                (
                    10,
                    Some(Self::setup_standard_steady_clock_core_handler),
                    "SetupStandardSteadyClockCore",
                ),
                (
                    11,
                    Some(Self::setup_standard_local_system_clock_core_handler),
                    "SetupStandardLocalSystemClockCore",
                ),
                (
                    12,
                    Some(Self::setup_standard_network_system_clock_core_handler),
                    "SetupStandardNetworkSystemClockCore",
                ),
                (
                    13,
                    Some(Self::setup_standard_user_system_clock_core_handler),
                    "SetupStandardUserSystemClockCore",
                ),
                (
                    14,
                    Some(Self::setup_time_zone_service_core_handler),
                    "SetupTimeZoneServiceCore",
                ),
                (
                    15,
                    Some(Self::setup_ephemeral_network_system_clock_core_handler),
                    "SetupEphemeralNetworkSystemClockCore",
                ),
                (
                    50,
                    Some(Self::get_standard_local_clock_operation_event_handler),
                    "GetStandardLocalClockOperationEvent",
                ),
                (
                    51,
                    Some(Self::get_standard_network_clock_operation_event_for_service_manager_handler),
                    "GetStandardNetworkClockOperationEventForServiceManager",
                ),
                (
                    52,
                    Some(Self::get_ephemeral_network_clock_operation_event_for_service_manager_handler),
                    "GetEphemeralNetworkClockOperationEventForServiceManager",
                ),
                (
                    60,
                    Some(Self::get_standard_user_system_clock_automatic_correction_updated_event_handler),
                    "GetStandardUserSystemClockAutomaticCorrectionUpdatedEvent",
                ),
                (
                    100,
                    Some(Self::set_standard_steady_clock_base_time_handler),
                    "SetStandardSteadyClockBaseTime",
                ),
                (
                    200,
                    Some(Self::get_closest_alarm_updated_event_handler),
                    "GetClosestAlarmUpdatedEvent",
                ),
                (201, Some(Self::check_and_signal_alarms_handler), "CheckAndSignalAlarms"),
                (202, Some(Self::get_closest_alarm_info_handler), "GetClosestAlarmInfo"),
            ]),
            handlers_tipc: BTreeMap::new(),
            time,
            get_time_ns,
            local_operation,
            network_operation,
            ephemeral_operation,
            local_operation_readable_event: Mutex::new(None),
            network_operation_readable_event: Mutex::new(None),
            ephemeral_operation_readable_event: Mutex::new(None),
            user_automatic_correction_readable_event: Mutex::new(None),
            closest_alarm_readable_event: Mutex::new(None),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub fn shared_time(&self) -> Arc<Mutex<TimeManager>> {
        Arc::clone(&self.time)
    }

    pub fn get_static_service(&self, setup_info: StaticServiceSetupInfo, _name: &str) -> Arc<StaticService> {
        Arc::new(StaticService::with_time_manager(setup_info, Arc::clone(&self.time)))
    }

    pub fn get_static_service_as_user(&self) -> Arc<StaticService> {
        self.get_static_service(
            StaticServiceSetupInfo {
                can_write_local_clock: false,
                can_write_user_clock: false,
                can_write_network_clock: false,
                can_write_timezone_device_location: false,
                can_write_steady_clock: false,
                can_write_uninitialized_clock: false,
            },
            "time:u",
        )
    }

    pub fn get_static_service_as_admin(&self) -> Arc<StaticService> {
        self.get_static_service(
            StaticServiceSetupInfo {
                can_write_local_clock: true,
                can_write_user_clock: true,
                can_write_network_clock: false,
                can_write_timezone_device_location: true,
                can_write_steady_clock: false,
                can_write_uninitialized_clock: false,
            },
            "time:a",
        )
    }

    pub fn get_static_service_as_repair(&self) -> Arc<StaticService> {
        self.get_static_service(
            StaticServiceSetupInfo {
                can_write_local_clock: false,
                can_write_user_clock: false,
                can_write_network_clock: false,
                can_write_timezone_device_location: false,
                can_write_steady_clock: true,
                can_write_uninitialized_clock: false,
            },
            "time:r",
        )
    }

    pub fn get_static_service_as_service_manager(&self) -> Arc<StaticService> {
        self.get_static_service(
            StaticServiceSetupInfo {
                can_write_local_clock: true,
                can_write_user_clock: true,
                can_write_network_clock: true,
                can_write_timezone_device_location: true,
                can_write_steady_clock: true,
                can_write_uninitialized_clock: false,
            },
            "time:sm",
        )
    }

    pub fn setup_standard_steady_clock_core(
        &self,
        is_rtc_reset_detected: bool,
        clock_source_id: ClockSourceId,
        rtc_offset: i64,
        internal_offset: i64,
        test_offset: i64,
    ) -> ResultCode {
        let mut time = self.time.lock().unwrap();
        time.standard_steady_clock.initialize(
            clock_source_id,
            rtc_offset,
            internal_offset,
            test_offset,
            is_rtc_reset_detected,
        );
        *time.steady_clock_source_id.lock().unwrap() = clock_source_id;
        time.alarms.set_steady_clock_initialized(true);

        let raw_time = time.standard_steady_clock.get_current_raw_time_point_impl();
        let boot_time = raw_time - (self.get_time_ns)();
        time.shared_memory
            .set_steady_clock_time_point(clock_source_id, boot_time);
        time.standard_steady_clock
            .set_continuous_adjustment(clock_source_id, boot_time);
        let time_point = time.standard_steady_clock.get_continuous_adjustment();
        time.shared_memory.set_continuous_adjustment(&time_point);
        RESULT_SUCCESS
    }

    pub fn setup_standard_local_system_clock_core(
        &self,
        context: &SystemClockContext,
        time_value: i64,
    ) -> ResultCode {
        let mut time = self.time.lock().unwrap();
        time.standard_local_system_clock.initialize(context, time_value);
        let context = time
            .standard_local_system_clock
            .clock
            .get_context()
            .unwrap_or(*context);
        time.shared_memory.set_local_system_context(&context);
        RESULT_SUCCESS
    }

    pub fn setup_standard_network_system_clock_core(
        &self,
        mut context: SystemClockContext,
        accuracy: i64,
    ) -> ResultCode {
        let mut time = self.time.lock().unwrap();
        if let Ok(local_context) = time.standard_local_system_clock.clock.get_context() {
            context = local_context;
        }
        time.standard_network_system_clock
            .initialize(&context, accuracy);
        let context = time
            .standard_network_system_clock
            .clock
            .get_context()
            .unwrap_or(context);
        time.shared_memory.set_network_system_context(&context);
        RESULT_SUCCESS
    }

    pub fn setup_standard_user_system_clock_core(
        &self,
        automatic_correction: bool,
        time_point: SteadyClockTimePoint,
    ) -> ResultCode {
        let mut time = self.time.lock().unwrap();
        let local = std::ptr::addr_of_mut!(time.standard_local_system_clock);
        let network = std::ptr::addr_of!(time.standard_network_system_clock);
        let rc = unsafe {
            time.standard_user_system_clock
                .set_automatic_correction(automatic_correction, &mut *local, &*network)
        };
        if rc.is_error() {
            return rc;
        }
        time.standard_user_system_clock
            .set_time_point_and_signal(&time_point);
        time.standard_user_system_clock.set_initialized();
        time.shared_memory
            .set_automatic_correction(automatic_correction);
        RESULT_SUCCESS
    }

    pub fn setup_time_zone_service_core(
        &self,
        name: &LocationName,
        rule_version: &RuleVersion,
        location_count: u32,
        time_point: &SteadyClockTimePoint,
        rule_buffer: &[u8],
    ) -> ResultCode {
        let mut time = self.time.lock().unwrap();
        let _ = time.time_zone.parse_binary(name, rule_buffer);
        time.time_zone.set_time_point(time_point);
        time.time_zone.set_total_location_name_count(location_count);
        time.time_zone.set_rule_version(rule_version);
        time.time_zone.set_initialized();
        RESULT_SUCCESS
    }

    pub fn setup_ephemeral_network_system_clock_core(&self) -> ResultCode {
        let mut time = self.time.lock().unwrap();
        time.ephemeral_network_clock.clock.set_initialized();
        RESULT_SUCCESS
    }

    pub fn set_standard_steady_clock_base_time(&self, base_time: i64) -> ResultCode {
        let mut time = self.time.lock().unwrap();
        time.standard_steady_clock.set_rtc_offset(base_time);
        let raw_time = time.standard_steady_clock.get_current_raw_time_point_impl();
        let diff = raw_time - (self.get_time_ns)();
        time.shared_memory.update_base_time(diff);
        time.standard_steady_clock
            .update_continuous_adjustment_time(diff);
        let time_point = time.standard_steady_clock.get_continuous_adjustment();
        time.shared_memory.set_continuous_adjustment(&time_point);
        RESULT_SUCCESS
    }

    fn get_or_create_event_handle(
        &self,
        ctx: &HLERequestContext,
        event: &Arc<crate::hle::service::os::event::Event>,
        readable_cache: &Mutex<Option<Arc<Mutex<KReadableEvent>>>>,
    ) -> Option<u32> {
        if let Some(readable_event) = readable_cache.lock().unwrap().as_ref() {
            return ctx.copy_handle_for_readable_event(Arc::clone(readable_event));
        }

        let (handle, readable_event) = ctx.create_readable_event(false)?;
        let owner_process = ctx.owner_process_arc()?;
        let scheduler = self.system.get().scheduler_arc();
        event.attach_kernel_event(Arc::clone(&readable_event), owner_process, scheduler);
        *readable_cache.lock().unwrap() = Some(readable_event);
        Some(handle)
    }

    fn push_static_service(
        ctx: &mut HLERequestContext,
        sub_service: Arc<dyn SessionRequestHandler>,
    ) {
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(sub_service.clone()).unwrap_or(0)
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(sub_service);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    fn get_static_service_as_user_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        Self::push_static_service(ctx, service.get_static_service_as_user());
    }

    fn get_static_service_as_admin_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        Self::push_static_service(ctx, service.get_static_service_as_admin());
    }

    fn get_static_service_as_repair_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        Self::push_static_service(ctx, service.get_static_service_as_repair());
    }

    fn get_static_service_as_service_manager_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        Self::push_static_service(ctx, service.get_static_service_as_service_manager());
    }

    fn setup_standard_steady_clock_core_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let is_rtc_reset_detected = rp.pop_bool();
        let mut clock_source_id = [0u8; 16];
        for i in 0..4 {
            let word = rp.pop_u32().to_le_bytes();
            clock_source_id[i * 4..(i + 1) * 4].copy_from_slice(&word);
        }
        let rtc_offset = rp.pop_i64();
        let internal_offset = rp.pop_i64();
        let test_offset = rp.pop_i64();
        let rc = service.setup_standard_steady_clock_core(
            is_rtc_reset_detected,
            clock_source_id,
            rtc_offset,
            internal_offset,
            test_offset,
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn setup_standard_local_system_clock_core_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let context = rp.pop_raw::<SystemClockContext>();
        let time_value = rp.pop_i64();
        let rc = service.setup_standard_local_system_clock_core(&context, time_value);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn setup_standard_network_system_clock_core_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let context = rp.pop_raw::<SystemClockContext>();
        let accuracy = rp.pop_i64();
        let rc = service.setup_standard_network_system_clock_core(context, accuracy);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn setup_standard_user_system_clock_core_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let automatic_correction = rp.pop_bool();
        let time_point = rp.pop_raw::<SteadyClockTimePoint>();
        let rc =
            service.setup_standard_user_system_clock_core(automatic_correction, time_point);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn setup_time_zone_service_core_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let mut name = [0u8; 0x24];
        for chunk in name.chunks_mut(4) {
            let word = rp.pop_u32().to_le_bytes();
            chunk.copy_from_slice(&word[..chunk.len()]);
        }
        let mut rule_version = [0u8; 0x10];
        for chunk in rule_version.chunks_mut(4) {
            let word = rp.pop_u32().to_le_bytes();
            chunk.copy_from_slice(&word[..chunk.len()]);
        }
        let location_count = rp.pop_u32();
        let time_point = rp.pop_raw::<SteadyClockTimePoint>();
        let rule_buffer = ctx.read_buffer(0);
        let rc = service.setup_time_zone_service_core(
            &name,
            &rule_version,
            location_count,
            &time_point,
            &rule_buffer,
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn setup_ephemeral_network_system_clock_core_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let rc = service.setup_ephemeral_network_system_clock_core();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn get_standard_local_clock_operation_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_or_create_event_handle(
            ctx,
            &service.local_operation.get_event(),
            &service.local_operation_readable_event,
        ) {
            Some(handle) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_copy_objects(handle);
            }
            None => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
        }
    }

    fn get_standard_network_clock_operation_event_for_service_manager_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_or_create_event_handle(
            ctx,
            &service.network_operation.get_event(),
            &service.network_operation_readable_event,
        ) {
            Some(handle) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_copy_objects(handle);
            }
            None => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
        }
    }

    fn get_ephemeral_network_clock_operation_event_for_service_manager_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_or_create_event_handle(
            ctx,
            &service.ephemeral_operation.get_event(),
            &service.ephemeral_operation_readable_event,
        ) {
            Some(handle) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_copy_objects(handle);
            }
            None => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
        }
    }

    fn get_standard_user_system_clock_automatic_correction_updated_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let event = {
            let time = service.time.lock().unwrap();
            time.standard_user_system_clock.get_event()
        };
        match service.get_or_create_event_handle(
            ctx,
            &event,
            &service.user_automatic_correction_readable_event,
        ) {
            Some(handle) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_copy_objects(handle);
            }
            None => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
        }
    }

    fn set_standard_steady_clock_base_time_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let base_time = rp.pop_i64();
        let rc = service.set_standard_steady_clock_base_time(base_time);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn get_closest_alarm_updated_event_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let event = {
            let time = service.time.lock().unwrap();
            time.alarms.get_event()
        };
        match service.get_or_create_event_handle(
            ctx,
            &event,
            &service.closest_alarm_readable_event,
        ) {
            Some(handle) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_copy_objects(handle);
            }
            None => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
        }
    }

    fn check_and_signal_alarms_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let time = service.time.lock().unwrap();
        time.alarms
            .check_and_signal(&time.power_state_request_manager);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_closest_alarm_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let (is_valid, info, out_time) = {
            let time = service.time.lock().unwrap();
            if let Some((alert_time, priority)) = time.alarms.get_closest_alarm() {
                (
                    true,
                    AlarmInfo {
                        alert_time,
                        priority,
                        _padding: 0,
                    },
                    time.alarms.get_raw_time(),
                )
            } else {
                (false, AlarmInfo::default(), 0)
            }
        };
        let mut rb = ResponseBuilder::new(ctx, 8, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(is_valid);
        rb.push_raw(&info);
        rb.push_i64(out_time);
    }
}

impl SessionRequestHandler for TimeServiceManager {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl ServiceFramework for TimeServiceManager {
    fn get_service_name(&self) -> &str {
        "time:m"
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
    use crate::hle::service::service::ServiceFramework;

    #[test]
    fn setup_standard_local_system_clock_core_updates_shared_memory_in_time_m_owner() {
        let service = TimeServiceManager::new(SystemRef::null(), std::ptr::null(), std::ptr::null_mut());
        let context = SystemClockContext {
            offset: 123,
            steady_time_point: SteadyClockTimePoint {
                time_point: 7,
                clock_source_id: [0x11; 16],
            },
        };

        assert_eq!(
            service.setup_standard_local_system_clock_core(&context, 123),
            RESULT_SUCCESS
        );

        let shared_time = service.shared_time();
        let time = shared_time.lock().unwrap();
        assert_eq!(time.shared_memory.get_local_system_context(), context);
    }

    #[test]
    fn setup_standard_user_system_clock_core_updates_automatic_correction_in_shared_memory() {
        let service = TimeServiceManager::new(SystemRef::null(), std::ptr::null(), std::ptr::null_mut());
        let time_point = SteadyClockTimePoint {
            time_point: 42,
            clock_source_id: [0x22; 16],
        };

        assert_eq!(
            service.setup_standard_user_system_clock_core(false, time_point),
            RESULT_SUCCESS
        );

        let shared_time = service.shared_time();
        let time = shared_time.lock().unwrap();
        assert!(!time.shared_memory.get_automatic_correction());
    }

    #[test]
    fn exercised_event_handlers_are_registered() {
        let service = TimeServiceManager::new(SystemRef::null(), std::ptr::null(), std::ptr::null_mut());
        let handlers = ServiceFramework::handlers(&service);
        for cmd in [50u32, 51, 52, 60, 200] {
            let info = handlers.get(&cmd).expect("missing handler");
            assert!(
                info.handler_callback.is_some(),
                "cmd {} should not be None",
                cmd
            );
        }
    }
}
