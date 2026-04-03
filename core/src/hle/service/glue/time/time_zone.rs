// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/time_zone.h
//! Port of zuyu/src/core/hle/service/glue/time/time_zone.cpp
//!
//! TimeZoneService: glue-layer timezone service that wraps PSC::Time::TimeZoneService
//! and adds timezone binary file loading support.

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::psc::time::common::{
    CalendarAdditionalInfo, CalendarTime, LocationName, RuleVersion, SteadyClockTimePoint,
};
use crate::hle::service::psc::time::errors::{
    RESULT_PERMISSION_DENIED, RESULT_TIME_ZONE_NOT_FOUND,
};
use crate::hle::service::psc::time::time_zone::TzRule;
use crate::hle::service::psc::time::time_zone_service::TimeZoneService as PscTimeZoneService;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::time_zone_binary::TimeZoneBinary;

/// IPC command IDs for Glue::Time::TimeZoneService.
///
/// Corresponds to the function table in upstream glue/time/time_zone.cpp constructor.
pub mod commands {
    pub const GET_DEVICE_LOCATION_NAME: u32 = 0;
    pub const SET_DEVICE_LOCATION_NAME: u32 = 1;
    pub const GET_TOTAL_LOCATION_NAME_COUNT: u32 = 2;
    pub const LOAD_LOCATION_NAME_LIST: u32 = 3;
    pub const LOAD_TIME_ZONE_RULE: u32 = 4;
    pub const GET_TIME_ZONE_RULE_VERSION: u32 = 5;
    pub const GET_DEVICE_LOCATION_NAME_AND_UPDATED_TIME: u32 = 6;
    pub const SET_DEVICE_LOCATION_NAME_WITH_TIME_ZONE_RULE: u32 = 7;
    pub const PARSE_TIME_ZONE_BINARY: u32 = 8;
    pub const GET_DEVICE_LOCATION_NAME_OPERATION_EVENT_READABLE_HANDLE: u32 = 20;
    pub const TO_CALENDAR_TIME: u32 = 100;
    pub const TO_CALENDAR_TIME_WITH_MY_RULE: u32 = 101;
    pub const TO_POSIX_TIME: u32 = 201;
    pub const TO_POSIX_TIME_WITH_MY_RULE: u32 = 202;
}

/// Glue-layer TimeZoneService.
///
/// Corresponds to `Glue::Time::TimeZoneService` in upstream glue/time/time_zone.h.
/// Wraps `PSC::Time::TimeZoneService` and adds timezone binary support.
///
/// Upstream holds:
/// - `m_wrapped_service` (shared_ptr<PSC::Time::TimeZoneService>)
/// - `m_file_timestamp_worker` (FileTimestampWorker&)
/// - `m_time_zone_binary` (TimeZoneBinary&)
/// - `m_operation_event` (OperationEvent)
/// - `m_set_sys` (shared_ptr<Set::ISystemSettingsServer>)
pub struct TimeZoneService {
    can_write_timezone_device_location: bool,
    /// Wrapped PSC timezone service. Upstream: `m_wrapped_service`.
    wrapped_service: Mutex<PscTimeZoneService>,
    /// TimeZone binary data provider. Upstream: `m_time_zone_binary`.
    time_zone_binary: Mutex<TimeZoneBinary>,
    /// Mutex for location changes. Upstream: `m_mutex`.
    mutex: Mutex<()>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl TimeZoneService {
    fn build_handlers() -> BTreeMap<u32, FunctionInfo> {
        build_handler_map(&[
            (
                commands::GET_DEVICE_LOCATION_NAME,
                Some(TimeZoneService::get_device_location_name_handler),
                "GetDeviceLocationName",
            ),
            (
                commands::SET_DEVICE_LOCATION_NAME,
                None,
                "SetDeviceLocationName",
            ),
            (
                commands::GET_TOTAL_LOCATION_NAME_COUNT,
                Some(TimeZoneService::get_total_location_name_count_handler),
                "GetTotalLocationNameCount",
            ),
            (
                commands::LOAD_LOCATION_NAME_LIST,
                None,
                "LoadLocationNameList",
            ),
            (commands::LOAD_TIME_ZONE_RULE, None, "LoadTimeZoneRule"),
            (
                commands::GET_TIME_ZONE_RULE_VERSION,
                Some(TimeZoneService::get_time_zone_rule_version_handler),
                "GetTimeZoneRuleVersion",
            ),
            (
                commands::GET_DEVICE_LOCATION_NAME_AND_UPDATED_TIME,
                Some(TimeZoneService::get_device_location_name_and_updated_time_handler),
                "GetDeviceLocationNameAndUpdatedTime",
            ),
            (
                commands::SET_DEVICE_LOCATION_NAME_WITH_TIME_ZONE_RULE,
                None,
                "SetDeviceLocationNameWithTimeZoneRule",
            ),
            (
                commands::PARSE_TIME_ZONE_BINARY,
                None,
                "ParseTimeZoneBinary",
            ),
            (
                commands::GET_DEVICE_LOCATION_NAME_OPERATION_EVENT_READABLE_HANDLE,
                None,
                "GetDeviceLocationNameOperationEventReadableHandle",
            ),
            (commands::TO_CALENDAR_TIME, None, "ToCalendarTime"),
            (
                commands::TO_CALENDAR_TIME_WITH_MY_RULE,
                Some(TimeZoneService::to_calendar_time_with_my_rule_handler),
                "ToCalendarTimeWithMyRule",
            ),
            (commands::TO_POSIX_TIME, None, "ToPosixTime"),
            (commands::TO_POSIX_TIME_WITH_MY_RULE, None, "ToPosixTimeWithMyRule"),
        ])
    }

    pub fn new(can_write_timezone_device_location: bool) -> Self {
        let mut tz_binary = TimeZoneBinary::new();
        let _ = tz_binary.mount();
        Self {
            can_write_timezone_device_location,
            wrapped_service: Mutex::new(PscTimeZoneService::new(
                can_write_timezone_device_location,
            )),
            time_zone_binary: Mutex::new(tz_binary),
            mutex: Mutex::new(()),
            handlers: Self::build_handlers(),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Create with an existing wrapped PSC service and timezone binary.
    pub fn with_wrapped(
        can_write_timezone_device_location: bool,
        wrapped_service: PscTimeZoneService,
        time_zone_binary: TimeZoneBinary,
    ) -> Self {
        Self {
            can_write_timezone_device_location,
            wrapped_service: Mutex::new(wrapped_service),
            time_zone_binary: Mutex::new(time_zone_binary),
            mutex: Mutex::new(()),
            handlers: Self::build_handlers(),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const TimeZoneService) }
    }

    /// GetDeviceLocationName (cmd 0).
    ///
    /// Corresponds to `TimeZoneService::GetDeviceLocationName` in upstream.
    /// Delegates to `m_wrapped_service->GetDeviceLocationName`.
    pub fn get_device_location_name(&self) -> Result<LocationName, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::GetDeviceLocationName called");
        self.wrapped_service
            .lock()
            .unwrap()
            .get_device_location_name()
    }

    /// SetDeviceLocationName (cmd 1).
    ///
    /// Corresponds to `TimeZoneService::SetDeviceLocationName` in upstream.
    /// Upstream validates the name against TimeZoneBinary, loads the rule,
    /// updates the PSC service, updates filesystem time, and saves to settings.
    pub fn set_device_location_name(&self, name: &LocationName) -> ResultCode {
        log::debug!("Glue::Time::TimeZoneService::SetDeviceLocationName called");
        if !self.can_write_timezone_device_location {
            return RESULT_PERMISSION_DENIED;
        }

        let tz_binary = self.time_zone_binary.lock().unwrap();
        if !tz_binary.is_valid(name) {
            return RESULT_TIME_ZONE_NOT_FOUND;
        }
        drop(tz_binary);

        let _lock = self.mutex.lock().unwrap();

        let mut tz_binary = self.time_zone_binary.lock().unwrap();
        let binary = match tz_binary.get_time_zone_rule(name) {
            Ok(b) => b,
            Err(rc) => return rc,
        };
        drop(tz_binary);

        let mut wrapped = self.wrapped_service.lock().unwrap();
        let rc = wrapped.set_device_location_name_with_time_zone_rule(name, &binary);
        if rc.is_error() {
            return rc;
        }

        // Upstream also calls m_file_timestamp_worker.SetFilesystemPosixTime(),
        // saves to set:sys, and signals operation events. Settings service
        // integration is not yet wired.

        RESULT_SUCCESS
    }

    /// GetTotalLocationNameCount (cmd 2).
    ///
    /// Corresponds to `TimeZoneService::GetTotalLocationNameCount` in upstream.
    /// Delegates to `m_wrapped_service->GetTotalLocationNameCount`.
    pub fn get_total_location_name_count(&self) -> Result<u32, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::GetTotalLocationNameCount called");
        self.wrapped_service
            .lock()
            .unwrap()
            .get_total_location_name_count()
    }

    /// LoadLocationNameList (cmd 3).
    ///
    /// Corresponds to `TimeZoneService::LoadLocationNameList` in upstream.
    /// Upstream delegates to `m_time_zone_binary.GetTimeZoneLocationList`.
    pub fn load_location_name_list(&self, index: u32) -> Result<Vec<LocationName>, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::LoadLocationNameList called");
        let _lock = self.mutex.lock().unwrap();
        let mut tz_binary = self.time_zone_binary.lock().unwrap();
        // Upstream passes out_names.size() as max_names; we use a reasonable default.
        tz_binary.get_time_zone_location_list(100, index)
    }

    /// LoadTimeZoneRule (cmd 4).
    ///
    /// Corresponds to `TimeZoneService::LoadTimeZoneRule` in upstream.
    /// Upstream loads the binary from TimeZoneBinary and calls
    /// `m_wrapped_service->ParseTimeZoneBinary(out_rule, binary)`.
    pub fn load_time_zone_rule(&self, name: &LocationName) -> Result<TzRule, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::LoadTimeZoneRule called");
        let _lock = self.mutex.lock().unwrap();
        let mut tz_binary = self.time_zone_binary.lock().unwrap();
        let binary = tz_binary.get_time_zone_rule(name)?;
        drop(tz_binary);

        let wrapped = self.wrapped_service.lock().unwrap();
        let mut rule = TzRule::default();
        let rc = wrapped.parse_time_zone_binary(&mut rule, &binary);
        if rc.is_error() {
            return Err(rc);
        }
        Ok(rule)
    }

    /// GetTimeZoneRuleVersion (cmd 5).
    ///
    /// Corresponds to `TimeZoneService::GetTimeZoneRuleVersion` in upstream.
    /// Delegates to `m_wrapped_service->GetTimeZoneRuleVersion`.
    pub fn get_time_zone_rule_version(&self) -> Result<RuleVersion, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::GetTimeZoneRuleVersion called");
        self.wrapped_service
            .lock()
            .unwrap()
            .get_time_zone_rule_version()
    }

    /// GetDeviceLocationNameAndUpdatedTime (cmd 6).
    ///
    /// Corresponds to `TimeZoneService::GetDeviceLocationNameAndUpdatedTime` in upstream.
    /// Delegates to `m_wrapped_service->GetDeviceLocationNameAndUpdatedTime`.
    pub fn get_device_location_name_and_updated_time(
        &self,
    ) -> Result<(LocationName, SteadyClockTimePoint), ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::GetDeviceLocationNameAndUpdatedTime called");
        self.wrapped_service
            .lock()
            .unwrap()
            .get_device_location_name_and_updated_time()
    }

    /// ToCalendarTime (cmd 20).
    ///
    /// Corresponds to `TimeZoneService::ToCalendarTime` in upstream.
    /// Delegates to `m_wrapped_service->ToCalendarTime`.
    pub fn to_calendar_time(
        &self,
        time: i64,
        rule: &TzRule,
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::ToCalendarTime: time={}", time);
        self.wrapped_service
            .lock()
            .unwrap()
            .to_calendar_time(time, rule)
    }

    /// ToCalendarTimeWithMyRule (cmd 21).
    ///
    /// Corresponds to `TimeZoneService::ToCalendarTimeWithMyRule` in upstream.
    /// Delegates to `m_wrapped_service->ToCalendarTimeWithMyRule`.
    pub fn to_calendar_time_with_my_rule(
        &self,
        time: i64,
    ) -> Result<(CalendarTime, CalendarAdditionalInfo), ResultCode> {
        log::debug!(
            "Glue::Time::TimeZoneService::ToCalendarTimeWithMyRule: time={}",
            time
        );
        self.wrapped_service
            .lock()
            .unwrap()
            .to_calendar_time_with_my_rule(time)
    }

    /// ToPosixTime (cmd 100).
    ///
    /// Corresponds to `TimeZoneService::ToPosixTime` in upstream.
    /// Delegates to `m_wrapped_service->ToPosixTime`.
    pub fn to_posix_time(&self, calendar: &CalendarTime, rule: &TzRule) -> Result<i64, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::ToPosixTime called");
        let mut out_times = [0i64; 2];
        let count =
            self.wrapped_service
                .lock()
                .unwrap()
                .to_posix_time(&mut out_times, calendar, rule)?;
        if count == 0 {
            return Ok(0);
        }
        Ok(out_times[0])
    }

    /// ToPosixTimeWithMyRule (cmd 101).
    ///
    /// Corresponds to `TimeZoneService::ToPosixTimeWithMyRule` in upstream.
    /// Delegates to `m_wrapped_service->ToPosixTimeWithMyRule`.
    pub fn to_posix_time_with_my_rule(&self, calendar: &CalendarTime) -> Result<i64, ResultCode> {
        log::debug!("Glue::Time::TimeZoneService::ToPosixTimeWithMyRule called");
        let mut out_times = [0i64; 2];
        let count = self
            .wrapped_service
            .lock()
            .unwrap()
            .to_posix_time_with_my_rule(&mut out_times, calendar)?;
        if count == 0 {
            return Ok(0);
        }
        Ok(out_times[0])
    }

    fn get_device_location_name_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_device_location_name() {
            Ok(location_name) => {
                let mut rb = ResponseBuilder::new(
                    ctx,
                    2 + (core::mem::size_of::<LocationName>() / 4) as u32,
                    0,
                    0,
                );
                rb.push_result(RESULT_SUCCESS);
                rb.push_raw(&location_name);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn get_total_location_name_count_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_total_location_name_count() {
            Ok(count) => {
                let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u32(count);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn get_time_zone_rule_version_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_time_zone_rule_version() {
            Ok(version) => {
                let mut rb = ResponseBuilder::new(
                    ctx,
                    2 + (core::mem::size_of::<RuleVersion>() / 4) as u32,
                    0,
                    0,
                );
                rb.push_result(RESULT_SUCCESS);
                rb.push_raw(&version);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn get_device_location_name_and_updated_time_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        match service.get_device_location_name_and_updated_time() {
            Ok((location_name, time_point)) => {
                let mut rb = ResponseBuilder::new(
                    ctx,
                    2
                        + (core::mem::size_of::<LocationName>() / 4) as u32
                        + (core::mem::size_of::<SteadyClockTimePoint>() / 4) as u32,
                    0,
                    0,
                );
                rb.push_result(RESULT_SUCCESS);
                rb.push_raw(&location_name);
                rb.push_raw(&time_point);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

    fn to_calendar_time_with_my_rule_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let time = rp.pop_i64();
        match service.to_calendar_time_with_my_rule(time) {
            Ok((calendar_time, additional_info)) => {
                let mut rb = ResponseBuilder::new(
                    ctx,
                    2
                        + (core::mem::size_of::<CalendarTime>() / 4) as u32
                        + (core::mem::size_of::<CalendarAdditionalInfo>() / 4) as u32,
                    0,
                    0,
                );
                rb.push_result(RESULT_SUCCESS);
                rb.push_raw(&calendar_time);
                rb.push_raw(&additional_info);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(rc);
            }
        }
    }

}

impl SessionRequestHandler for TimeZoneService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ITimeZoneService"
    }
}

impl ServiceFramework for TimeZoneService {
    fn get_service_name(&self) -> &str {
        "ITimeZoneService"
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

    #[test]
    fn exercised_handlers_are_registered() {
        let service = TimeZoneService::new(false);
        assert!(
            service
                .handlers()
                .get(&commands::GET_DEVICE_LOCATION_NAME)
                .and_then(|info| info.handler_callback)
                .is_some()
        );
        assert!(
            service
                .handlers()
                .get(&commands::GET_TOTAL_LOCATION_NAME_COUNT)
                .and_then(|info| info.handler_callback)
                .is_some()
        );
        assert!(
            service
                .handlers()
                .get(&commands::GET_TIME_ZONE_RULE_VERSION)
                .and_then(|info| info.handler_callback)
                .is_some()
        );
        assert!(
            service
                .handlers()
                .get(&commands::GET_DEVICE_LOCATION_NAME_AND_UPDATED_TIME)
                .and_then(|info| info.handler_callback)
                .is_some()
        );
        assert!(
            service
                .handlers()
                .get(&commands::TO_CALENDAR_TIME_WITH_MY_RULE)
                .and_then(|info| info.handler_callback)
                .is_some()
        );
    }
}
