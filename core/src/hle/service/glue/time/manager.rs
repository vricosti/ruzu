// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/manager.h
//! Port of zuyu/src/core/hle/service/glue/time/manager.cpp
//!
//! TimeManager: orchestrates time service initialization for the glue layer.
//! Gets time:m and set:sys from the ServiceManager (direct handler access,
//! matching upstream's system.ServiceManager().GetService<T>("name", true))
//! and calls their methods directly.

use std::sync::{Arc, Mutex};

use super::file_timestamp_worker::FileTimestampWorker;
use super::standard_steady_clock_resource::StandardSteadyClockResource;
use super::worker::TimeWorker;

use crate::core::SystemRef;
use crate::hle::service::psc::time::common::{
    CalendarTime, StaticServiceSetupInfo, SteadyClockTimePoint, SystemClockContext,
};
use crate::hle::service::psc::time::r#static::StaticService as PscStaticService;
use crate::hle::service::sm::sm::ServiceManager;

/// TimeManager corresponds to upstream `Glue::Time::TimeManager`.
pub struct TimeManager {
    pub worker: TimeWorker,
    pub file_timestamp_worker: FileTimestampWorker,
    pub steady_clock_resource: StandardSteadyClockResource,
    pub psc_time: Arc<Mutex<crate::hle::service::psc::time::manager::TimeManager>>,
    pub time_sm: Arc<PscStaticService>,
    service_manager: Arc<Mutex<ServiceManager>>,
}

impl TimeManager {
    /// Create a new TimeManager with a reference to the ServiceManager.
    ///
    /// Matches upstream `TimeManager::TimeManager(Core::System& system)`.
    pub fn new(service_manager: Arc<Mutex<ServiceManager>>, system: SystemRef) -> Self {
        log::debug!("Glue::Time::TimeManager::new called");
        let psc_time = Arc::new(Mutex::new(
            crate::hle::service::psc::time::manager::TimeManager::new(Box::new(move || {
                if system.is_null() {
                    0
                } else {
                    system
                        .get()
                        .core_timing()
                        .lock()
                        .unwrap()
                        .get_global_time_ns()
                        .as_nanos() as i64
                }
            })),
        ));
        let time_sm = Arc::new(PscStaticService::with_time_manager(
            StaticServiceSetupInfo {
                can_write_local_clock: true,
                can_write_user_clock: true,
                can_write_network_clock: true,
                can_write_timezone_device_location: true,
                can_write_steady_clock: true,
                can_write_uninitialized_clock: false,
            },
            Arc::clone(&psc_time),
        ));
        Self {
            worker: TimeWorker::new(),
            file_timestamp_worker: FileTimestampWorker::new(),
            steady_clock_resource: StandardSteadyClockResource::new(),
            psc_time,
            time_sm,
            service_manager,
        }
    }

    pub fn make_static_service(&self, setup_info: StaticServiceSetupInfo) -> PscStaticService {
        PscStaticService::with_time_manager(setup_info, Arc::clone(&self.psc_time))
    }

    /// Initialize the time manager — the core initialization sequence.
    ///
    /// Upstream constructor calls:
    /// 1. GetService("time:m", true) → direct handler reference
    /// 2. GetService("set:sys", true) → direct handler reference
    /// 3. m_time_zone_binary.Mount() → SynthesizeSystemArchive("TimeZoneBinary")
    /// 4. SetupStandardSteadyClockCore() → calls set:sys Get/SetExternalSteadyClockSourceId
    /// 5. SetupStandardLocalSystemClockCore() via time:m
    /// 6. SetupStandardNetworkSystemClockCore() via time:m
    /// 7. SetupStandardUserSystemClockCore() via time:m
    /// 8. SetupEphemeralNetworkSystemClockCore() via time:m
    /// 9. SetupTimeZoneServiceCore() → calls set:sys Get/SetDeviceTimeZoneLocationName
    pub fn initialize(&mut self) {
        log::info!("Glue::Time::TimeManager: starting initialization");

        // Get set:sys handler via ServiceManager (direct handler access)
        let set_sys_handler = {
            let sm = self.service_manager.lock().unwrap();
            sm.get_service("set:sys")
        };
        let set_sys = match set_sys_handler {
            Some(h) => h,
            None => {
                log::error!("TimeManager: set:sys service not found!");
                return;
            }
        };

        // Get time:m handler via ServiceManager (direct handler access)
        let time_m_handler = {
            let sm = self.service_manager.lock().unwrap();
            sm.get_service("time:m")
        };
        let _time_m = match time_m_handler {
            Some(h) => h,
            None => {
                log::error!("TimeManager: time:m service not found!");
                return;
            }
        };

        // Downcast set:sys to access its methods directly
        // Matches upstream: m_set_sys->GetExternalSteadyClockSourceId(...)
        use crate::hle::service::set::system_settings_server::SystemSettingsService;
        let set_sys_ref = set_sys.as_any().downcast_ref::<SystemSettingsService>();
        if set_sys_ref.is_none() {
            log::error!("TimeManager: failed to downcast set:sys to SystemSettingsService");
            return;
        }
        let set_sys_svc = set_sys_ref.unwrap();

        // === Mount TimeZoneBinary ===
        // Upstream: res = m_time_zone_binary.Mount();
        {
            use crate::hle::service::glue::time::time_zone_binary::TimeZoneBinary;
            let mut tz_binary = TimeZoneBinary::new();
            let res = tz_binary.mount();
            if res.is_error() {
                log::error!("TimeManager: TimeZoneBinary::Mount failed!");
            }
        }

        // === SetupStandardSteadyClockCore ===
        // Upstream: m_set_sys->GetExternalSteadyClockSourceId(&id)
        {
            let inner = set_sys_svc.inner.lock().unwrap();
            let clock_source_id = inner.get_external_steady_clock_source_id();
            log::info!(
                "ISystemSettingsServer::GetExternalSteadyClockSourceId called, clock_source_id={}",
                format_uuid(&clock_source_id)
            );

            let _internal_offset = inner.get_external_steady_clock_internal_offset();
        }

        // Generate a new clock source ID (matching upstream steady_clock_resource.Initialize)
        let new_clock_source_id = generate_clock_source_id();
        {
            let mut inner = set_sys_svc.inner.lock().unwrap();
            inner.set_external_steady_clock_source_id(new_clock_source_id);
            log::info!(
                "ISystemSettingsServer::SetExternalSteadyClockSourceId called, clock_source_id={}",
                format_uuid(&new_clock_source_id)
            );
        }
        {
            let mut time = self.psc_time.lock().unwrap();
            time.standard_steady_clock
                .initialize(new_clock_source_id, 0, 0, 0, false);
            *time.steady_clock_source_id.lock().unwrap() = new_clock_source_id;
            time.shared_memory
                .set_automatic_correction(false);
        }

        // === SetupStandardLocalSystemClockCore ===
        // Upstream: m_set_sys->GetUserSystemClockContext(&context)
        {
            let inner = set_sys_svc.inner.lock().unwrap();
            let user_context = decode_system_clock_context(&inner.get_user_system_clock_context());
            log::info!("ISystemSettingsServer::GetUserSystemClockContext called");
            let epoch_time = get_epoch_time_from_initial_year(set_sys_svc);
            self.psc_time
                .lock()
                .unwrap()
                .standard_local_system_clock
                .initialize(&user_context, epoch_time);
        }

        // === SetupStandardNetworkSystemClockCore ===
        // Upstream: m_set_sys->GetNetworkSystemClockContext(&context)
        {
            let inner = set_sys_svc.inner.lock().unwrap();
            let network_context =
                decode_system_clock_context(&inner.get_network_system_clock_context());
            log::info!("ISystemSettingsServer::GetNetworkSystemClockContext called");
            self.psc_time
                .lock()
                .unwrap()
                .standard_network_system_clock
                .initialize(&network_context, 10 * 24 * 60 * 60 * 1_000_000_000);
        }

        // === SetupStandardUserSystemClockCore ===
        // Upstream: m_set_sys->IsUserSystemClockAutomaticCorrectionEnabled(...)
        //           m_set_sys->GetUserSystemClockAutomaticCorrectionUpdatedTime(...)
        {
            let inner = set_sys_svc.inner.lock().unwrap();
            let enabled = inner.is_user_system_clock_automatic_correction_enabled();
            log::info!(
                "ISystemSettingsServer::IsUserSystemClockAutomaticCorrectionEnabled called, out_automatic_correction_enabled={}",
                enabled
            );
            let updated_time = decode_steady_clock_time_point(
                &inner.get_user_system_clock_automatic_correction_updated_time(),
            );
            log::info!(
                "ISystemSettingsServer::GetUserSystemClockAutomaticCorrectionUpdatedTime called"
            );
            let mut time = self.psc_time.lock().unwrap();
            time.standard_user_system_clock.set_time_point_and_signal(&updated_time);
            time.shared_memory.set_automatic_correction(enabled);
        }

        // === SetupTimeZoneServiceCore ===
        // Upstream: m_set_sys->GetDeviceTimeZoneLocationName(&name)
        //           m_set_sys->SetDeviceTimeZoneLocationName(configured)
        //           m_set_sys->SetDeviceTimeZoneLocationUpdatedTime(...)
        //           m_set_sys->GetDeviceTimeZoneLocationUpdatedTime(...)
        {
            let inner = set_sys_svc.inner.lock().unwrap();
            let name = inner.get_device_time_zone_location_name();
            let name_str = std::str::from_utf8(&name)
                .unwrap_or("")
                .trim_end_matches('\0');
            log::info!("ISystemSettingsServer::GetDeviceTimeZoneLocationName called");
            drop(inner);

            // Set timezone to UTC if not configured
            let mut tz_name = [0u8; 0x24];
            if name_str.is_empty() {
                tz_name[..3].copy_from_slice(b"UTC");
            } else {
                let len = name_str.len().min(0x23);
                tz_name[..len].copy_from_slice(&name_str.as_bytes()[..len]);
            }

            let mut inner = set_sys_svc.inner.lock().unwrap();
            inner.set_device_time_zone_location_name(tz_name);
            log::info!("ISystemSettingsServer::SetDeviceTimeZoneLocationName called");

            inner.set_device_time_zone_location_updated_time([0u8; 0x18]);
            log::info!("ISystemSettingsServer::SetDeviceTimeZoneLocationUpdatedTime called");

            let updated =
                decode_steady_clock_time_point(&inner.get_device_time_zone_location_updated_time());
            log::info!("ISystemSettingsServer::GetDeviceTimeZoneLocationUpdatedTime called");

            let mut time = self.psc_time.lock().unwrap();
            let rc = time.time_zone.parse_binary(&tz_name, b"");
            if rc.is_error() {
                log::error!("TimeManager: failed to set default timezone rule");
            }
            time.time_zone.set_time_point(&updated);
            time.time_zone.set_initialized();
        }

        // === Post-setup: write back clock contexts ===
        // Upstream: m_set_sys->SetUserSystemClockContext(context)
        //           m_set_sys->SetNetworkSystemClockContext(context)
        //           m_set_sys->SetUserSystemClockAutomaticCorrectionEnabled(enabled)
        //           m_set_sys->SetUserSystemClockAutomaticCorrectionUpdatedTime(time_point)
        {
            let mut inner = set_sys_svc.inner.lock().unwrap();
            let context = inner.get_user_system_clock_context();
            inner.set_user_system_clock_context(context);
            log::info!("ISystemSettingsServer::SetUserSystemClockContext called");

            let network_context = inner.get_network_system_clock_context();
            inner.set_network_system_clock_context(network_context);
            log::info!("ISystemSettingsServer::SetNetworkSystemClockContext called");

            let enabled = inner.is_user_system_clock_automatic_correction_enabled();
            inner.set_user_system_clock_automatic_correction_enabled(enabled);
            log::info!(
                "ISystemSettingsServer::SetUserSystemClockAutomaticCorrectionEnabled called, out_automatic_correction_enabled={}",
                enabled
            );

            let time = inner.get_user_system_clock_automatic_correction_updated_time();
            inner.set_user_system_clock_automatic_correction_updated_time(time);
            log::info!(
                "ISystemSettingsServer::SetUserSystemClockAutomaticCorrectionUpdatedTime called"
            );
        }

        self.psc_time
            .lock()
            .unwrap()
            .ephemeral_network_clock
            .clock
            .set_initialized();

        self.worker.initialize();
        log::info!("Glue::Time::TimeManager: initialization complete");
    }
}

fn get_epoch_time_from_initial_year(
    _set_sys: &crate::hle::service::set::system_settings_server::SystemSettingsService,
) -> i64 {
    calendar_time_to_epoch(CalendarTime {
        year: 2023,
        month: 1,
        day: 1,
        hour: 0,
        minute: 0,
        second: 0,
    })
}

fn format_uuid(id: &[u8; 16]) -> String {
    format!(
        "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        id[0], id[1], id[2], id[3], id[4], id[5], id[6], id[7],
        id[8], id[9], id[10], id[11], id[12], id[13], id[14], id[15]
    )
}

fn generate_clock_source_id() -> [u8; 16] {
    // Generate a pseudo-random UUID, matching upstream SteadyClockResource::Initialize
    use std::time::SystemTime;
    let d = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default();
    let mut id = [0u8; 16];
    let nanos = d.as_nanos();
    id[0..8].copy_from_slice(&(nanos as u64).to_le_bytes());
    id[8..16].copy_from_slice(&((nanos >> 64) as u64).to_le_bytes());
    // Set version 4 UUID bits
    id[6] = (id[6] & 0x0f) | 0x40;
    id[8] = (id[8] & 0x3f) | 0x80;
    id
}

fn decode_system_clock_context(bytes: &[u8; 0x20]) -> SystemClockContext {
    unsafe { std::ptr::read_unaligned(bytes.as_ptr() as *const SystemClockContext) }
}

fn decode_steady_clock_time_point(bytes: &[u8; 0x18]) -> SteadyClockTimePoint {
    unsafe { std::ptr::read_unaligned(bytes.as_ptr() as *const SteadyClockTimePoint) }
}

fn calendar_time_to_epoch(calendar: CalendarTime) -> i64 {
    const MONTH_START_DAY_OF_YEAR: [i32; 12] =
        [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];

    let month_s16 = calendar.month as i16;
    let month = (((month_s16 * 43) & !i16::MAX) + ((month_s16 * 43) >> 9)) as i8;
    let mut month_index = calendar.month - 12 * month;
    if month_index == 0 {
        month_index = 12;
    }
    let year = (month as i32 + calendar.year as i32) - i32::from(month_index == 0);
    let v8 = if year >= 0 { year } else { year + 3 };

    let mut days_since_epoch =
        calendar.day as i64 + MONTH_START_DAY_OF_YEAR[month_index as usize - 1] as i64;
    days_since_epoch +=
        (year as i64 * 365) + (v8 / 4) as i64 - (year / 100) as i64 + (year / 400) as i64 - 365;

    let is_leap = |y: i32| -> bool { (y % 4 == 0) && ((y % 100 != 0) || (y % 400 == 0)) };
    if month_index <= 2 && is_leap(year) {
        days_since_epoch -= 1;
    }

    (((24 * days_since_epoch + calendar.hour as i64) * 60 + calendar.minute as i64) * 60
        + calendar.second as i64)
        - 62_135_683_200
}
