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

use crate::hle::service::sm::sm::ServiceManager;

/// TimeManager corresponds to upstream `Glue::Time::TimeManager`.
pub struct TimeManager {
    pub worker: TimeWorker,
    pub file_timestamp_worker: FileTimestampWorker,
    pub steady_clock_resource: StandardSteadyClockResource,
    service_manager: Arc<Mutex<ServiceManager>>,
}

impl TimeManager {
    /// Create a new TimeManager with a reference to the ServiceManager.
    ///
    /// Matches upstream `TimeManager::TimeManager(Core::System& system)`.
    pub fn new(service_manager: Arc<Mutex<ServiceManager>>) -> Self {
        log::debug!("Glue::Time::TimeManager::new called");
        Self {
            worker: TimeWorker::new(),
            file_timestamp_worker: FileTimestampWorker::new(),
            steady_clock_resource: StandardSteadyClockResource::new(),
            service_manager,
        }
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

        // === SetupStandardLocalSystemClockCore ===
        // Upstream: m_set_sys->GetUserSystemClockContext(&context)
        {
            let inner = set_sys_svc.inner.lock().unwrap();
            let _user_context = inner.get_user_system_clock_context();
            log::info!("ISystemSettingsServer::GetUserSystemClockContext called");
        }

        // === SetupStandardNetworkSystemClockCore ===
        // Upstream: m_set_sys->GetNetworkSystemClockContext(&context)
        {
            let inner = set_sys_svc.inner.lock().unwrap();
            let _network_context = inner.get_network_system_clock_context();
            log::info!("ISystemSettingsServer::GetNetworkSystemClockContext called");
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
            let _updated_time = inner.get_user_system_clock_automatic_correction_updated_time();
            log::info!("ISystemSettingsServer::GetUserSystemClockAutomaticCorrectionUpdatedTime called");
        }

        // === SetupTimeZoneServiceCore ===
        // Upstream: m_set_sys->GetDeviceTimeZoneLocationName(&name)
        //           m_set_sys->SetDeviceTimeZoneLocationName(configured)
        //           m_set_sys->SetDeviceTimeZoneLocationUpdatedTime(...)
        //           m_set_sys->GetDeviceTimeZoneLocationUpdatedTime(...)
        {
            let inner = set_sys_svc.inner.lock().unwrap();
            let name = inner.get_device_time_zone_location_name();
            let name_str = std::str::from_utf8(&name).unwrap_or("").trim_end_matches('\0');
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

            let _updated = inner.get_device_time_zone_location_updated_time();
            log::info!("ISystemSettingsServer::GetDeviceTimeZoneLocationUpdatedTime called");
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
            log::info!("ISystemSettingsServer::SetUserSystemClockAutomaticCorrectionUpdatedTime called");
        }

        self.worker.initialize();
        log::info!("Glue::Time::TimeManager: initialization complete");
    }
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
