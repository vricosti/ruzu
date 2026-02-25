// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `time:u` -- Time service stub.
//!
//! Uses the host system's clock for GetCurrentTime.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

// ── time:u ───────────────────────────────────────────────────────────────────

pub struct TimeService;

impl TimeService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for TimeService {
    fn service_name(&self) -> &str {
        "time:u"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("time:u: cmd_id={}", cmd_id);
        match cmd_id {
            // GetStandardUserSystemClock → ISystemClock
            0 => {
                log::info!("time:u: GetStandardUserSystemClock");
                IpcResponse::success().with_move_handle(0)
            }
            // GetStandardNetworkSystemClock → ISystemClock
            1 => {
                log::info!("time:u: GetStandardNetworkSystemClock");
                IpcResponse::success().with_move_handle(0)
            }
            // GetStandardSteadyClock → ISteadyClock
            2 => {
                log::info!("time:u: GetStandardSteadyClock");
                IpcResponse::success().with_move_handle(0)
            }
            // GetTimeZoneService → ITimeZoneService
            3 => {
                log::info!("time:u: GetTimeZoneService");
                IpcResponse::success().with_move_handle(0)
            }
            // GetStandardLocalSystemClock → ISystemClock
            4 => {
                log::info!("time:u: GetStandardLocalSystemClock");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("time:u: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── time:ISystemClock ────────────────────────────────────────────────────────

pub struct SystemClockService;

impl SystemClockService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for SystemClockService {
    fn service_name(&self) -> &str {
        "time:ISystemClock"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("time:ISystemClock: cmd_id={}", cmd_id);
        match cmd_id {
            // GetCurrentTime — Unix timestamp as u64
            0 => {
                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs();
                let lo = now as u32;
                let hi = (now >> 32) as u32;
                log::info!("time:ISystemClock: GetCurrentTime ({})", now);
                IpcResponse::success_with_data(vec![lo, hi])
            }
            // SetCurrentTime
            1 => {
                log::info!("time:ISystemClock: SetCurrentTime (ignored)");
                IpcResponse::success()
            }
            // GetSystemClockContext — zeroed 0x20 bytes (8 u32s)
            2 => {
                log::info!("time:ISystemClock: GetSystemClockContext");
                IpcResponse::success_with_data(vec![0; 8])
            }
            _ => {
                log::warn!("time:ISystemClock: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── time:ISteadyClock ────────────────────────────────────────────────────────

pub struct SteadyClockService;

impl SteadyClockService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for SteadyClockService {
    fn service_name(&self) -> &str {
        "time:ISteadyClock"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("time:ISteadyClock: cmd_id={}", cmd_id);
        match cmd_id {
            // GetCurrentTimePoint — zeroed SteadyClockTimePoint (0x18 bytes = 6 u32s)
            0 => {
                log::info!("time:ISteadyClock: GetCurrentTimePoint");
                IpcResponse::success_with_data(vec![0; 6])
            }
            _ => {
                log::warn!("time:ISteadyClock: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── time:ITimeZoneService ────────────────────────────────────────────────────

pub struct TimeZoneService;

impl TimeZoneService {
    pub fn new() -> Self {
        Self
    }
}

/// Encode a short string as u32 words (NUL-padded).
fn encode_string_words(s: &str) -> Vec<u32> {
    let bytes = s.as_bytes();
    let padded_len = ((bytes.len() + 1) + 3) & !3;
    let mut padded = vec![0u8; padded_len];
    padded[..bytes.len()].copy_from_slice(bytes);

    padded
        .chunks(4)
        .map(|chunk| {
            u32::from_le_bytes([
                chunk[0],
                chunk.get(1).copied().unwrap_or(0),
                chunk.get(2).copied().unwrap_or(0),
                chunk.get(3).copied().unwrap_or(0),
            ])
        })
        .collect()
}

impl ServiceHandler for TimeZoneService {
    fn service_name(&self) -> &str {
        "time:ITimeZoneService"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("time:ITimeZoneService: cmd_id={}", cmd_id);
        match cmd_id {
            // GetDeviceLocationName — "UTC"
            0 => {
                log::info!("time:ITimeZoneService: GetDeviceLocationName (UTC)");
                IpcResponse::success_with_data(encode_string_words("UTC"))
            }
            // ToCalendarTime — zeroed CalendarTime struct
            100 => {
                log::info!("time:ITimeZoneService: ToCalendarTime");
                IpcResponse::success_with_data(vec![0; 8])
            }
            // ToCalendarTimeWithMyRule — zeroed CalendarTime struct
            101 => {
                log::info!("time:ITimeZoneService: ToCalendarTimeWithMyRule");
                IpcResponse::success_with_data(vec![0; 8])
            }
            _ => {
                log::warn!("time:ITimeZoneService: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::CommandType;

    fn make_command(cmd_id: u32) -> IpcCommand {
        IpcCommand {
            command_type: CommandType::Request,
            data_size: 0,
            num_x_bufs: 0,
            num_a_bufs: 0,
            num_b_bufs: 0,
            has_handle_descriptor: false,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            send_pid: false,
            cmif_magic: 0x49434653,
            command_id: cmd_id,
            raw_data: Vec::new(),
            b_buf_addrs: Vec::new(),
        }
    }

    #[test]
    fn test_time_sub_interface_handles() {
        let mut svc = TimeService::new();
        for cmd_id in [0, 1, 2, 3, 4] {
            let cmd = make_command(cmd_id);
            let resp = svc.handle_request(cmd_id, &cmd);
            assert!(resp.result.is_success());
            assert_eq!(resp.handles_to_move, vec![0], "cmd_id={cmd_id}");
        }
    }

    #[test]
    fn test_system_clock_get_current_time() {
        let mut svc = SystemClockService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data.len(), 2); // u64 as two u32s
        // Timestamp should be reasonable (> year 2020)
        let ts = resp.data[0] as u64 | ((resp.data[1] as u64) << 32);
        assert!(ts > 1_577_836_800); // 2020-01-01
    }

    #[test]
    fn test_timezone_location_name() {
        let mut svc = TimeZoneService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert!(!resp.data.is_empty());
    }

    #[test]
    fn test_steady_clock_timepoint() {
        let mut svc = SteadyClockService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data.len(), 6);
    }
}
