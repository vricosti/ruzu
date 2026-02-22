// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `apm` -- Performance Management service.
//!
//! Service hierarchy:
//!   apm -> apm:ISession (SetPerformanceConfiguration, GetPerformanceConfiguration)

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

// ── Root interface: apm ─────────────────────────────────────────────────────

pub struct ApmService;

impl ApmService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ApmService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for ApmService {
    fn service_name(&self) -> &str {
        "apm"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("apm: cmd_id={}", cmd_id);
        match cmd_id {
            // OpenSession
            0 => {
                log::info!("apm: OpenSession");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("apm: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── apm:ISession ────────────────────────────────────────────────────────────

pub struct ApmSessionService;

impl ApmSessionService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ApmSessionService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for ApmSessionService {
    fn service_name(&self) -> &str {
        "apm:ISession"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("apm:ISession: cmd_id={}", cmd_id);
        match cmd_id {
            // SetPerformanceConfiguration
            0 => {
                log::info!("apm:ISession: SetPerformanceConfiguration");
                IpcResponse::success()
            }
            // GetPerformanceConfiguration — docked performance config (0x00020003)
            1 => {
                log::info!("apm:ISession: GetPerformanceConfiguration");
                IpcResponse::success_with_data(vec![0x00020003])
            }
            _ => {
                log::warn!("apm:ISession: unhandled cmd_id={}", cmd_id);
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
        }
    }

    #[test]
    fn test_apm_open_session() {
        let mut svc = ApmService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }

    #[test]
    fn test_apm_session_get_perf_config() {
        let mut svc = ApmSessionService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0x00020003]);
    }

    #[test]
    fn test_apm_session_set_perf_config() {
        let mut svc = ApmSessionService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
    }
}
