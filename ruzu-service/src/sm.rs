// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/sm/sm.h, sm.cpp, sm_controller.h, sm_controller.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-05
//!
//! `sm:` -- Service Manager service.
//!
//! The Switch's service manager is the first IPC service a process connects to.
//! It provides commands:
//!   0 = Initialize
//!   1 = GetService (CMIF)
//!   2 = RegisterService (CMIF)
//!   3 = UnregisterService
//!   4 = DetachClient
//!
//! Additionally, the SM Controller (IPC control commands) provides:
//!   0 = ConvertCurrentObjectToDomain
//!   2 = CloneCurrentObject
//!   3 = QueryPointerBufferSize
//!   4 = CloneCurrentObjectEx

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};
use common::ResultCode;

// ── SM result codes ──────────────────────────────────────────────────────────
/// Module 21 = SM
const SM_RESULT_INVALID_CLIENT: ResultCode = ResultCode(21 | (2 << 9));
const SM_RESULT_ALREADY_REGISTERED: ResultCode = ResultCode(21 | (4 << 9));
const SM_RESULT_INVALID_SERVICE_NAME: ResultCode = ResultCode(21 | (6 << 9));
const SM_RESULT_NOT_REGISTERED: ResultCode = ResultCode(21 | (7 << 9));

// ── SM: service ──────────────────────────────────────────────────────────────

/// HLE implementation of `sm:`.
pub struct SmService {
    initialized: bool,
    /// After a GetService call, this holds the resolved service name
    /// so the IPC bridge can create a session for it.
    pub last_get_service_name: Option<String>,
}

impl SmService {
    pub fn new() -> Self {
        Self {
            initialized: false,
            last_get_service_name: None,
        }
    }
}

impl Default for SmService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for SmService {
    fn service_name(&self) -> &str {
        "sm:"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("sm: cmd_id={}", cmd_id);

        match cmd_id {
            // ── Initialize (0) ──────────────────────────────────────────
            0 => {
                log::info!("sm: Initialize");
                self.initialized = true;
                IpcResponse::success()
            }

            // ── GetService (1) ──────────────────────────────────────────
            1 => {
                let name = read_service_name(&command.raw_data);
                log::info!("sm: GetService(\"{}\")", name);

                if name.is_empty() || name.len() > 8 {
                    log::error!("sm: Invalid service name '{}'", name);
                    return IpcResponse::error(SM_RESULT_INVALID_SERVICE_NAME);
                }

                // Store the resolved name so the IPC bridge can create a
                // KClientSession for the target service.
                self.last_get_service_name = Some(name);

                // The actual handle creation is performed by the IPC bridge.
                // Return move handle so the bridge knows to create a session.
                IpcResponse::success().with_move_handle(0)
            }

            // ── RegisterService (2) ─────────────────────────────────────
            2 => {
                let name = read_service_name(&command.raw_data);
                log::info!("sm: RegisterService(\"{}\")", name);

                if name.is_empty() || name.len() > 8 {
                    return IpcResponse::error(SM_RESULT_INVALID_SERVICE_NAME);
                }

                // Return move handle (server port) for the registered service.
                IpcResponse::success().with_move_handle(0)
            }

            // ── UnregisterService (3) ───────────────────────────────────
            3 => {
                let name = read_service_name(&command.raw_data);
                log::info!("sm: UnregisterService(\"{}\")", name);
                IpcResponse::success()
            }

            // ── DetachClient (4) ────────────────────────────────────────
            4 => {
                log::info!("sm: DetachClient");
                IpcResponse::success()
            }

            _ => {
                log::warn!("sm: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── SM Controller ────────────────────────────────────────────────────────────

/// HLE implementation of the IPC Controller interface (sm_controller).
///
/// This handles IPC Control commands (CommandType::Control) that are sent
/// to any service session, not just sm:. The commands manage domain
/// conversion and pointer buffer queries.
pub struct SmControllerService;

impl SmControllerService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for SmControllerService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for SmControllerService {
    fn service_name(&self) -> &str {
        "IpcController"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("IpcController: cmd_id={}", cmd_id);

        match cmd_id {
            // ConvertCurrentObjectToDomain (0)
            0 => {
                log::info!("IpcController: ConvertCurrentObjectToDomain");
                // Return domain_id = 1 (sessions converted to domains start at 1)
                IpcResponse::success_with_data(vec![1])
            }

            // CopyFromCurrentDomain (1) — unimplemented in C++
            1 => {
                log::warn!("IpcController: CopyFromCurrentDomain (unimplemented)");
                IpcResponse::success()
            }

            // CloneCurrentObject (2)
            2 => {
                log::info!("IpcController: CloneCurrentObject");
                IpcResponse::success().with_move_handle(0)
            }

            // QueryPointerBufferSize (3)
            3 => {
                log::info!("IpcController: QueryPointerBufferSize -> 0x8000");
                // Return pointer buffer size = 0x8000 (32KB), as u16 packed in u32
                IpcResponse::success_with_data(vec![0x8000])
            }

            // CloneCurrentObjectEx (4) — delegates to CloneCurrentObject
            4 => {
                log::info!("IpcController: CloneCurrentObjectEx");
                IpcResponse::success().with_move_handle(0)
            }

            _ => {
                log::warn!("IpcController: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Helper ───────────────────────────────────────────────────────────────────

/// Read a service name (up to 8 bytes) from the first two raw data words.
pub fn read_service_name(raw_data: &[u32]) -> String {
    let mut bytes = [0u8; 8];

    if let Some(&w0) = raw_data.first() {
        bytes[0..4].copy_from_slice(&w0.to_le_bytes());
    }
    if let Some(&w1) = raw_data.get(1) {
        bytes[4..8].copy_from_slice(&w1.to_le_bytes());
    }

    // Trim trailing NUL bytes and non-printable chars.
    let len = bytes
        .iter()
        .position(|&b| b == 0 || !(b' '..=b'~').contains(&b))
        .unwrap_or(8);
    String::from_utf8_lossy(&bytes[..len]).into_owned()
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::CommandType;

    fn make_command(cmd_id: u32, raw_data: Vec<u32>) -> IpcCommand {
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
            raw_data,
            b_buf_addrs: Vec::new(),
            x_bufs: Vec::new(),
            a_bufs: Vec::new(),
            a_buf_data: Vec::new(),
            b_buf_sizes: Vec::new(),
        }
    }

    #[test]
    fn test_initialize() {
        let mut svc = SmService::new();
        let cmd = make_command(0, vec![]);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert!(svc.initialized);
    }

    #[test]
    fn test_get_service_name_parsing() {
        // "fsp-srv\0" encoded as two little-endian u32s.
        let w0 = u32::from_le_bytes([b'f', b's', b'p', b'-']);
        let w1 = u32::from_le_bytes([b's', b'r', b'v', 0]);
        let name = read_service_name(&[w0, w1]);
        assert_eq!(name, "fsp-srv");
    }

    #[test]
    fn test_get_service() {
        let mut svc = SmService::new();
        let w0 = u32::from_le_bytes([b'h', b'i', b'd', 0]);
        let cmd = make_command(1, vec![w0, 0]);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(svc.last_get_service_name.as_deref(), Some("hid"));
    }

    #[test]
    fn test_get_service_returns_move_handle() {
        let mut svc = SmService::new();
        let w0 = u32::from_le_bytes([b's', b'e', b't', 0]);
        let cmd = make_command(1, vec![w0, 0]);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move.len(), 1);
    }

    #[test]
    fn test_controller_query_pointer_buffer_size() {
        let mut svc = SmControllerService::new();
        let cmd = make_command(3, vec![]);
        let resp = svc.handle_request(3, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0x8000]);
    }

    #[test]
    fn test_controller_convert_to_domain() {
        let mut svc = SmControllerService::new();
        let cmd = make_command(0, vec![]);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1]); // domain_id = 1
    }

    #[test]
    fn test_controller_clone() {
        let mut svc = SmControllerService::new();
        let cmd = make_command(2, vec![]);
        let resp = svc.handle_request(2, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move.len(), 1);
    }
}
