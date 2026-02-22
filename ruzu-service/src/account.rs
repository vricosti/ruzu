// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `acc:u0` -- Account service stub.
//!
//! Returns a single fake user with zeroed UID.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

pub struct AccountService;

impl AccountService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for AccountService {
    fn service_name(&self) -> &str {
        "acc:u0"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("acc:u0: cmd_id={}", cmd_id);
        match cmd_id {
            // GetUserCount — 1 user
            0 => {
                log::info!("acc:u0: GetUserCount (1)");
                IpcResponse::success_with_data(vec![1])
            }
            // GetUserExistence — user exists
            1 => {
                log::info!("acc:u0: GetUserExistence (true)");
                IpcResponse::success_with_data(vec![1])
            }
            // ListAllUsers — 1 user with zeroed UID (128-bit = 4 u32s)
            2 => {
                log::info!("acc:u0: ListAllUsers");
                IpcResponse::success_with_data(vec![1, 0, 0, 0])
            }
            // ListOpenUsers
            3 => {
                log::info!("acc:u0: ListOpenUsers");
                IpcResponse::success_with_data(vec![1, 0, 0, 0])
            }
            // GetLastOpenedUser — UID as 4 u32s
            4 => {
                log::info!("acc:u0: GetLastOpenedUser");
                IpcResponse::success_with_data(vec![1, 0, 0, 0])
            }
            // GetProfile → IProfile
            5 => {
                log::info!("acc:u0: GetProfile");
                IpcResponse::success().with_move_handle(0)
            }
            // InitializeApplicationInfo
            100 => {
                log::info!("acc:u0: InitializeApplicationInfo");
                IpcResponse::success()
            }
            // GetBaasAccountManagerForApplication → IBaas
            101 => {
                log::info!("acc:u0: GetBaasAccountManagerForApplication");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("acc:u0: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── acc:IProfile ─────────────────────────────────────────────────────────────

pub struct ProfileService;

impl ProfileService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for ProfileService {
    fn service_name(&self) -> &str {
        "acc:IProfile"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("acc:IProfile: cmd_id={}", cmd_id);
        match cmd_id {
            // Get — zeroed UserData (0x80 bytes = 32 u32s) + ProfileBase (0x38 bytes = 14 u32s)
            0 => {
                log::info!("acc:IProfile: Get");
                IpcResponse::success_with_data(vec![0; 46])
            }
            // GetBase — zeroed ProfileBase (0x38 bytes = 14 u32s)
            1 => {
                log::info!("acc:IProfile: GetBase");
                IpcResponse::success_with_data(vec![0; 14])
            }
            _ => {
                log::warn!("acc:IProfile: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── acc:IBaas ────────────────────────────────────────────────────────────────

pub struct BaasService;

impl BaasService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for BaasService {
    fn service_name(&self) -> &str {
        "acc:IBaas"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("acc:IBaas: cmd_id={}", cmd_id);
        IpcResponse::success()
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
    fn test_get_user_count() {
        let mut svc = AccountService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![1]);
    }

    #[test]
    fn test_get_profile() {
        let mut svc = AccountService::new();
        let cmd = make_command(5);
        let resp = svc.handle_request(5, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }

    #[test]
    fn test_profile_get_base() {
        let mut svc = ProfileService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data.len(), 14);
    }
}
