// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `nifm:u` -- Network Interface service stub.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

pub struct NifmService;

impl NifmService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for NifmService {
    fn service_name(&self) -> &str {
        "nifm:u"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("nifm:u: cmd_id={}", cmd_id);
        match cmd_id {
            // CreateGeneralServiceOld → IGeneralService
            4 => {
                log::info!("nifm:u: CreateGeneralServiceOld");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("nifm:u: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

pub struct GeneralNetworkService;

impl GeneralNetworkService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for GeneralNetworkService {
    fn service_name(&self) -> &str {
        "nifm:IGeneralService"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("nifm:IGeneralService: cmd_id={}", cmd_id);
        IpcResponse::success()
    }
}

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
    fn test_create_general_service() {
        let mut svc = NifmService::new();
        let cmd = make_command(4);
        let resp = svc.handle_request(4, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }
}
