// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `pctl:s` -- Parental Controls service stub.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

pub struct PctlService;

impl PctlService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for PctlService {
    fn service_name(&self) -> &str {
        "pctl:s"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("pctl:s: cmd_id={}", cmd_id);
        match cmd_id {
            // CreateService → IParentalControlService
            0 => {
                log::info!("pctl:s: CreateService");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("pctl:s: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

pub struct ParentalControlService;

impl ParentalControlService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for ParentalControlService {
    fn service_name(&self) -> &str {
        "pctl:IParentalControlService"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("pctl:IParentalControlService: cmd_id={}", cmd_id);
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
    fn test_create_service() {
        let mut svc = PctlService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }
}
