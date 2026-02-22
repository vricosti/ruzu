// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `ssl` -- SSL service stub.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

pub struct SslService;

impl SslService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for SslService {
    fn service_name(&self) -> &str {
        "ssl"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("ssl: cmd_id={}", cmd_id);
        IpcResponse::success()
    }
}
