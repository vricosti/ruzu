// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `nsd:u` -- Name Server service stub.

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

pub struct NsdService;

impl NsdService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for NsdService {
    fn service_name(&self) -> &str {
        "nsd:u"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("nsd:u: cmd_id={}", cmd_id);
        IpcResponse::success()
    }
}
