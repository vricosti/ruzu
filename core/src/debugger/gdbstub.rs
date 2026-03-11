// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/debugger/gdbstub.h and gdbstub.cpp
//! GDB stub for remote debugging.

use std::collections::BTreeMap;

use crate::debugger::debugger_interface::{DebuggerAction, DebuggerBackend, DebuggerFrontend};
use crate::debugger::gdbstub_arch::{GdbStubArch, GdbStubA32, GdbStubA64};

// GDB protocol constants (matching upstream)
const GDB_STUB_START: u8 = b'$';
const GDB_STUB_END: u8 = b'#';
const GDB_STUB_ACK: u8 = b'+';
const GDB_STUB_NACK: u8 = b'-';
const GDB_STUB_INT3: u8 = 0x03;
const GDB_STUB_SIGTRAP: u8 = 5;

const GDB_STUB_REPLY_ERR: &str = "E01";
const GDB_STUB_REPLY_OK: &str = "OK";
const GDB_STUB_REPLY_EMPTY: &str = "";

/// GDB stub frontend.
///
/// Corresponds to upstream `Core::GDBStub`.
pub struct GdbStub {
    // TODO: system, debug_process references
    arch: Box<dyn GdbStubArch>,
    current_command: Vec<u8>,
    replaced_instructions: BTreeMap<u64, u32>,
    no_ack: bool,
    is_64bit: bool,
}

impl GdbStub {
    /// Create a new GDB stub.
    ///
    /// Corresponds to upstream `GDBStub::GDBStub`.
    pub fn new(
        _backend: &dyn DebuggerBackend,
        _system: &dyn std::any::Any, // TODO: Core::System
        is_64bit: bool,
    ) -> Self {
        let arch: Box<dyn GdbStubArch> = if is_64bit {
            Box::new(GdbStubA64)
        } else {
            Box::new(GdbStubA32)
        };

        Self {
            arch,
            current_command: Vec::new(),
            replaced_instructions: BTreeMap::new(),
            no_ack: false,
            is_64bit,
        }
    }
}

impl DebuggerFrontend for GdbStub {
    fn connected(&mut self) {
        // Nothing to do on connection
    }

    fn stopped(&mut self, _thread_id: u64) {
        // TODO: Send thread status reply
        // SendReply(arch->ThreadStatus(thread, GDB_STUB_SIGTRAP));
    }

    fn shutting_down(&mut self) {
        // Nothing to do on shutdown
    }

    fn watchpoint(&mut self, _thread_id: u64, _watch_addr: u64, _watch_type: u8) {
        // TODO: Send watchpoint-specific reply based on watch type
    }

    fn client_data(&mut self, data: &[u8]) -> Vec<DebuggerAction> {
        let mut actions = Vec::new();
        self.current_command.extend_from_slice(data);

        while !self.current_command.is_empty() {
            self.process_data(&mut actions);
        }

        actions
    }
}

impl GdbStub {
    /// Process incoming data and generate debugger actions.
    ///
    /// Corresponds to upstream `GDBStub::ProcessData`.
    fn process_data(&mut self, actions: &mut Vec<DebuggerAction>) {
        if self.current_command.is_empty() {
            return;
        }

        let c = self.current_command[0];

        // Acknowledgement
        if c == GDB_STUB_ACK || c == GDB_STUB_NACK {
            self.current_command.remove(0);
            return;
        }

        // Interrupt
        if c == GDB_STUB_INT3 {
            log::info!("GDB: Received interrupt");
            self.current_command.remove(0);
            actions.push(DebuggerAction::Interrupt);
            return;
        }

        // Require start of command
        if c != GDB_STUB_START {
            log::error!("GDB: Invalid command buffer contents");
            self.current_command.clear();
            return;
        }

        // TODO: Full command parsing, checksum validation, and dispatch
        // Upstream:
        //   1. Read until GDB_STUB_END + 2 checksum chars
        //   2. Validate checksum
        //   3. Dispatch to ExecuteCommand
        // For now, clear the command buffer
        self.current_command.clear();
    }

    /// Calculate GDB checksum.
    ///
    /// Corresponds to upstream `CalculateChecksum`.
    fn calculate_checksum(data: &[u8]) -> u8 {
        data.iter().fold(0u8, |acc, &b| acc.wrapping_add(b))
    }

    /// Escape special GDB characters.
    ///
    /// Corresponds to upstream `EscapeGDB`.
    fn escape_gdb(data: &str) -> String {
        let mut escaped = String::with_capacity(data.len());
        for c in data.bytes() {
            match c {
                b'#' => escaped.push_str("}\x03"),
                b'$' => escaped.push_str("}\x04"),
                b'*' => escaped.push_str("}\x0a"),
                b'}' => escaped.push_str("}\x5d"),
                _ => escaped.push(c as char),
            }
        }
        escaped
    }
}

/// Breakpoint types.
///
/// Corresponds to upstream anonymous `BreakpointType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
enum BreakpointType {
    Software = 0,
    Hardware = 1,
    WriteWatch = 2,
    ReadWatch = 3,
    AccessWatch = 4,
}
