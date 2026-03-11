// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/lm/lm.cpp
//!
//! Log Manager service ("lm") and ILogger interface.

use std::collections::HashMap;

/// IPC command IDs for LM
pub mod lm_commands {
    pub const OPEN_LOGGER: u32 = 0;
}

/// IPC command IDs for ILogger
pub mod logger_commands {
    pub const LOG: u32 = 0;
    pub const SET_DESTINATION: u32 = 1;
}

/// Log severity levels. Upstream: `LogSeverity` in `lm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum LogSeverity {
    Trace = 0,
    Info = 1,
    Warning = 2,
    Error = 3,
    Fatal = 4,
}

impl LogSeverity {
    pub fn name(&self) -> &'static str {
        match self {
            LogSeverity::Trace => "TRACE",
            LogSeverity::Info => "INFO",
            LogSeverity::Warning => "WARNING",
            LogSeverity::Error => "ERROR",
            LogSeverity::Fatal => "FATAL",
        }
    }
}

/// Log destination flags. Upstream: `LogDestination` in `lm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum LogDestination {
    TargetManager = 1 << 0,
    Uart = 1 << 1,
    UartSleep = 1 << 2,
    All = 0xffff,
}

/// Log packet flags. Upstream: `LogPacketFlags` in `lm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum LogPacketFlags {
    Head = 1 << 0,
    Tail = 1 << 1,
    LittleEndian = 1 << 2,
}

/// LogPacketHeaderEntry - used as key for multi-packet log assembly.
/// Upstream: `LogPacketHeaderEntry` in `lm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct LogPacketHeaderEntry {
    pub pid: u64,
    pub tid: u64,
    pub severity: u8,
    pub verbosity: u8,
}

/// LogPacketHeader. Upstream: `LogPacketHeader` in `lm.cpp`.
#[repr(C)]
pub struct LogPacketHeader {
    pub pid: u64,
    pub tid: u64,
    pub flags: u8,
    pub _padding: u8,
    pub severity: u8,
    pub verbosity: u8,
    pub payload_size: u32,
}

/// Log data chunk key. Upstream: `LogDataChunkKey` in `lm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum LogDataChunkKey {
    LogSessionBegin = 0,
    LogSessionEnd = 1,
    TextLog = 2,
    LineNumber = 3,
    FileName = 4,
    FunctionName = 5,
    ModuleName = 6,
    ThreadName = 7,
    LogPacketDropCount = 8,
    UserSystemClock = 9,
    ProcessName = 10,
}

/// ILogger interface returned by LM::OpenLogger.
pub struct ILogger {
    entries: HashMap<LogPacketHeaderEntry, Vec<u8>>,
    destination: u32,
}

impl ILogger {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            destination: LogDestination::All as u32,
        }
    }

    /// Log (cmd 0) - processes log packets
    pub fn log(&mut self, _data: &[u8]) {
        // TODO: implement full log packet parsing matching upstream
        log::debug!("ILogger::log called");
    }

    /// SetDestination (cmd 1)
    pub fn set_destination(&mut self, destination: u32) {
        log::debug!("ILogger::set_destination called, destination={}", destination);
        self.destination = destination;
    }
}

/// LM service ("lm").
pub struct LM;

impl LM {
    pub fn new() -> Self {
        Self
    }

    /// OpenLogger (cmd 0) - creates an ILogger interface
    pub fn open_logger(&self) -> ILogger {
        log::debug!("LM::open_logger called");
        ILogger::new()
    }
}

/// Registers "lm" service.
///
/// Corresponds to `LoopProcess` in upstream `lm.cpp`.
pub fn loop_process() {
    // TODO: register "lm" -> LM with ServerManager
}
