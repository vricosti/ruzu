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

    pub fn from_u8(val: u8) -> Option<Self> {
        match val {
            0 => Some(LogSeverity::Trace),
            1 => Some(LogSeverity::Info),
            2 => Some(LogSeverity::Warning),
            3 => Some(LogSeverity::Error),
            4 => Some(LogSeverity::Fatal),
            _ => None,
        }
    }
}

/// Log destination flags. Upstream: `LogDestination` in `lm.cpp`.
///
/// Uses bitflags via manual implementation to match upstream `DECLARE_ENUM_FLAG_OPERATORS`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LogDestination(pub u32);

impl LogDestination {
    pub const TARGET_MANAGER: Self = Self(1 << 0);
    pub const UART: Self = Self(1 << 1);
    pub const UART_SLEEP: Self = Self(1 << 2);
    pub const ALL: Self = Self(0xffff);

    pub fn contains(&self, other: Self) -> bool {
        (self.0 & other.0) != 0
    }
}

/// Log packet flags. Upstream: `LogPacketFlags` in `lm.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LogPacketFlags(pub u8);

impl LogPacketFlags {
    pub const HEAD: Self = Self(1 << 0);
    pub const TAIL: Self = Self(1 << 1);
    pub const LITTLE_ENDIAN: Self = Self(1 << 2);

    pub fn contains(&self, other: Self) -> bool {
        (self.0 & other.0) != 0
    }
}

/// LogPacketHeaderEntry - used as key for multi-packet log assembly.
/// Upstream: `LogPacketHeaderEntry` in `lm.cpp`.
///
/// Keeps flags and payload_size out of the hash/equality.
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

const _: () = assert!(std::mem::size_of::<LogPacketHeader>() == 0x18);

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

impl LogDataChunkKey {
    fn from_u64(val: u64) -> Option<Self> {
        match val {
            0 => Some(Self::LogSessionBegin),
            1 => Some(Self::LogSessionEnd),
            2 => Some(Self::TextLog),
            3 => Some(Self::LineNumber),
            4 => Some(Self::FileName),
            5 => Some(Self::FunctionName),
            6 => Some(Self::ModuleName),
            7 => Some(Self::ThreadName),
            8 => Some(Self::LogPacketDropCount),
            9 => Some(Self::UserSystemClock),
            10 => Some(Self::ProcessName),
            _ => None,
        }
    }
}

/// Convert destination flags to a human-readable string.
///
/// Corresponds to `DestinationToString` in upstream lm.cpp.
fn destination_to_string(destination: LogDestination) -> String {
    if destination.contains(LogDestination::ALL) {
        return "TargetManager | Uart | UartSleep".to_string();
    }
    let mut output = String::new();
    if destination.contains(LogDestination::TARGET_MANAGER) {
        output += "| TargetManager";
    }
    if destination.contains(LogDestination::UART) {
        output += "| Uart";
    }
    if destination.contains(LogDestination::UART_SLEEP) {
        output += "| UartSleep";
    }
    if output.len() > 2 {
        output[2..].to_string()
    } else {
        "No Destination".to_string()
    }
}

/// Read a LEB128-encoded u64 from data at the given offset.
///
/// Corresponds to `ILogger::ReadLeb128` in upstream lm.cpp.
fn read_leb128(data: &[u8], offset: &mut usize) -> u64 {
    let mut result: u64 = 0;
    let mut shift: u32 = 0;

    for _ in 0..std::mem::size_of::<u64>() {
        if *offset >= data.len() {
            break;
        }
        let v = data[*offset];
        result |= ((v & 0x7f) as u64) << shift;
        shift += 7;
        *offset += 1;
        if *offset >= data.len() || (v & 0x80) == 0 {
            break;
        }
    }
    result
}

/// Read a string of the given length from data at offset.
///
/// Corresponds to `ILogger::ReadString` in upstream lm.cpp.
fn read_string(data: &[u8], offset: &mut usize, length: u64) -> Option<String> {
    if length == 0 {
        return None;
    }
    let length_to_read = (length as usize).min(data.len().saturating_sub(*offset));
    let s = String::from_utf8_lossy(&data[*offset..*offset + length_to_read]).to_string();
    *offset += length_to_read;
    Some(s)
}

/// Read a u32 from data at offset.
///
/// Corresponds to `ILogger::ReadAsU32` in upstream lm.cpp.
fn read_as_u32(data: &[u8], offset: &mut usize, length: u64) -> u32 {
    assert!(length == 4);
    if *offset + 4 > data.len() {
        *offset += length as usize;
        return 0;
    }
    let val = u32::from_le_bytes([
        data[*offset],
        data[*offset + 1],
        data[*offset + 2],
        data[*offset + 3],
    ]);
    *offset += length as usize;
    val
}

/// Read a u64 from data at offset.
///
/// Corresponds to `ILogger::ReadAsU64` in upstream lm.cpp.
fn read_as_u64(data: &[u8], offset: &mut usize, length: u64) -> u64 {
    assert!(length == 8);
    if *offset + 8 > data.len() {
        *offset += length as usize;
        return 0;
    }
    let val = u64::from_le_bytes([
        data[*offset],
        data[*offset + 1],
        data[*offset + 2],
        data[*offset + 3],
        data[*offset + 4],
        data[*offset + 5],
        data[*offset + 6],
        data[*offset + 7],
    ]);
    *offset += length as usize;
    val
}

/// Parse a complete log entry and emit it.
///
/// Corresponds to `ILogger::ParseLog` in upstream lm.cpp.
fn parse_log(entry: &LogPacketHeaderEntry, log_data: &[u8], destination: LogDestination) {
    let mut text_log: Option<String> = None;
    let mut line_number: Option<u32> = None;
    let mut file_name: Option<String> = None;
    let mut function_name: Option<String> = None;
    let mut module_name: Option<String> = None;
    let mut _thread_name: Option<String> = None;
    let mut _log_pack_drop_count: Option<u64> = None;
    let mut _user_system_clock: Option<u64> = None;
    let mut process_name: Option<String> = None;

    let mut offset: usize = 0;
    while offset < log_data.len() {
        let key_raw = read_leb128(log_data, &mut offset);
        let chunk_size = read_leb128(log_data, &mut offset);

        match LogDataChunkKey::from_u64(key_raw) {
            Some(LogDataChunkKey::LogSessionBegin) | Some(LogDataChunkKey::LogSessionEnd) => {
                // No data to read for these keys.
                offset += chunk_size as usize;
            }
            Some(LogDataChunkKey::TextLog) => {
                text_log = read_string(log_data, &mut offset, chunk_size);
            }
            Some(LogDataChunkKey::LineNumber) => {
                line_number = Some(read_as_u32(log_data, &mut offset, chunk_size));
            }
            Some(LogDataChunkKey::FileName) => {
                file_name = read_string(log_data, &mut offset, chunk_size);
            }
            Some(LogDataChunkKey::FunctionName) => {
                function_name = read_string(log_data, &mut offset, chunk_size);
            }
            Some(LogDataChunkKey::ModuleName) => {
                module_name = read_string(log_data, &mut offset, chunk_size);
            }
            Some(LogDataChunkKey::ThreadName) => {
                _thread_name = read_string(log_data, &mut offset, chunk_size);
            }
            Some(LogDataChunkKey::LogPacketDropCount) => {
                _log_pack_drop_count = Some(read_as_u64(log_data, &mut offset, chunk_size));
            }
            Some(LogDataChunkKey::UserSystemClock) => {
                _user_system_clock = Some(read_as_u64(log_data, &mut offset, chunk_size));
            }
            Some(LogDataChunkKey::ProcessName) => {
                process_name = read_string(log_data, &mut offset, chunk_size);
            }
            None => {
                // Unknown key, skip.
                offset += chunk_size as usize;
            }
        }
    }

    // Build the output log string, matching upstream format.
    let mut output_log = String::new();
    if let Some(ref name) = process_name {
        output_log += &format!("Process: {}\n", name);
    }
    if let Some(ref name) = module_name {
        output_log += &format!("Module: {}\n", name);
    }
    if let Some(ref name) = file_name {
        output_log += &format!("File: {}\n", name);
    }
    if let Some(ref name) = function_name {
        output_log += &format!("Function: {}\n", name);
    }
    if let Some(line) = line_number {
        if line != 0 {
            output_log += &format!("Line: {}\n", line);
        }
    }
    output_log += &format!("ProcessID: {:X}\n", entry.pid);
    output_log += &format!("ThreadID: {:X}\n", entry.tid);

    if let Some(ref text) = text_log {
        output_log += &format!("Log Text: {}\n", text);
    }

    let severity_name = LogSeverity::from_u8(entry.severity)
        .map(|s| s.name())
        .unwrap_or("UNKNOWN");

    log::debug!(
        "LogManager {} ({}):\n{}",
        severity_name,
        destination_to_string(destination),
        output_log
    );
}

/// ILogger interface returned by LM::OpenLogger.
///
/// Corresponds to `ILogger` in upstream lm.cpp.
pub struct ILogger {
    entries: HashMap<LogPacketHeaderEntry, Vec<u8>>,
    destination: LogDestination,
}

impl ILogger {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            destination: LogDestination::ALL,
        }
    }

    /// Log (cmd 0) - processes log packets.
    ///
    /// Corresponds to `ILogger::Log` in upstream lm.cpp.
    /// This function always succeeds (matching upstream).
    pub fn log(&mut self, data: &[u8]) {
        if data.len() < std::mem::size_of::<LogPacketHeader>() {
            log::error!(
                "Data size is too small for header! size={}",
                data.len()
            );
            return;
        }

        // Read header from raw bytes.
        let header_size = std::mem::size_of::<LogPacketHeader>();
        let pid = u64::from_le_bytes(data[0..8].try_into().unwrap());
        let tid = u64::from_le_bytes(data[8..16].try_into().unwrap());
        let flags = LogPacketFlags(data[16]);
        let severity = data[18];
        let verbosity = data[19];
        let _payload_size = u32::from_le_bytes(data[20..24].try_into().unwrap());

        let entry = LogPacketHeaderEntry {
            pid,
            tid,
            severity,
            verbosity,
        };

        if flags.contains(LogPacketFlags::HEAD) {
            // Start a new entry.
            let payload = data[header_size..].to_vec();
            self.entries.insert(entry, payload);
        } else {
            // Append to existing entry.
            match self.entries.get_mut(&entry) {
                Some(existing) => {
                    existing.extend_from_slice(&data[header_size..]);
                }
                None => {
                    log::error!("Log entry does not exist!");
                    return;
                }
            }
        }

        if flags.contains(LogPacketFlags::TAIL) {
            if let Some(log_data) = self.entries.remove(&entry) {
                parse_log(&entry, &log_data, self.destination);
            } else {
                log::error!("Log entry does not exist!");
            }
        }
    }

    /// SetDestination (cmd 1).
    ///
    /// Corresponds to `ILogger::SetDestination` in upstream lm.cpp.
    pub fn set_destination(&mut self, destination: u32) {
        let dest = LogDestination(destination);
        log::debug!(
            "ILogger::set_destination called, destination={}",
            destination_to_string(dest)
        );
        self.destination = dest;
    }
}

/// LM service ("lm").
///
/// Corresponds to `LM` in upstream lm.cpp.
pub struct LM {
    handlers: std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo>,
    handlers_tipc: std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo>,
}

impl LM {
    pub fn new() -> Self {
        let handlers = crate::hle::service::service::build_handler_map(&[
            (0, Some(LM::open_logger_handler), "OpenLogger"),
        ]);
        Self {
            handlers,
            handlers_tipc: std::collections::BTreeMap::new(),
        }
    }

    fn open_logger_handler(this: &dyn crate::hle::service::service::ServiceFramework, ctx: &mut crate::hle::service::hle_ipc::HLERequestContext) {
        log::debug!("LM::OpenLogger called");
        // Upstream creates an ILogger domain object. For bring-up, just return success.
        let mut rb = crate::hle::service::ipc_helpers::ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
    }
}

impl crate::hle::service::hle_ipc::SessionRequestHandler for LM {
    fn handle_sync_request(&self, ctx: &mut crate::hle::service::hle_ipc::HLERequestContext) -> crate::hle::result::ResultCode {
        use crate::hle::service::service::ServiceFramework;
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "lm"
    }
}

impl crate::hle::service::service::ServiceFramework for LM {
    fn get_service_name(&self) -> &str {
        "lm"
    }

    fn get_max_sessions(&self) -> u32 {
        42
    }

    fn handlers(&self) -> &std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &std::collections::BTreeMap<u32, crate::hle::service::service::FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_leb128() {
        // Single byte: 0x05
        let data = [0x05u8];
        let mut offset = 0;
        assert_eq!(read_leb128(&data, &mut offset), 5);
        assert_eq!(offset, 1);

        // Two bytes: 0x80 0x01 = 128
        let data2 = [0x80u8, 0x01];
        offset = 0;
        assert_eq!(read_leb128(&data2, &mut offset), 128);
        assert_eq!(offset, 2);
    }

    #[test]
    fn test_read_string() {
        let data = b"Hello World";
        let mut offset = 0;
        let result = read_string(data, &mut offset, 5);
        assert_eq!(result, Some("Hello".to_string()));
        assert_eq!(offset, 5);
    }

    #[test]
    fn test_destination_to_string() {
        assert_eq!(
            destination_to_string(LogDestination::ALL),
            "TargetManager | Uart | UartSleep"
        );
        assert_eq!(
            destination_to_string(LogDestination(0)),
            "No Destination"
        );
        // Any non-zero destination matches ALL (0xffff) via bitwise AND,
        // so individual flags also produce the "All" string -- matching upstream.
        assert_eq!(
            destination_to_string(LogDestination::TARGET_MANAGER),
            "TargetManager | Uart | UartSleep"
        );
    }

    #[test]
    fn test_logger_single_packet() {
        let mut logger = ILogger::new();

        // Build a minimal packet: HEAD | TAIL, severity=Info, no payload
        let mut data = vec![0u8; 24]; // LogPacketHeader size
        // pid = 1
        data[0..8].copy_from_slice(&1u64.to_le_bytes());
        // tid = 2
        data[8..16].copy_from_slice(&2u64.to_le_bytes());
        // flags = HEAD | TAIL
        data[16] = 0x03;
        // severity = Info (1)
        data[18] = 1;
        // verbosity = 0
        data[19] = 0;
        // payload_size = 0
        data[20..24].copy_from_slice(&0u32.to_le_bytes());

        logger.log(&data);
        // Should not panic. Entry should be processed and removed.
        assert!(logger.entries.is_empty());
    }

    #[test]
    fn test_logger_multi_packet() {
        let mut logger = ILogger::new();

        let entry = LogPacketHeaderEntry {
            pid: 1,
            tid: 2,
            severity: 1,
            verbosity: 0,
        };

        // HEAD packet
        let mut head_data = vec![0u8; 24];
        head_data[0..8].copy_from_slice(&1u64.to_le_bytes());
        head_data[8..16].copy_from_slice(&2u64.to_le_bytes());
        head_data[16] = 0x01; // HEAD
        head_data[18] = 1;
        head_data[19] = 0;
        head_data[20..24].copy_from_slice(&0u32.to_le_bytes());
        // Add some payload
        head_data.extend_from_slice(&[0xAA, 0xBB]);

        logger.log(&head_data);
        assert!(logger.entries.contains_key(&entry));

        // Middle packet (no HEAD, no TAIL)
        let mut mid_data = vec![0u8; 24];
        mid_data[0..8].copy_from_slice(&1u64.to_le_bytes());
        mid_data[8..16].copy_from_slice(&2u64.to_le_bytes());
        mid_data[16] = 0x00; // no flags
        mid_data[18] = 1;
        mid_data[19] = 0;
        mid_data[20..24].copy_from_slice(&0u32.to_le_bytes());
        mid_data.extend_from_slice(&[0xCC]);

        logger.log(&mid_data);
        assert_eq!(logger.entries[&entry].len(), 3);

        // TAIL packet
        let mut tail_data = vec![0u8; 24];
        tail_data[0..8].copy_from_slice(&1u64.to_le_bytes());
        tail_data[8..16].copy_from_slice(&2u64.to_le_bytes());
        tail_data[16] = 0x02; // TAIL
        tail_data[18] = 1;
        tail_data[19] = 0;
        tail_data[20..24].copy_from_slice(&0u32.to_le_bytes());

        logger.log(&tail_data);
        // Entry should be consumed.
        assert!(logger.entries.is_empty());
    }
}
