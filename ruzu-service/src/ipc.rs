// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! IPC message parser for HIPC (CommandHeader) and CMIF (SFCI/SFCO) layers.
//!
//! The TLS message buffer is 0x100 bytes. The first two words form the HIPC
//! command header; an optional handle descriptor follows; then the CMIF payload
//! starts at a u32-aligned offset with magic `SFCI` (requests) or `SFCO`
//! (responses).

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use ruzu_common::ResultCode;
use std::io::Cursor;

/// CMIF request magic: "SFCI" in little-endian.
pub const SFCI_MAGIC: u32 = 0x49434653;

/// CMIF response magic: "SFCO" in little-endian.
pub const SFCO_MAGIC: u32 = 0x4F434653;

/// Size of the per-thread TLS IPC message buffer.
pub const TLS_MESSAGE_SIZE: usize = 0x100;

// ── Command type ─────────────────────────────────────────────────────────────

/// HIPC command types encoded in bits [15:0] of word 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum CommandType {
    Invalid = 0,
    LegacyRequest = 1,
    Close = 2,
    LegacyControl = 3,
    Request = 4,
    Control = 5,
    RequestWithContext = 6,
    ControlWithContext = 7,
}

impl CommandType {
    pub fn from_u16(value: u16) -> Self {
        match value {
            1 => Self::LegacyRequest,
            2 => Self::Close,
            3 => Self::LegacyControl,
            4 => Self::Request,
            5 => Self::Control,
            6 => Self::RequestWithContext,
            7 => Self::ControlWithContext,
            _ => Self::Invalid,
        }
    }
}

// ── Parsed IPC command ───────────────────────────────────────────────────────

/// A parsed HIPC + CMIF IPC command.
#[derive(Debug, Clone)]
pub struct IpcCommand {
    /// HIPC command type.
    pub command_type: CommandType,
    /// Size of the raw data section (in u32 words).
    pub data_size: u32,
    /// Number of X (pointer) descriptors.
    pub num_x_bufs: u32,
    /// Number of A (send) descriptors.
    pub num_a_bufs: u32,
    /// Number of B (receive) descriptors.
    pub num_b_bufs: u32,
    /// Whether the handle descriptor is present.
    pub has_handle_descriptor: bool,
    /// Handles to copy (from handle descriptor).
    pub handles_to_copy: Vec<u32>,
    /// Handles to move (from handle descriptor).
    pub handles_to_move: Vec<u32>,
    /// Whether a PID is sent with the handle descriptor.
    pub send_pid: bool,
    /// CMIF magic (SFCI for requests).
    pub cmif_magic: u32,
    /// CMIF command id.
    pub command_id: u32,
    /// Remaining raw data words after the CMIF header.
    pub raw_data: Vec<u32>,
    /// Guest virtual addresses of B-type (receive) output buffers.
    /// Services write their output data to these addresses.
    pub b_buf_addrs: Vec<u64>,
}

// ── Outgoing IPC response ────────────────────────────────────────────────────

/// Builder for an HIPC + CMIF response.
#[derive(Debug, Clone)]
pub struct IpcResponse {
    /// Result code.
    pub result: ResultCode,
    /// Extra data words to include after the result code (inline CMIF payload).
    pub data: Vec<u32>,
    /// Handles to copy into the client.
    pub handles_to_copy: Vec<u32>,
    /// Handles to move into the client.
    pub handles_to_move: Vec<u32>,
    /// Data to write into the caller's B-type output buffers.
    /// `out_bufs[i]` is written to `command.b_buf_addrs[i]` in guest memory.
    pub out_bufs: Vec<Vec<u8>>,
}

impl IpcResponse {
    /// Create a success response with no extra data.
    pub fn success() -> Self {
        Self {
            result: ResultCode::SUCCESS,
            data: Vec::new(),
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            out_bufs: Vec::new(),
        }
    }

    /// Create a success response carrying extra data words.
    pub fn success_with_data(data: Vec<u32>) -> Self {
        Self {
            result: ResultCode::SUCCESS,
            data,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            out_bufs: Vec::new(),
        }
    }

    /// Create an error response.
    pub fn error(result: ResultCode) -> Self {
        Self {
            result,
            data: Vec::new(),
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            out_bufs: Vec::new(),
        }
    }

    /// Attach a handle to copy into the response.
    pub fn with_copy_handle(mut self, handle: u32) -> Self {
        self.handles_to_copy.push(handle);
        self
    }

    /// Attach a handle to move into the response.
    pub fn with_move_handle(mut self, handle: u32) -> Self {
        self.handles_to_move.push(handle);
        self
    }

    /// Attach data to write to the i-th B-type output buffer in guest memory.
    pub fn with_out_buf(mut self, data: Vec<u8>) -> Self {
        self.out_bufs.push(data);
        self
    }
}

// ── Parsing ──────────────────────────────────────────────────────────────────

/// Parse an IPC command from the 0x100-byte TLS message buffer.
///
/// Returns `Err` if the buffer is too short or the structure is malformed.
pub fn parse_ipc_command(tls_data: &[u8]) -> Result<IpcCommand, anyhow::Error> {
    if tls_data.len() < TLS_MESSAGE_SIZE {
        anyhow::bail!(
            "TLS buffer too small: {} < {}",
            tls_data.len(),
            TLS_MESSAGE_SIZE,
        );
    }

    let mut cur = Cursor::new(tls_data);

    // ── HIPC header: word 0 ──────────────────────────────────────────────
    let word0 = cur.read_u32::<LittleEndian>()?;
    let command_type = CommandType::from_u16((word0 & 0xFFFF) as u16);
    let num_x_bufs = (word0 >> 16) & 0xF;
    let num_a_bufs = (word0 >> 20) & 0xF;
    let num_b_bufs = (word0 >> 24) & 0xF;
    // bits 28..31: num_w_bufs (unused for now)

    // ── HIPC header: word 1 ──────────────────────────────────────────────
    let word1 = cur.read_u32::<LittleEndian>()?;
    let data_size = word1 & 0x3FF; // bits [9:0]
    // bits [13:10]: C descriptor flags (unused)
    let has_handle_descriptor = (word1 & (1 << 31)) != 0;

    // ── Handle descriptor (optional) ─────────────────────────────────────
    let mut handles_to_copy = Vec::new();
    let mut handles_to_move = Vec::new();
    let mut send_pid = false;

    if has_handle_descriptor {
        let handle_desc = cur.read_u32::<LittleEndian>()?;
        send_pid = (handle_desc & 1) != 0;
        let num_copy = ((handle_desc >> 1) & 0xF) as usize;
        let num_move = ((handle_desc >> 5) & 0xF) as usize;

        if send_pid {
            // Skip 8-byte PID.
            let _pid_lo = cur.read_u32::<LittleEndian>()?;
            let _pid_hi = cur.read_u32::<LittleEndian>()?;
        }

        for _ in 0..num_copy {
            handles_to_copy.push(cur.read_u32::<LittleEndian>()?);
        }
        for _ in 0..num_move {
            handles_to_move.push(cur.read_u32::<LittleEndian>()?);
        }
    }

    // ── Buffer descriptors (X, A, B) ─────────────────────────────────────
    // X descriptors: 2 words each (skip)
    for _ in 0..num_x_bufs {
        let _ = cur.read_u32::<LittleEndian>()?;
        let _ = cur.read_u32::<LittleEndian>()?;
    }
    // A descriptors: 3 words each (skip — send buffers, not used by HLE)
    for _ in 0..num_a_bufs {
        let _ = cur.read_u32::<LittleEndian>()?;
        let _ = cur.read_u32::<LittleEndian>()?;
        let _ = cur.read_u32::<LittleEndian>()?;
    }
    // B descriptors: 3 words each — extract the guest address so services
    // can write output data into the caller's buffer.
    // Format (from Ryujinx/libnx): word0=addr[31:0], word1=size[31:0],
    // word2 bits[31:28]=addr[35:32], bits[27:24]=size[35:32], bits[1:0]=flags.
    let mut b_buf_addrs: Vec<u64> = Vec::new();
    for _ in 0..num_b_bufs {
        let word0 = cur.read_u32::<LittleEndian>()?;
        let _word1 = cur.read_u32::<LittleEndian>()?;
        let word2 = cur.read_u32::<LittleEndian>()?;
        let addr = (word0 as u64) | (((word2 >> 28) & 0xF) as u64) << 32;
        b_buf_addrs.push(addr);
    }

    // ── Align to 16 bytes for CMIF payload ───────────────────────────────
    let pos = cur.position() as usize;
    let aligned_pos = (pos + 0xF) & !0xF;
    cur.set_position(aligned_pos as u64);

    // ── CMIF header ──────────────────────────────────────────────────────
    let cmif_magic = cur.read_u32::<LittleEndian>()?;
    let _version = cur.read_u32::<LittleEndian>()?; // CMIF version (usually 0 or 1)
    let command_id = cur.read_u32::<LittleEndian>()?;
    let _token = cur.read_u32::<LittleEndian>()?; // token / padding

    // ── Remaining raw data ───────────────────────────────────────────────
    let raw_start = cur.position() as usize;
    let raw_end = tls_data.len().min(raw_start + (data_size as usize).saturating_sub(4) * 4);
    let mut raw_data = Vec::new();
    let mut raw_cur = Cursor::new(&tls_data[raw_start..raw_end]);
    while raw_cur.position() < (raw_end - raw_start) as u64 {
        match raw_cur.read_u32::<LittleEndian>() {
            Ok(w) => raw_data.push(w),
            Err(_) => break,
        }
    }

    Ok(IpcCommand {
        command_type,
        data_size,
        num_x_bufs,
        num_a_bufs,
        num_b_bufs,
        has_handle_descriptor,
        handles_to_copy,
        handles_to_move,
        send_pid,
        cmif_magic,
        command_id,
        raw_data,
        b_buf_addrs,
    })
}

// ── Response building ────────────────────────────────────────────────────────

/// Build the raw byte representation of an IPC response to write into the TLS
/// message buffer.
///
/// Layout:
///   HIPC word 0-1 | optional handle descriptor | padding | SFCO header | data
pub fn build_ipc_response(result: ResultCode, data: &[u32]) -> Vec<u8> {
    build_ipc_response_full(result, data, &[], &[])
}

/// Build a full IPC response with optional handles.
pub fn build_ipc_response_full(
    result: ResultCode,
    data: &[u32],
    handles_to_copy: &[u32],
    handles_to_move: &[u32],
) -> Vec<u8> {
    let mut buf = vec![0u8; TLS_MESSAGE_SIZE];
    let mut cur = Cursor::new(&mut buf[..]);

    let has_handles = !handles_to_copy.is_empty() || !handles_to_move.is_empty();

    // Data payload: SFCO magic(1) + version(1) + result(1) + user data
    let cmif_words: u32 = 4 + data.len() as u32;

    // ── HIPC word 0 ──────────────────────────────────────────────────────
    let word0: u32 = CommandType::Request as u32; // response reuses Request type encoding
    let _ = cur.write_u32::<LittleEndian>(word0);

    // ── HIPC word 1 ──────────────────────────────────────────────────────
    let mut word1: u32 = cmif_words & 0x3FF;
    if has_handles {
        word1 |= 1 << 31;
    }
    let _ = cur.write_u32::<LittleEndian>(word1);

    // ── Handle descriptor (optional) ─────────────────────────────────────
    if has_handles {
        let handle_desc: u32 = ((handles_to_copy.len() as u32 & 0xF) << 1)
            | ((handles_to_move.len() as u32 & 0xF) << 5);
        let _ = cur.write_u32::<LittleEndian>(handle_desc);

        for &h in handles_to_copy {
            let _ = cur.write_u32::<LittleEndian>(h);
        }
        for &h in handles_to_move {
            let _ = cur.write_u32::<LittleEndian>(h);
        }
    }

    // ── Align to 16 bytes for CMIF ───────────────────────────────────────
    let pos = cur.position() as usize;
    let aligned_pos = (pos + 0xF) & !0xF;
    cur.set_position(aligned_pos as u64);

    // ── CMIF response header ─────────────────────────────────────────────
    let _ = cur.write_u32::<LittleEndian>(SFCO_MAGIC);
    let _ = cur.write_u32::<LittleEndian>(0); // version
    let _ = cur.write_u32::<LittleEndian>(result.raw());
    let _ = cur.write_u32::<LittleEndian>(0); // token / padding

    // ── User data ────────────────────────────────────────────────────────
    for &word in data {
        let _ = cur.write_u32::<LittleEndian>(word);
    }

    buf
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a minimal TLS buffer containing a CMIF Request with the given
    /// command id and optional raw data words.
    fn make_tls_request(command_id: u32, raw_data: &[u32]) -> Vec<u8> {
        let mut buf = vec![0u8; TLS_MESSAGE_SIZE];
        let mut cur = Cursor::new(&mut buf[..]);

        // HIPC word 0: type = Request (4), no buffer descriptors.
        let word0: u32 = CommandType::Request as u32;
        cur.write_u32::<LittleEndian>(word0).unwrap();

        // HIPC word 1: data_size = 4 (CMIF hdr) + raw words, no handle desc.
        let data_size = 4 + raw_data.len() as u32;
        cur.write_u32::<LittleEndian>(data_size & 0x3FF).unwrap();

        // Align to 16 bytes for CMIF.
        let pos = cur.position() as usize;
        let aligned = (pos + 0xF) & !0xF;
        cur.set_position(aligned as u64);

        // CMIF header.
        cur.write_u32::<LittleEndian>(SFCI_MAGIC).unwrap();
        cur.write_u32::<LittleEndian>(0).unwrap(); // version
        cur.write_u32::<LittleEndian>(command_id).unwrap();
        cur.write_u32::<LittleEndian>(0).unwrap(); // token

        for &w in raw_data {
            cur.write_u32::<LittleEndian>(w).unwrap();
        }

        buf
    }

    #[test]
    fn test_parse_simple_request() {
        let tls = make_tls_request(42, &[0xDEAD, 0xBEEF]);
        let cmd = parse_ipc_command(&tls).unwrap();
        assert_eq!(cmd.command_type, CommandType::Request);
        assert_eq!(cmd.cmif_magic, SFCI_MAGIC);
        assert_eq!(cmd.command_id, 42);
        assert!(!cmd.has_handle_descriptor);
    }

    #[test]
    fn test_build_response_roundtrip() {
        let data = [1u32, 2, 3];
        let response_bytes = build_ipc_response(ResultCode::SUCCESS, &data);
        assert_eq!(response_bytes.len(), TLS_MESSAGE_SIZE);

        // The SFCO magic should appear at the 16-byte-aligned position after
        // the two HIPC header words (offset 8, aligned to 16).
        let magic_offset = 0x10; // 16
        let mut cur = Cursor::new(&response_bytes[magic_offset..]);
        let magic = cur.read_u32::<LittleEndian>().unwrap();
        assert_eq!(magic, SFCO_MAGIC);

        // Result code at offset +8 from magic.
        let _version = cur.read_u32::<LittleEndian>().unwrap();
        let rc = cur.read_u32::<LittleEndian>().unwrap();
        assert_eq!(rc, 0); // SUCCESS
    }

    #[test]
    fn test_buffer_too_small() {
        let small = vec![0u8; 16];
        assert!(parse_ipc_command(&small).is_err());
    }
}
