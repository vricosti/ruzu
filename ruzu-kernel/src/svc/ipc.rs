// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::{debug, warn};
use ruzu_common::error;
use ruzu_common::{Handle, ResultCode, VAddr};

use crate::kernel::KernelCore;
use crate::objects::{KClientSession, KernelObject};
use ruzu_cpu::CpuState;

/// SVC 0x1F: ConnectToNamedPort
/// X1 = pointer to port name string in guest memory
/// Returns: X0 = result, X1 = session handle
pub fn svc_connect_to_named_port(
    kernel: &mut KernelCore,
    _cpu: &CpuState,
    name_addr: VAddr,
) -> Result<Handle, ResultCode> {
    let process = kernel.process_mut().ok_or(error::INVALID_STATE)?;

    // Read port name from guest memory (null-terminated, max 12 bytes)
    let mut name_bytes = Vec::new();
    for i in 0..12u64 {
        let byte = process
            .memory
            .read_u8(name_addr + i)
            .map_err(|_| error::INVALID_ADDRESS)?;
        if byte == 0 {
            break;
        }
        name_bytes.push(byte);
    }

    let port_name = String::from_utf8_lossy(&name_bytes).to_string();
    debug!("ConnectToNamedPort: \"{}\"", port_name);

    // Create a client session for this port
    let session = KClientSession::new(port_name.clone());
    let handle = process
        .handle_table
        .add(KernelObject::ClientSession(session))
        .map_err(|_| error::HANDLE_TABLE_FULL)?;

    debug!("ConnectToNamedPort: created session handle {} for \"{}\"", handle, port_name);
    Ok(handle)
}

/// SVC 0x21: SendSyncRequest
/// X0 = session handle
/// IPC message is in the current thread's TLS (first 0x100 bytes)
pub fn svc_send_sync_request(
    kernel: &mut KernelCore,
    _cpu: &mut CpuState,
    handle: Handle,
) -> ResultCode {
    debug!("SendSyncRequest: handle={}", handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    // Get the service name from the session handle
    let service_name = match process.handle_table.get(handle) {
        Ok(KernelObject::ClientSession(session)) => session.service_name.clone(),
        _ => {
            warn!("SendSyncRequest: invalid handle {}", handle);
            return error::INVALID_HANDLE;
        }
    };

    // Get TLS address from current thread
    let tls_addr = match process.current_thread() {
        Some(thread) => thread.tls_addr,
        None => return error::INVALID_STATE,
    };

    // Read the command header from TLS
    let cmd_word0 = match process.memory.read_u32(tls_addr) {
        Ok(v) => v,
        Err(_) => return error::INVALID_ADDRESS,
    };
    let cmd_word1 = match process.memory.read_u32(tls_addr + 4) {
        Ok(v) => v,
        Err(_) => return error::INVALID_ADDRESS,
    };

    let cmd_type = cmd_word0 & 0xFFFF;
    let data_size = cmd_word1 & 0x3FF;
    let has_handle_desc = (cmd_word1 >> 31) & 1;

    debug!(
        "  IPC: type={}, data_size={}, has_handle_desc={}, service=\"{}\"",
        cmd_type, data_size, has_handle_desc, service_name
    );

    // Calculate offset to CMIF header (skip HIPC header + handle descriptor)
    let mut offset = 8u64; // past CommandHeader

    if has_handle_desc != 0 {
        let handle_desc = match process.memory.read_u32(tls_addr + offset) {
            Ok(v) => v,
            Err(_) => return error::INVALID_ADDRESS,
        };
        let send_pid = handle_desc & 1;
        let num_copy = (handle_desc >> 1) & 0xF;
        let num_move = (handle_desc >> 5) & 0xF;
        offset += 4;
        if send_pid != 0 {
            offset += 8;
        }
        offset += (num_copy + num_move) as u64 * 4;
    }

    // Skip buffer descriptors (X, A, B)
    let num_x = (cmd_word0 >> 16) & 0xF;
    let num_a = (cmd_word0 >> 20) & 0xF;
    let num_b = (cmd_word0 >> 24) & 0xF;
    offset += num_x as u64 * 8;
    offset += num_a as u64 * 12;
    offset += num_b as u64 * 12;

    // Align to 16 bytes for CMIF data
    offset = (offset + 15) & !15;

    // Read CMIF header: magic (SFCI) + command_id
    let cmif_magic = match process.memory.read_u32(tls_addr + offset) {
        Ok(v) => v,
        Err(_) => {
            // If we can't read CMIF, just return success with empty response
            write_empty_response(process, tls_addr);
            return ResultCode::SUCCESS;
        }
    };

    let command_id = match process.memory.read_u32(tls_addr + offset + 8) {
        Ok(v) => v,
        Err(_) => 0,
    };

    debug!(
        "  CMIF: magic=0x{:08X}, cmd_id={}, service=\"{}\"",
        cmif_magic, command_id, service_name
    );

    // Dispatch to the appropriate service handler
    // For Phase 1, we just write a success response
    write_stub_response(process, tls_addr, &service_name, command_id);

    ResultCode::SUCCESS
}

/// SVC 0x16: CloseHandle
pub fn svc_close_handle(kernel: &mut KernelCore, handle: Handle) -> ResultCode {
    debug!("CloseHandle: handle={}", handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    match process.handle_table.close(handle) {
        Ok(_) => ResultCode::SUCCESS,
        Err(rc) => rc,
    }
}

/// Write an empty CMIF success response to TLS.
fn write_empty_response(process: &mut crate::process::KProcess, tls_addr: VAddr) {
    write_cmif_response(process, tls_addr, ResultCode::SUCCESS, &[]);
}

/// Write a stub success response for a service command.
fn write_stub_response(
    process: &mut crate::process::KProcess,
    tls_addr: VAddr,
    service_name: &str,
    command_id: u32,
) {
    // sm: GetService returns a session handle for the requested service
    if service_name == "sm:" && command_id == 1 {
        // GetService: read service name from request data, return a new handle
        // For Phase 1, we create a dummy session
        debug!("  sm:GetService dispatched");
    }

    write_cmif_response(process, tls_addr, ResultCode::SUCCESS, &[]);
}

/// Write a CMIF response to the thread's TLS.
fn write_cmif_response(
    process: &mut crate::process::KProcess,
    tls_addr: VAddr,
    result: ResultCode,
    extra_data: &[u32],
) {
    // HIPC response header
    let data_words = 4 + extra_data.len() as u32; // CMIF header (4 words) + extra
    let _ = process.memory.write_u32(tls_addr, 0); // type = 0 (response)
    let _ = process.memory.write_u32(tls_addr + 4, data_words); // data size

    // Align to 16 bytes
    let cmif_offset = 16u64;

    // CMIF response header
    let _ = process
        .memory
        .write_u32(tls_addr + cmif_offset, 0x4F434653); // "SFCO"
    let _ = process.memory.write_u32(tls_addr + cmif_offset + 4, 0); // version
    let _ = process
        .memory
        .write_u32(tls_addr + cmif_offset + 8, result.raw()); // result code
    let _ = process.memory.write_u32(tls_addr + cmif_offset + 12, 0); // token

    // Extra response data
    for (i, &word) in extra_data.iter().enumerate() {
        let _ = process
            .memory
            .write_u32(tls_addr + cmif_offset + 16 + (i as u64 * 4), word);
    }
}
