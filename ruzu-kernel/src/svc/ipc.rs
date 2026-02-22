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

    // 1. Read the service name from the session handle.
    let service_name = {
        let process = match kernel.process_mut() {
            Some(p) => p,
            None => return error::INVALID_STATE,
        };
        match process.handle_table.get(handle) {
            Ok(KernelObject::ClientSession(session)) => session.service_name.clone(),
            _ => {
                warn!("SendSyncRequest: invalid handle {}", handle);
                return error::INVALID_HANDLE;
            }
        }
    };

    // 2. Clone the IPC handler Arc (avoids borrow conflicts with kernel).
    let handler = match kernel.ipc_handler.clone() {
        Some(h) => h,
        None => {
            // No handler set — fall back to stub response.
            debug!("SendSyncRequest: no IPC handler, writing stub response");
            let process = kernel.process_mut().unwrap();
            let tls_addr = match process.current_thread() {
                Some(thread) => thread.tls_addr,
                None => return error::INVALID_STATE,
            };
            write_cmif_response(process, tls_addr, ResultCode::SUCCESS, &[]);
            return ResultCode::SUCCESS;
        }
    };

    // 3. Read 0x100 bytes from TLS into a local buffer.
    let tls_addr;
    let tls_data;
    {
        let process = match kernel.process_mut() {
            Some(p) => p,
            None => return error::INVALID_STATE,
        };
        tls_addr = match process.current_thread() {
            Some(thread) => thread.tls_addr,
            None => return error::INVALID_STATE,
        };
        let mut buf = vec![0u8; 0x100];
        for i in 0..0x100u64 {
            buf[i as usize] = process.memory.read_u8(tls_addr + i).unwrap_or(0);
        }
        tls_data = buf;
    }

    // 4. Call the handler.
    let result = handler.handle_ipc(&service_name, &tls_data);

    // 5. If create_session_for is Some, create a new KClientSession and record its handle.
    //    This handle will be placed into the response's move-handle descriptor.
    let mut extra_move_handles: Vec<u32> = result.move_handles.clone();
    if let Some(ref sub_service_name) = result.create_session_for {
        let process = kernel.process_mut().unwrap();
        let sub_session = KClientSession::new(sub_service_name.clone());
        match process.handle_table.add(KernelObject::ClientSession(sub_session)) {
            Ok(sub_handle) => {
                debug!(
                    "SendSyncRequest: created sub-session handle {} for \"{}\"",
                    sub_handle, sub_service_name
                );
                extra_move_handles.push(sub_handle);
            }
            Err(_) => {
                warn!("SendSyncRequest: failed to create sub-session for \"{}\"", sub_service_name);
            }
        }
    }

    // 6. Build final response bytes.
    //    If the handler returned response_bytes with handles, we need to patch in
    //    the handle descriptor for any copy/move handles.
    let copy_handles = &result.copy_handles;
    let has_handles = !copy_handles.is_empty() || !extra_move_handles.is_empty();

    let response_bytes = if has_handles {
        // Re-build the response with handle descriptor.
        // Parse the CMIF result code and data from the handler's response.
        let (result_code, data_words) = parse_cmif_from_response(&result.response_bytes);
        build_response_with_handles(
            result_code,
            &data_words,
            copy_handles,
            &extra_move_handles,
        )
    } else {
        result.response_bytes
    };

    // 7. Write response bytes back to TLS.
    let process = kernel.process_mut().unwrap();
    let write_len = response_bytes.len().min(0x100);
    for i in 0..write_len {
        let _ = process.memory.write_u8(tls_addr + i as u64, response_bytes[i]);
    }

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

/// Parse result code and data words from a 0x100-byte CMIF response.
fn parse_cmif_from_response(response: &[u8]) -> (u32, Vec<u32>) {
    if response.len() < 0x20 {
        return (0, Vec::new());
    }

    // HIPC header: 2 words
    let word1 = u32::from_le_bytes(response[4..8].try_into().unwrap_or([0; 4]));
    let has_handle_desc = (word1 >> 31) & 1;

    let mut offset = 8usize;
    if has_handle_desc != 0 {
        let handle_desc = u32::from_le_bytes(
            response[offset..offset + 4].try_into().unwrap_or([0; 4]),
        );
        let num_copy = (handle_desc >> 1) & 0xF;
        let num_move = (handle_desc >> 5) & 0xF;
        offset += 4;
        offset += (num_copy + num_move) as usize * 4;
    }

    // Align to 16 bytes
    offset = (offset + 15) & !15;

    // SFCO magic at offset, version at +4, result at +8, token at +12
    if offset + 16 > response.len() {
        return (0, Vec::new());
    }
    let result_code = u32::from_le_bytes(
        response[offset + 8..offset + 12].try_into().unwrap_or([0; 4]),
    );

    // Read data words after CMIF header (16 bytes)
    let data_start = offset + 16;
    let mut data_words = Vec::new();
    let mut pos = data_start;
    while pos + 4 <= response.len() {
        let w = u32::from_le_bytes(response[pos..pos + 4].try_into().unwrap_or([0; 4]));
        data_words.push(w);
        pos += 4;
    }

    // Trim trailing zeros
    while data_words.last() == Some(&0) {
        data_words.pop();
    }

    (result_code, data_words)
}

/// Build a 0x100-byte IPC response with handle descriptor.
fn build_response_with_handles(
    result_code: u32,
    data: &[u32],
    handles_to_copy: &[u32],
    handles_to_move: &[u32],
) -> Vec<u8> {
    let mut buf = vec![0u8; 0x100];

    let has_handles = !handles_to_copy.is_empty() || !handles_to_move.is_empty();
    let cmif_words: u32 = 4 + data.len() as u32;

    // HIPC word 0: type = Response (0)
    buf[0..4].copy_from_slice(&0u32.to_le_bytes());

    // HIPC word 1: data_size + handle descriptor flag
    let mut word1: u32 = cmif_words & 0x3FF;
    if has_handles {
        word1 |= 1 << 31;
    }
    buf[4..8].copy_from_slice(&word1.to_le_bytes());

    let mut offset = 8usize;

    // Handle descriptor
    if has_handles {
        let handle_desc: u32 = ((handles_to_copy.len() as u32 & 0xF) << 1)
            | ((handles_to_move.len() as u32 & 0xF) << 5);
        buf[offset..offset + 4].copy_from_slice(&handle_desc.to_le_bytes());
        offset += 4;

        for &h in handles_to_copy {
            buf[offset..offset + 4].copy_from_slice(&h.to_le_bytes());
            offset += 4;
        }
        for &h in handles_to_move {
            buf[offset..offset + 4].copy_from_slice(&h.to_le_bytes());
            offset += 4;
        }
    }

    // Align to 16 bytes
    offset = (offset + 15) & !15;

    // CMIF response header
    if offset + 16 <= buf.len() {
        buf[offset..offset + 4].copy_from_slice(&0x4F434653u32.to_le_bytes()); // SFCO
        buf[offset + 4..offset + 8].copy_from_slice(&0u32.to_le_bytes()); // version
        buf[offset + 8..offset + 12].copy_from_slice(&result_code.to_le_bytes());
        buf[offset + 12..offset + 16].copy_from_slice(&0u32.to_le_bytes()); // token
    }

    // Data words
    let data_start = offset + 16;
    for (i, &word) in data.iter().enumerate() {
        let pos = data_start + i * 4;
        if pos + 4 <= buf.len() {
            buf[pos..pos + 4].copy_from_slice(&word.to_le_bytes());
        }
    }

    buf
}

/// Write a CMIF response directly to the thread's TLS (fallback when no handler).
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
