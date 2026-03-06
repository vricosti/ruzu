// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use common::{error, Handle, ResultCode, VAddr};

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

    debug!(
        "ConnectToNamedPort: created session handle {} for \"{}\"",
        handle, port_name
    );
    Ok(handle)
}
