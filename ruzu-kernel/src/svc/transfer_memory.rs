// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::{error, Handle, ResultCode, VAddr};
use log::debug;

use crate::kernel::KernelCore;
use crate::objects::{KTransferMemory, KernelObject};

/// SVC 0x15: CreateTransferMemory
/// X1 = addr, X2 = size, X3 = permission
/// Returns: X0 = result, X1 = handle
pub fn svc_create_transfer_memory(
    kernel: &mut KernelCore,
    addr: VAddr,
    size: u64,
    perm: u32,
) -> Result<Handle, ResultCode> {
    debug!(
        "CreateTransferMemory: addr=0x{:X}, size=0x{:X}, perm={}",
        addr, size, perm
    );

    let process = kernel.process_mut().ok_or(error::INVALID_STATE)?;
    let tmem = KTransferMemory {
        addr,
        size: size as usize,
        permission: perm,
    };
    let handle = process
        .handle_table
        .add(KernelObject::TransferMemory(tmem))
        .map_err(|_| error::HANDLE_TABLE_FULL)?;
    Ok(handle)
}
