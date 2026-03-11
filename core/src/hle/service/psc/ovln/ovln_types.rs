// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/ovln/ovln_types.h

pub type OverlayNotification = [u64; 0x10];
const _: () = assert!(core::mem::size_of::<OverlayNotification>() == 0x80);

/// MessageFlags - union with bit fields in upstream
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct MessageFlags {
    pub raw: u64,
}
const _: () = assert!(core::mem::size_of::<MessageFlags>() == 0x8);

impl MessageFlags {
    pub fn message_type(&self) -> u8 {
        (self.raw & 0xFF) as u8
    }

    pub fn queue_type(&self) -> u8 {
        ((self.raw >> 8) & 0xFF) as u8
    }
}
