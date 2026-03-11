// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/shared_buffer_manager.cpp/.h
//! Status: Stubbed - types defined, full implementation depends on nvdrv/nvnflinger.

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SharedMemorySlot {
    pub buffer_offset: u64,
    pub size: u64,
    pub width: i32,
    pub height: i32,
}
const _: () = assert!(core::mem::size_of::<SharedMemorySlot>() == 0x18);

#[derive(Clone, Copy)]
#[repr(C)]
pub struct SharedMemoryPoolLayout {
    pub num_slots: i32,
    pub _padding: [u8; 4],
    pub slots: [SharedMemorySlot; 0x10],
}
const _: () = assert!(core::mem::size_of::<SharedMemoryPoolLayout>() == 0x188);
