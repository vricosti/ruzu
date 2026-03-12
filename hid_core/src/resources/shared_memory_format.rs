// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/shared_memory_format.h
//!
//! Defines the full shared memory layout for HID services.

pub const HID_ENTRY_COUNT: usize = 17;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct CommonHeader {
    pub timestamp: i64,
    pub total_entry_count: i64,
    pub last_entry_index: i64,
    pub entry_count: i64,
}
const _: () = assert!(std::mem::size_of::<CommonHeader>() == 0x20);

// TODO: Port all shared memory format structs:
// - DebugPadSharedMemoryFormat
// - TouchScreenSharedMemoryFormat
// - MouseSharedMemoryFormat
// - KeyboardSharedMemoryFormat
// - DigitizerSharedMemoryFormat
// - HomeButtonSharedMemoryFormat
// - SleepButtonSharedMemoryFormat
// - CaptureButtonSharedMemoryFormat
// - InputDetectorSharedMemoryFormat
// - UniquePadSharedMemoryFormat
// - NpadSharedMemoryFormat
// - GestureSharedMemoryFormat
// - ConsoleSixAxisSensorSharedMemoryFormat
// - SharedMemoryFormat (total 0x40000 bytes)
