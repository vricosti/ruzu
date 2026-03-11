// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/shared_memory.h/.cpp

use super::common::{ContinuousAdjustmentTimePoint, SteadyClockTimePoint, SystemClockContext};

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct LockFreeAtomicSteadyClockTimePoint {
    pub counter: u32,
    pub _padding: u32,
    pub value: [SteadyClockTimePoint; 2],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct LockFreeAtomicSystemClockContext {
    pub counter: u32,
    pub _padding: u32,
    pub value: [SystemClockContext; 2],
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SharedMemoryStruct {
    pub steady_time_points: LockFreeAtomicSteadyClockTimePoint,
    pub local_system_clock_contexts: LockFreeAtomicSystemClockContext,
    pub network_system_clock_contexts: LockFreeAtomicSystemClockContext,
    pub automatic_corrections_counter: u32,
    pub _pad_ac: u32,
    pub automatic_corrections: [bool; 2],
    pub _pad_ac2: [u8; 6],
    pub continuous_adjustment_counter: u32,
    pub _pad_ca: u32,
    pub continuous_adjustment_time_points: [ContinuousAdjustmentTimePoint; 2],
    pub _pad0150: [u8; 0xEB0],
}
const _: () = assert!(core::mem::size_of::<SharedMemoryStruct>() == 0x1000);
