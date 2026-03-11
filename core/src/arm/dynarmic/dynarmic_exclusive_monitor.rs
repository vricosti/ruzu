// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/dynarmic_exclusive_monitor.h and .cpp
//! Dynarmic exclusive monitor implementation wrapping Dynarmic::ExclusiveMonitor.

use crate::arm::exclusive_monitor::{ExclusiveMonitor, VAddr};

/// Dynarmic-backed exclusive monitor.
///
/// Corresponds to upstream `Core::DynarmicExclusiveMonitor`.
///
/// In the C++ version, this wraps `Dynarmic::ExclusiveMonitor` and delegates
/// to `Memory::Read*` / `Memory::WriteExclusive*` for the actual memory operations.
///
/// Until rdynarmic is integrated, this is a structural placeholder.
pub struct DynarmicExclusiveMonitor {
    // TODO: Replace with actual Dynarmic::ExclusiveMonitor when rdynarmic is integrated
    // monitor: dynarmic::ExclusiveMonitor,
    // memory: &mut Memory,
    _core_count: usize,
}

impl DynarmicExclusiveMonitor {
    /// Create a new DynarmicExclusiveMonitor.
    ///
    /// Corresponds to upstream constructor.
    pub fn new(
        _memory: &mut dyn std::any::Any, // TODO: Core::Memory::Memory
        core_count: usize,
    ) -> Self {
        Self {
            _core_count: core_count,
        }
    }
}

impl ExclusiveMonitor for DynarmicExclusiveMonitor {
    fn exclusive_read8(&mut self, _core_index: usize, _addr: VAddr) -> u8 {
        // monitor.ReadAndMark<u8>(core_index, addr, || memory.Read8(addr))
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_read16(&mut self, _core_index: usize, _addr: VAddr) -> u16 {
        // monitor.ReadAndMark<u16>(core_index, addr, || memory.Read16(addr))
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_read32(&mut self, _core_index: usize, _addr: VAddr) -> u32 {
        // monitor.ReadAndMark<u32>(core_index, addr, || memory.Read32(addr))
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_read64(&mut self, _core_index: usize, _addr: VAddr) -> u64 {
        // monitor.ReadAndMark<u64>(core_index, addr, || memory.Read64(addr))
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_read128(&mut self, _core_index: usize, _addr: VAddr) -> u128 {
        // monitor.ReadAndMark<u128>(core_index, addr, || {
        //     let lo = memory.Read64(addr);
        //     let hi = memory.Read64(addr + 8);
        //     (hi as u128) << 64 | lo as u128
        // })
        todo!("Requires rdynarmic integration")
    }

    fn clear_exclusive(&mut self, _core_index: usize) {
        // monitor.ClearProcessor(core_index)
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_write8(&mut self, _core_index: usize, _vaddr: VAddr, _value: u8) -> bool {
        // monitor.DoExclusiveOperation<u8>(core_index, vaddr, |expected| {
        //     memory.WriteExclusive8(vaddr, value, expected)
        // })
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_write16(&mut self, _core_index: usize, _vaddr: VAddr, _value: u16) -> bool {
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_write32(&mut self, _core_index: usize, _vaddr: VAddr, _value: u32) -> bool {
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_write64(&mut self, _core_index: usize, _vaddr: VAddr, _value: u64) -> bool {
        todo!("Requires rdynarmic integration")
    }

    fn exclusive_write128(&mut self, _core_index: usize, _vaddr: VAddr, _value: u128) -> bool {
        todo!("Requires rdynarmic integration")
    }
}
