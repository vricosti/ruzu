// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/exclusive_monitor.h and exclusive_monitor.cpp
//! ExclusiveMonitor for atomic operations.

/// Type alias for virtual addresses
pub type VAddr = u64;

/// Abstract exclusive monitor interface.
///
/// Corresponds to upstream `Core::ExclusiveMonitor`.
pub trait ExclusiveMonitor {
    fn exclusive_read8(&mut self, core_index: usize, addr: VAddr) -> u8;
    fn exclusive_read16(&mut self, core_index: usize, addr: VAddr) -> u16;
    fn exclusive_read32(&mut self, core_index: usize, addr: VAddr) -> u32;
    fn exclusive_read64(&mut self, core_index: usize, addr: VAddr) -> u64;
    fn exclusive_read128(&mut self, core_index: usize, addr: VAddr) -> u128;
    fn clear_exclusive(&mut self, core_index: usize);

    fn exclusive_write8(&mut self, core_index: usize, vaddr: VAddr, value: u8) -> bool;
    fn exclusive_write16(&mut self, core_index: usize, vaddr: VAddr, value: u16) -> bool;
    fn exclusive_write32(&mut self, core_index: usize, vaddr: VAddr, value: u32) -> bool;
    fn exclusive_write64(&mut self, core_index: usize, vaddr: VAddr, value: u64) -> bool;
    fn exclusive_write128(&mut self, core_index: usize, vaddr: VAddr, value: u128) -> bool;
}

/// Factory function for creating an exclusive monitor.
///
/// Corresponds to upstream `Core::MakeExclusiveMonitor`.
/// On supported architectures (x86_64, arm64), this would create a
/// DynarmicExclusiveMonitor. Currently returns None as a placeholder.
pub fn make_exclusive_monitor(
    _memory: &mut dyn std::any::Any, // TODO: Core::Memory::Memory
    _num_cores: usize,
) -> Option<Box<dyn ExclusiveMonitor>> {
    // TODO: Create DynarmicExclusiveMonitor when rdynarmic is integrated
    // #[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
    // { return Some(Box::new(DynarmicExclusiveMonitor::new(memory, num_cores))); }
    None
}
