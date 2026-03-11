// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/tools/freezer.h and freezer.cpp
//! Memory freezer that prevents games from writing new values to certain addresses.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

/// Type alias for virtual addresses.
pub type VAddr = u64;

/// Callback interval for the memory freezer (60 Hz).
///
/// Corresponds to upstream `memory_freezer_ns`.
pub const MEMORY_FREEZER_NS: u64 = 1_000_000_000 / 60;

/// A single frozen memory entry.
///
/// Corresponds to upstream `Tools::Freezer::Entry`.
#[derive(Debug, Clone)]
pub struct Entry {
    pub address: VAddr,
    pub width: u32,
    pub value: u64,
}

/// Memory freezer that prevents an application from writing new values to certain
/// memory locations.
///
/// Corresponds to upstream `Tools::Freezer`.
pub struct Freezer {
    active: AtomicBool,
    entries: Mutex<Vec<Entry>>,
    // TODO: event: shared_ptr<Core::Timing::EventType>
    // TODO: core_timing: &CoreTiming
    // TODO: memory: &Memory
}

impl Freezer {
    /// Create a new Freezer.
    ///
    /// Corresponds to upstream `Freezer::Freezer(CoreTiming&, Memory&)`.
    pub fn new(
        // TODO: core_timing: &CoreTiming,
        // TODO: memory: &Memory,
    ) -> Self {
        // TODO: Create timing event and schedule at MEMORY_FREEZER_NS
        Self {
            active: AtomicBool::new(false),
            entries: Mutex::new(Vec::new()),
        }
    }

    /// Enables or disables the entire memory freezer.
    ///
    /// Corresponds to upstream `Freezer::SetActive`.
    pub fn set_active(&self, is_active: bool) {
        if !self.active.swap(is_active, Ordering::SeqCst) {
            // Was not previously active, now activating
            if is_active {
                self.fill_entry_reads();
                // TODO: core_timing.schedule_event(MEMORY_FREEZER_NS, event);
                log::debug!("Memory freezer activated!");
            }
        } else {
            log::debug!("Memory freezer deactivated!");
        }
    }

    /// Returns whether or not the freezer is active.
    ///
    /// Corresponds to upstream `Freezer::IsActive`.
    pub fn is_active(&self) -> bool {
        self.active.load(Ordering::Relaxed)
    }

    /// Removes all entries from the freezer.
    ///
    /// Corresponds to upstream `Freezer::Clear`.
    pub fn clear(&self) {
        let mut entries = self.entries.lock().unwrap();
        log::debug!("Clearing all frozen memory values.");
        entries.clear();
    }

    /// Freezes a value to its current memory address.
    /// Width can be 1, 2, 4, or 8 (in bytes).
    ///
    /// Corresponds to upstream `Freezer::Freeze`.
    pub fn freeze(&self, address: VAddr, width: u32) -> u64 {
        let mut entries = self.entries.lock().unwrap();

        // TODO: Read current value from memory
        // let current_value = memory_read_width(&self.memory, width, address);
        let current_value = 0u64;

        entries.push(Entry {
            address,
            width,
            value: current_value,
        });

        log::debug!(
            "Freezing memory for address={:016X}, width={:02X}, current_value={:016X}",
            address,
            width,
            current_value
        );

        current_value
    }

    /// Unfreezes the memory value at address.
    ///
    /// Corresponds to upstream `Freezer::Unfreeze`.
    pub fn unfreeze(&self, address: VAddr) {
        let mut entries = self.entries.lock().unwrap();
        log::debug!("Unfreezing memory for address={:016X}", address);
        entries.retain(|entry| entry.address != address);
    }

    /// Returns whether or not the address is frozen.
    ///
    /// Corresponds to upstream `Freezer::IsFrozen`.
    pub fn is_frozen(&self, address: VAddr) -> bool {
        let entries = self.entries.lock().unwrap();
        entries.iter().any(|entry| entry.address == address)
    }

    /// Sets the value that address should be frozen to.
    ///
    /// Corresponds to upstream `Freezer::SetFrozenValue`.
    pub fn set_frozen_value(&self, address: VAddr, value: u64) {
        let mut entries = self.entries.lock().unwrap();

        if let Some(entry) = entries.iter_mut().find(|e| e.address == address) {
            log::debug!(
                "Manually overridden freeze value for address={:016X}, width={:02X} to value={:016X}",
                entry.address,
                entry.width,
                value
            );
            entry.value = value;
        } else {
            log::error!(
                "Tried to set freeze value for address={:016X} that is not frozen!",
                address
            );
        }
    }

    /// Returns the entry corresponding to the address if frozen.
    ///
    /// Corresponds to upstream `Freezer::GetEntry`.
    pub fn get_entry(&self, address: VAddr) -> Option<Entry> {
        let entries = self.entries.lock().unwrap();
        entries.iter().find(|e| e.address == address).cloned()
    }

    /// Returns all entries in the freezer.
    ///
    /// Corresponds to upstream `Freezer::GetEntries`.
    pub fn get_entries(&self) -> Vec<Entry> {
        let entries = self.entries.lock().unwrap();
        entries.clone()
    }

    /// Frame callback: enforce frozen values by writing them back to memory.
    ///
    /// Corresponds to upstream `Freezer::FrameCallback`.
    fn frame_callback(&self, _ns_late: u64) {
        if !self.is_active() {
            log::debug!("Memory freezer has been deactivated, ending callback events.");
            return;
        }

        let entries = self.entries.lock().unwrap();
        for entry in entries.iter() {
            log::debug!(
                "Enforcing memory freeze at address={:016X}, value={:016X}, width={:02X}",
                entry.address,
                entry.value,
                entry.width
            );
            // TODO: memory_write_width(&self.memory, entry.width, entry.address, entry.value);
        }

        // TODO: core_timing.schedule_event(MEMORY_FREEZER_NS - ns_late, event);
    }

    /// Update all entry values from memory.
    ///
    /// Corresponds to upstream `Freezer::FillEntryReads`.
    fn fill_entry_reads(&self) {
        let mut entries = self.entries.lock().unwrap();
        log::debug!("Updating memory freeze entries to current values.");
        for _entry in entries.iter_mut() {
            // TODO: entry.value = memory_read_width(&self.memory, entry.width, entry.address);
        }
    }
}

impl Drop for Freezer {
    fn drop(&mut self) {
        // TODO: core_timing.unschedule_event(event);
    }
}

/// Read memory at a given address with the specified width.
///
/// Corresponds to upstream anonymous `MemoryReadWidth`.
#[allow(dead_code)]
fn memory_read_width(
    _memory: &dyn std::any::Any, // TODO: Core::Memory::Memory
    width: u32,
    _addr: VAddr,
) -> u64 {
    match width {
        1 => 0, // TODO: memory.read8(addr)
        2 => 0, // TODO: memory.read16(addr)
        4 => 0, // TODO: memory.read32(addr)
        8 => 0, // TODO: memory.read64(addr)
        _ => unreachable!("Invalid memory width: {}", width),
    }
}

/// Write memory at a given address with the specified width.
///
/// Corresponds to upstream anonymous `MemoryWriteWidth`.
#[allow(dead_code)]
fn memory_write_width(
    _memory: &dyn std::any::Any, // TODO: Core::Memory::Memory
    width: u32,
    _addr: VAddr,
    _value: u64,
) {
    match width {
        1 => {} // TODO: memory.write8(addr, value as u8)
        2 => {} // TODO: memory.write16(addr, value as u16)
        4 => {} // TODO: memory.write32(addr, value as u32)
        8 => {} // TODO: memory.write64(addr, value)
        _ => unreachable!("Invalid memory width: {}", width),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_freezer_basic() {
        let freezer = Freezer::new();
        assert!(!freezer.is_active());
        assert!(freezer.get_entries().is_empty());
    }

    #[test]
    fn test_freeze_unfreeze() {
        let freezer = Freezer::new();
        freezer.freeze(0x1000, 4);
        assert!(freezer.is_frozen(0x1000));
        assert!(!freezer.is_frozen(0x2000));

        freezer.unfreeze(0x1000);
        assert!(!freezer.is_frozen(0x1000));
    }

    #[test]
    fn test_set_frozen_value() {
        let freezer = Freezer::new();
        freezer.freeze(0x1000, 4);
        freezer.set_frozen_value(0x1000, 42);
        let entry = freezer.get_entry(0x1000).unwrap();
        assert_eq!(entry.value, 42);
    }

    #[test]
    fn test_clear() {
        let freezer = Freezer::new();
        freezer.freeze(0x1000, 4);
        freezer.freeze(0x2000, 8);
        assert_eq!(freezer.get_entries().len(), 2);
        freezer.clear();
        assert!(freezer.get_entries().is_empty());
    }
}
