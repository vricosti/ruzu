//! Port of zuyu/src/core/hle/kernel/board/nintendo/nx/secure_monitor.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! Secure monitor (SMC) constants: memory size and arrangement enums
//! used during kernel initialization to determine DRAM layout.

/// Memory size identifiers returned by the secure monitor.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemorySize {
    MemorySize4GB = 0,
    MemorySize6GB = 1,
    MemorySize8GB = 2,
}

/// Memory arrangement identifiers returned by the secure monitor.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryArrangement {
    MemoryArrangement4GB = 0,
    MemoryArrangement4GBForAppletDev = 1,
    MemoryArrangement4GBForSystemDev = 2,
    MemoryArrangement6GB = 3,
    MemoryArrangement6GBForAppletDev = 4,
    MemoryArrangement8GB = 5,
}
