// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Memory access trait for the CPU interpreter.
//!
//! This trait abstracts memory operations so the interpreter can read/write
//! guest memory without depending on the kernel crate directly.

use thiserror::Error;

/// Memory access error.
#[derive(Debug, Error)]
pub enum MemoryFault {
    #[error("unmapped address: 0x{0:016X}")]
    Unmapped(u64),
    #[error("permission denied at 0x{0:016X}")]
    PermissionDenied(u64),
    #[error("misaligned access at 0x{0:016X}")]
    Misaligned(u64),
}

/// Trait for guest memory access, used by the interpreter.
///
/// Implementations must handle address translation and permission checks.
/// All multi-byte reads/writes are little-endian (matching ARM64).
pub trait MemoryAccess {
    fn read_u8(&self, addr: u64) -> Result<u8, MemoryFault>;
    fn read_u16(&self, addr: u64) -> Result<u16, MemoryFault>;
    fn read_u32(&self, addr: u64) -> Result<u32, MemoryFault>;
    fn read_u64(&self, addr: u64) -> Result<u64, MemoryFault>;
    fn read_u128(&self, addr: u64) -> Result<u128, MemoryFault>;
    fn write_u8(&mut self, addr: u64, val: u8) -> Result<(), MemoryFault>;
    fn write_u16(&mut self, addr: u64, val: u16) -> Result<(), MemoryFault>;
    fn write_u32(&mut self, addr: u64, val: u32) -> Result<(), MemoryFault>;
    fn write_u64(&mut self, addr: u64, val: u64) -> Result<(), MemoryFault>;
    fn write_u128(&mut self, addr: u64, val: u128) -> Result<(), MemoryFault>;
}
