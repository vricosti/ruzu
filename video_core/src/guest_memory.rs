// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/guest_memory.h
//!
//! Type aliases for guest memory access patterns used by the GPU.
//! The upstream C++ version provides template aliases over Core::Memory::GuestMemory
//! specialized for different memory managers (device vs GPU virtual).

/// Flags controlling guest memory access safety.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GuestMemoryFlags {
    /// Safe read: validates memory before access.
    SafeRead,
    /// Unsafe read: skips validation for performance.
    UnsafeRead,
}

/// Placeholder for device guest memory access.
///
/// In the full port, this wraps MaxwellDeviceMemoryManager with the given flags.
/// The actual implementation depends on the memory manager types from the core crate.
pub struct DeviceGuestMemory<T> {
    _marker: std::marker::PhantomData<T>,
}

/// Placeholder for GPU guest memory access.
///
/// In the full port, this wraps Tegra::MemoryManager with the given flags.
pub struct GpuGuestMemory<T> {
    _marker: std::marker::PhantomData<T>,
}

/// Scoped device guest memory access.
pub struct DeviceGuestMemoryScoped<T> {
    _marker: std::marker::PhantomData<T>,
}

/// Scoped GPU guest memory access.
pub struct GpuGuestMemoryScoped<T> {
    _marker: std::marker::PhantomData<T>,
}
