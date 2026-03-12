// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/gpu_device_memory_manager.h` and
//! `gpu_device_memory_manager.cpp`.
//!
//! Defines the Maxwell device memory traits and the device memory manager type
//! alias. In C++ this instantiates `Core::DeviceMemoryManager<MaxwellDeviceTraits>`;
//! in Rust we define the trait constants and a placeholder manager struct until
//! the core device memory manager crate is fully ported.

/// Number of virtual address bits for the Maxwell device address space.
///
/// Port of `MaxwellDeviceTraits::device_virtual_bits`.
pub const DEVICE_VIRTUAL_BITS: usize = 34;

/// Device address type (matches upstream `DAddr`).
pub type DAddr = u64;

/// Placeholder for `Core::DeviceMemoryManager<MaxwellDeviceTraits>`.
///
/// The full implementation lives in the `core` crate; this struct provides the
/// interface surface needed by `host1x` code.
pub struct MaxwellDeviceMemoryManager {
    // TODO: Back with the real DeviceMemoryManager once core is wired up.
}

impl MaxwellDeviceMemoryManager {
    /// Read a value of type T from a device address.
    pub fn read_u8(&self, _addr: DAddr) -> u8 {
        todo!("MaxwellDeviceMemoryManager::read_u8")
    }

    pub fn read_u16(&self, _addr: DAddr) -> u16 {
        todo!("MaxwellDeviceMemoryManager::read_u16")
    }

    pub fn read_u32(&self, _addr: DAddr) -> u32 {
        todo!("MaxwellDeviceMemoryManager::read_u32")
    }

    pub fn read_u64(&self, _addr: DAddr) -> u64 {
        todo!("MaxwellDeviceMemoryManager::read_u64")
    }

    /// Write a value of type T to a device address.
    pub fn write_u8(&self, _addr: DAddr, _data: u8) {
        todo!("MaxwellDeviceMemoryManager::write_u8")
    }

    pub fn write_u16(&self, _addr: DAddr, _data: u16) {
        todo!("MaxwellDeviceMemoryManager::write_u16")
    }

    pub fn write_u32(&self, _addr: DAddr, _data: u32) {
        todo!("MaxwellDeviceMemoryManager::write_u32")
    }

    pub fn write_u64(&self, _addr: DAddr, _data: u64) {
        todo!("MaxwellDeviceMemoryManager::write_u64")
    }

    /// Get a pointer to the given device address.
    pub fn get_pointer(&self, _addr: DAddr) -> *const u8 {
        todo!("MaxwellDeviceMemoryManager::get_pointer")
    }

    /// Get a mutable pointer to the given device address.
    pub fn get_pointer_mut(&self, _addr: DAddr) -> *mut u8 {
        todo!("MaxwellDeviceMemoryManager::get_pointer_mut")
    }
}

/// Port of `Tegra::MaxwellDeviceMethods`.
///
/// Provides the `mark_region_caching` callback used by the device memory manager.
pub struct MaxwellDeviceMethods;

impl MaxwellDeviceMethods {
    pub fn mark_region_caching(_address: u64, _size: usize, _caching: bool) {
        todo!("MaxwellDeviceMethods::mark_region_caching")
    }
}
