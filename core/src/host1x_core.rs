// SPDX-FileCopyrightText: 2026 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Opaque Host1x bridge used by `core` owners that must talk to the
//! frontend-provided Host1x implementation without depending on `video_core`.

use std::any::Any;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Host1xChannelType {
    MsEnc = 0,
    Vic = 1,
    Gpu = 2,
    NvDec = 3,
    Display = 4,
    NvJpg = 5,
    TSec = 6,
    Max = 7,
}

pub trait Host1xCoreInterface: Any + Send + Sync {
    fn as_any(&self) -> &(dyn Any + Send + Sync);
    fn get_host_syncpoint_value(&self, id: u32) -> u32;
    fn wait_host(&self, id: u32, expected_value: u32);
    fn register_host_action(
        &self,
        id: u32,
        expected_value: u32,
        action: Box<dyn FnOnce() + Send>,
    ) -> Option<u64>;
    fn deregister_host_action(&self, id: u32, handle: u64);

    /// Allocate `size` bytes of device-virtual (SMMU) address space and
    /// return the device address. Bump-allocator semantics — matches
    /// upstream `host1x.MemoryManager().Allocate(size)`.
    fn smmu_allocate(&self, size: usize) -> u64;

    /// Install an SMMU page-table mapping covering `[d_address, d_address
    /// + size)` and pointing at host backing `host_ptr` (as `usize` so
    /// raw pointers don't cross the trait boundary). The caller — typically
    /// `nvmap::pin_handle` — is responsible for ensuring `host_ptr`
    /// remains valid for the lifetime of the mapping.
    ///
    /// # Safety
    /// `host_ptr` must point to at least `size` bytes of accessible host
    /// memory that outlive this mapping.
    fn smmu_map(&self, d_address: u64, host_ptr: usize, size: usize);

    /// Look up the host pointer for a device address via the SMMU page
    /// table. Returns 0 if no mapping exists.
    fn smmu_lookup(&self, d_address: u64) -> usize;

    /// Bind the callback used after Host1x device-memory writes.
    ///
    /// Upstream `GPU::Impl::BindRenderer` calls
    /// `host1x.MemoryManager().BindInterface(rasterizer)`, and
    /// `DeviceMemoryManager::WriteBlock` then invalidates the written device
    /// range through that rasterizer. The trait keeps this dependency opaque
    /// so `core` does not depend on `video_core`.
    fn bind_device_memory_invalidator(&self, callback: Box<dyn Fn(u64, usize) + Send + Sync>);

    /// Allocate a low 32-bit Host1x GMMU address and map it to an existing
    /// SMMU/device address. Mirrors the `NvMap::PinHandle(low_area_pin)`
    /// path:
    /// `host1x.Allocator().Allocate(size); host1x.GMMU().Map(addr, daddr, size)`.
    fn host1x_gmmu_map_low(&self, d_address: u64, size: usize) -> u32;

    /// Undo `host1x_gmmu_map_low`, mirroring `NvMap::UnmapHandle`:
    /// `host1x.GMMU().Unmap(addr, size); host1x.Allocator().Free(addr, size)`.
    fn host1x_gmmu_unmap_low(&self, gpu_address: u32, size: usize);

    fn start_device(&self, fd: i32, channel_type: Host1xChannelType, syncpt: u32);
    fn stop_device(&self, fd: i32, channel_type: Host1xChannelType);
    fn push_entries(&self, fd: i32, entries: Vec<u32>);
}
