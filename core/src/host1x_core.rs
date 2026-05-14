// SPDX-FileCopyrightText: 2026 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Opaque Host1x bridge used by `core` owners that must talk to the
//! frontend-provided Host1x implementation without depending on `video_core`.

use std::any::Any;

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
}
