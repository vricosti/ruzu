// SPDX-FileCopyrightText: 2026 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Opaque GPU-core bridge used by `core` owners that must talk to the
//! frontend-provided GPU implementation without depending on `video_core`.

use std::any::Any;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct GpuCommandListHeader {
    pub raw: u64,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct GpuCommandHeader {
    pub raw: u32,
}

#[derive(Debug, Clone, Default)]
pub struct GpuCommandList {
    pub command_lists: Vec<GpuCommandListHeader>,
    pub prefetch_command_list: Vec<GpuCommandHeader>,
}

/// Opaque GPU memory-manager state handle.
pub trait GpuMemoryManagerHandle: Any + Send + Sync {
    fn as_any(&self) -> &(dyn Any + Send + Sync);

    /// Mirrors upstream `Tegra::MemoryManager::Map(...)`.
    fn map(&self, gpu_addr: u64, device_addr: u64, size: u64, kind: u32, is_big_pages: bool);

    /// Mirrors upstream `Tegra::MemoryManager::MapSparse(...)`.
    fn map_sparse(&self, gpu_addr: u64, size: u64, is_big_pages: bool);

    /// Mirrors upstream `Tegra::MemoryManager::Unmap(...)`.
    fn unmap(&self, gpu_addr: u64, size: u64);
}

/// Opaque per-channel GPU state handle.
///
/// This bridges the upstream ownership path
/// `system.GPU().AllocateChannel() -> ChannelState` into the Rust split-crate
/// layout where `core` cannot name `video_core` types directly.
pub trait GpuChannelHandle: Send + Sync {
    /// Mirrors the upstream `channel_state->memory_manager = gmmu` write.
    fn bind_memory_manager(&self, memory_manager: Arc<dyn GpuMemoryManagerHandle>);

    /// Mirrors the upstream `system.GPU().InitChannel(*channel_state, program_id)`.
    fn init_channel(&self, program_id: u64);

    /// Mirrors the upstream `channel_state->bind_id`.
    fn bind_id(&self) -> i32;
}

/// Opaque GPU implementation owned by [`crate::core::System`].
pub trait GpuCoreInterface: Any + Send {
    fn as_any(&self) -> &(dyn Any + Send);

    /// Mirrors the upstream `GPU::AllocateChannel()`.
    fn allocate_channel_handle(&self) -> Arc<dyn GpuChannelHandle>;

    /// Mirrors the upstream `std::make_shared<Tegra::MemoryManager>(...)` owner
    /// created by `nvhost_as_gpu`.
    fn allocate_memory_manager_handle(
        &self,
        address_space_bits: u64,
        split_address: u64,
        big_page_bits: u64,
        page_bits: u64,
    ) -> Arc<dyn GpuMemoryManagerHandle>;

    /// Mirrors the upstream `GPU::InitAddressSpace(Tegra::MemoryManager&)`.
    fn init_address_space(&self, memory_manager: Arc<dyn GpuMemoryManagerHandle>);

    /// Mirrors the upstream `GPU::PushGPUEntries(s32, CommandList&&)`.
    fn push_gpu_entries(&self, channel_id: i32, entries: GpuCommandList);

    /// Mirrors the upstream `GPU::OnCPUWrite(DAddr, u64)`.
    fn on_cpu_write(&self, addr: u64, size: u64) -> bool;
}
