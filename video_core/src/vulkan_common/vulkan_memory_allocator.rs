// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `zuyu/src/video_core/vulkan_common/vulkan_memory_allocator.h` and
//! `zuyu/src/video_core/vulkan_common/vulkan_memory_allocator.cpp`.
//!
//! Memory allocation subsystem for Vulkan.
//! The C++ code uses a custom sub-allocator layered on top of VMA (Vulkan Memory Allocator).
//! In Rust, we use `gpu-allocator` or implement a compatible interface.
//!
//! The allocator has two allocation paths:
//! 1. VMA-based allocation for images and buffers (via `CreateImage`/`CreateBuffer`).
//! 2. Manual sub-allocation for raw memory commits (via `Commit`).

use ash::vk;

use super::vulkan_wrapper::VulkanError;

// ---------------------------------------------------------------------------
// MemoryUsage — port of `Vulkan::MemoryUsage`
// ---------------------------------------------------------------------------

/// Hints and requirements for the backing memory type of a commit.
///
/// Port of `Vulkan::MemoryUsage` from `vulkan_memory_allocator.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryUsage {
    /// Requests device local host visible buffer, falling back to device local memory.
    DeviceLocal,
    /// Requires a host visible memory type optimized for CPU to GPU uploads.
    Upload,
    /// Requires a host visible memory type optimized for GPU to CPU readbacks.
    Download,
    /// Requests device local host visible buffer, falling back to host memory.
    Stream,
}

// ---------------------------------------------------------------------------
// Helper: memory property flags for a given usage
// ---------------------------------------------------------------------------

/// Returns the `VkMemoryPropertyFlags` for a given `MemoryUsage`.
///
/// Port of `MemoryUsagePropertyFlags` from `vulkan_memory_allocator.cpp`.
fn memory_usage_property_flags(usage: MemoryUsage) -> vk::MemoryPropertyFlags {
    match usage {
        MemoryUsage::DeviceLocal => vk::MemoryPropertyFlags::DEVICE_LOCAL,
        MemoryUsage::Upload => {
            vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT
        }
        MemoryUsage::Download => {
            vk::MemoryPropertyFlags::HOST_VISIBLE
                | vk::MemoryPropertyFlags::HOST_COHERENT
                | vk::MemoryPropertyFlags::HOST_CACHED
        }
        MemoryUsage::Stream => {
            vk::MemoryPropertyFlags::DEVICE_LOCAL
                | vk::MemoryPropertyFlags::HOST_VISIBLE
                | vk::MemoryPropertyFlags::HOST_COHERENT
        }
    }
}

// ---------------------------------------------------------------------------
// Helper: allocation chunk sizes
// ---------------------------------------------------------------------------

/// Allocation chunk sizes used by the sub-allocator.
///
/// Port of `AllocationChunkSize` from `vulkan_memory_allocator.cpp`.
const ALLOCATION_CHUNK_SIZES: &[u64] = &[
    0x1000 << 10,
    0x1400 << 10,
    0x1800 << 10,
    0x1c00 << 10,
    0x2000 << 10,
    0x3200 << 10,
    0x4000 << 10,
    0x6000 << 10,
    0x8000 << 10,
    0xA000 << 10,
    0x10000 << 10,
    0x18000 << 10,
    0x20000 << 10,
];

/// Returns the allocation chunk size for a required size.
///
/// Port of `AllocationChunkSize` from `vulkan_memory_allocator.cpp`.
fn allocation_chunk_size(required_size: u64) -> u64 {
    match ALLOCATION_CHUNK_SIZES
        .iter()
        .find(|&&s| s >= required_size)
    {
        Some(&size) => size,
        None => {
            // Align up to 4 MiB
            let align = 4u64 << 20;
            (required_size + align - 1) & !(align - 1)
        }
    }
}

// ---------------------------------------------------------------------------
// Range — port of anonymous `Range` struct
// ---------------------------------------------------------------------------

/// A range within an allocation.
///
/// Port of the anonymous `Range` struct from `vulkan_memory_allocator.cpp`.
#[derive(Debug, Clone, Copy)]
struct Range {
    begin: u64,
    end: u64,
}

impl Range {
    fn contains(&self, iterator: u64, size: u64) -> bool {
        iterator < self.end && self.begin < iterator + size
    }
}

// ---------------------------------------------------------------------------
// MemoryCommit — port of `Vulkan::MemoryCommit`
// ---------------------------------------------------------------------------

/// Ownership handle of a memory commitment.
/// Points to a subregion of a memory allocation.
///
/// Port of `Vulkan::MemoryCommit` from `vulkan_memory_allocator.h`.
pub struct MemoryCommit {
    /// Index into the allocator's allocations list.
    allocation_index: Option<usize>,
    /// Vulkan device memory handle.
    memory: vk::DeviceMemory,
    /// Beginning offset in bytes to where the commit exists.
    begin: u64,
    /// Offset in bytes where the commit ends.
    end: u64,
    /// Host visible memory mapping. Empty if not queried before.
    mapped_ptr: Option<*mut u8>,
}

// SAFETY: The memory handle is owned by the Vulkan device and the commit
// is only accessed through the allocator which synchronizes access.
unsafe impl Send for MemoryCommit {}
unsafe impl Sync for MemoryCommit {}

impl MemoryCommit {
    /// Creates an empty commit.
    pub fn new() -> Self {
        Self {
            allocation_index: None,
            memory: vk::DeviceMemory::null(),
            begin: 0,
            end: 0,
            mapped_ptr: None,
        }
    }

    /// Creates a commit from the given parameters.
    fn from_parts(
        allocation_index: usize,
        memory: vk::DeviceMemory,
        begin: u64,
        end: u64,
    ) -> Self {
        Self {
            allocation_index: Some(allocation_index),
            memory,
            begin,
            end,
            mapped_ptr: None,
        }
    }

    /// Returns the Vulkan memory handle.
    ///
    /// Port of `MemoryCommit::Memory()`.
    pub fn memory(&self) -> vk::DeviceMemory {
        self.memory
    }

    /// Returns the start position of the commit relative to the allocation.
    ///
    /// Port of `MemoryCommit::Offset()`.
    pub fn offset(&self) -> vk::DeviceSize {
        self.begin
    }

    /// Returns the size of this commit.
    pub fn size(&self) -> u64 {
        self.end - self.begin
    }
}

impl Default for MemoryCommit {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// MemoryAllocation — port of `Vulkan::MemoryAllocation` (internal class)
// ---------------------------------------------------------------------------

/// A large memory allocation from which smaller commits are sub-allocated.
///
/// Port of `Vulkan::MemoryAllocation` from `vulkan_memory_allocator.cpp`.
struct MemoryAllocation {
    /// Vulkan device memory handle.
    memory: vk::DeviceMemory,
    /// Total size of this allocation.
    allocation_size: u64,
    /// Vulkan memory property flags.
    property_flags: vk::MemoryPropertyFlags,
    /// Shifted Vulkan memory type (1 << type_index).
    shifted_memory_type: u32,
    /// All commit ranges done from this allocation.
    commits: Vec<Range>,
    /// Memory mapped pointer. None if not yet mapped.
    mapped_ptr: Option<*mut u8>,
}

// SAFETY: Raw pointers are from vkMapMemory and are valid for the allocation lifetime.
unsafe impl Send for MemoryAllocation {}
unsafe impl Sync for MemoryAllocation {}

impl MemoryAllocation {
    fn new(
        memory: vk::DeviceMemory,
        allocation_size: u64,
        property_flags: vk::MemoryPropertyFlags,
        memory_type: u32,
    ) -> Self {
        Self {
            memory,
            allocation_size,
            property_flags,
            shifted_memory_type: 1u32 << memory_type,
            commits: Vec::new(),
            mapped_ptr: None,
        }
    }

    /// Tries to commit a region of the given size and alignment.
    ///
    /// Port of `MemoryAllocation::Commit`.
    fn commit(&mut self, size: u64, alignment: u64) -> Option<(u64, u64)> {
        let alloc = self.find_free_region(size, alignment)?;
        let range = Range {
            begin: alloc,
            end: alloc + size,
        };
        // Insert sorted by begin
        let pos = self
            .commits
            .partition_point(|r| r.begin <= alloc);
        self.commits.insert(pos, range);
        Some((alloc, alloc + size))
    }

    /// Frees a previously committed region.
    ///
    /// Port of `MemoryAllocation::Free`.
    fn free(&mut self, begin: u64) -> bool {
        if let Some(pos) = self.commits.iter().position(|r| r.begin == begin) {
            self.commits.remove(pos);
            true
        } else {
            false
        }
    }

    /// Returns true if this allocation is compatible with the given flags and type mask.
    ///
    /// Port of `MemoryAllocation::IsCompatible`.
    fn is_compatible(&self, flags: vk::MemoryPropertyFlags, type_mask: u32) -> bool {
        (self.property_flags & flags) == flags && (type_mask & self.shifted_memory_type) != 0
    }

    /// Returns true if there are no active commits in this allocation.
    fn is_empty(&self) -> bool {
        self.commits.is_empty()
    }

    /// Finds a free region of the given size with the given alignment.
    ///
    /// Port of `MemoryAllocation::FindFreeRegion`.
    fn find_free_region(&self, size: u64, alignment: u64) -> Option<u64> {
        debug_assert!(alignment.is_power_of_two());
        let alignment_log2 = alignment.trailing_zeros();
        let mut candidate: Option<u64> = None;
        let mut iterator = 0u64;
        let mut commit_iter = self.commits.iter();

        while iterator + size <= self.allocation_size {
            candidate = candidate.or(Some(iterator));
            match commit_iter.next() {
                None => break,
                Some(commit) => {
                    if let Some(c) = candidate {
                        if commit.contains(c, size) {
                            candidate = None;
                        }
                    }
                    // Align up
                    iterator = (commit.end + (1 << alignment_log2) - 1) >> alignment_log2
                        << alignment_log2;
                }
            }
        }
        candidate
    }
}

// ---------------------------------------------------------------------------
// MemoryAllocator — port of `Vulkan::MemoryAllocator`
// ---------------------------------------------------------------------------

/// Memory allocator container.
///
/// Port of `Vulkan::MemoryAllocator` from `vulkan_memory_allocator.h`.
///
/// This allocator manages Vulkan device memory allocations and sub-allocates
/// smaller regions (commits) from them. It also provides VMA-based image and
/// buffer creation (currently stubbed with `todo!()`).
pub struct MemoryAllocator {
    /// The Vulkan device.
    device: ash::Device,
    /// Physical device memory properties.
    properties: vk::PhysicalDeviceMemoryProperties,
    /// Buffer-image granularity from device limits.
    buffer_image_granularity: vk::DeviceSize,
    /// Current allocations.
    allocations: Vec<MemoryAllocation>,
    /// Valid memory types bitmask (may exclude small device-local heaps for debugging).
    valid_memory_types: u32,
}

impl MemoryAllocator {
    /// Constructs a memory allocator.
    ///
    /// Port of `MemoryAllocator::MemoryAllocator`.
    ///
    /// # Parameters
    /// - `device`: The logical Vulkan device.
    /// - `memory_properties`: Physical device memory properties.
    /// - `buffer_image_granularity`: From `VkPhysicalDeviceLimits::bufferImageGranularity`.
    /// - `has_debugging_tool`: Whether a debugging tool (e.g., RenderDoc) is attached.
    pub fn new(
        device: ash::Device,
        memory_properties: vk::PhysicalDeviceMemoryProperties,
        buffer_image_granularity: vk::DeviceSize,
        has_debugging_tool: bool,
    ) -> Self {
        let mut valid_memory_types = !0u32;

        // Port of the RenderDoc heap size check from the C++ constructor.
        // GPUs not supporting rebar may only have a small host visible/device local region.
        // With RenderDoc attached and only a small region, restrict which types are valid.
        if has_debugging_tool {
            const SMALL_HEAP_THRESHOLD: u64 = 256 * 1024 * 1024; // 256 MiB
            for i in 0..memory_properties.memory_type_count as usize {
                let mem_type = memory_properties.memory_types[i];
                let flags = mem_type.property_flags;
                if flags.contains(vk::MemoryPropertyFlags::DEVICE_LOCAL)
                    && flags.contains(vk::MemoryPropertyFlags::HOST_VISIBLE)
                {
                    let heap = memory_properties.memory_heaps[mem_type.heap_index as usize];
                    if heap.size <= SMALL_HEAP_THRESHOLD {
                        valid_memory_types &= !(1u32 << i);
                    }
                }
            }
        }

        Self {
            device,
            properties: memory_properties,
            buffer_image_granularity,
            allocations: Vec::new(),
            valid_memory_types,
        }
    }

    /// Creates a VMA-allocated image.
    ///
    /// Port of `MemoryAllocator::CreateImage`.
    ///
    /// NOTE: The C++ implementation calls `vmaCreateImage` via the VMA library.
    /// VMA is not yet integrated in the Rust port. Returns
    /// `ERROR_FEATURE_NOT_PRESENT` until gpu-allocator or raw VkMemory
    /// sub-allocation is wired up.
    pub fn create_image(&self, _ci: &vk::ImageCreateInfo) -> Result<vk::Image, VulkanError> {
        log::warn!("MemoryAllocator::create_image: VMA not integrated, returning error");
        Err(VulkanError::new(vk::Result::ERROR_FEATURE_NOT_PRESENT))
    }

    /// Creates a VMA-allocated buffer.
    ///
    /// Port of `MemoryAllocator::CreateBuffer`.
    ///
    /// NOTE: The C++ implementation calls `vmaCreateBuffer` via the VMA library.
    /// VMA is not yet integrated in the Rust port. Returns
    /// `ERROR_FEATURE_NOT_PRESENT` until gpu-allocator or raw VkMemory
    /// sub-allocation is wired up.
    pub fn create_buffer(
        &self,
        _ci: &vk::BufferCreateInfo,
        _usage: MemoryUsage,
    ) -> Result<vk::Buffer, VulkanError> {
        log::warn!("MemoryAllocator::create_buffer: VMA not integrated, returning error");
        Err(VulkanError::new(vk::Result::ERROR_FEATURE_NOT_PRESENT))
    }

    /// Commits a memory region with the specified requirements.
    ///
    /// Port of `MemoryAllocator::Commit(VkMemoryRequirements, MemoryUsage)`.
    pub fn commit(
        &mut self,
        requirements: &vk::MemoryRequirements,
        usage: MemoryUsage,
    ) -> Result<MemoryCommit, VulkanError> {
        let type_mask = requirements.memory_type_bits;
        let usage_flags = memory_usage_property_flags(usage);
        let flags = self.memory_property_flags(type_mask, usage_flags);

        if let Some(commit) = self.try_commit(requirements, flags) {
            return Ok(commit);
        }

        // Commit has failed, allocate more memory
        let chunk_size = allocation_chunk_size(requirements.size);
        if !self.try_alloc_memory(flags, type_mask, chunk_size)? {
            return Err(VulkanError::new(vk::Result::ERROR_OUT_OF_DEVICE_MEMORY));
        }

        // Commit again — should succeed now
        self.try_commit(requirements, flags)
            .ok_or_else(|| VulkanError::new(vk::Result::ERROR_OUT_OF_DEVICE_MEMORY))
    }

    /// Tries to allocate a chunk of device memory.
    ///
    /// Port of `MemoryAllocator::TryAllocMemory`.
    fn try_alloc_memory(
        &mut self,
        flags: vk::MemoryPropertyFlags,
        type_mask: u32,
        size: u64,
    ) -> Result<bool, VulkanError> {
        let type_index = match self.find_type(flags, type_mask) {
            Some(t) => t,
            None => return Ok(false),
        };

        let alloc_info = vk::MemoryAllocateInfo::builder()
            .allocation_size(size)
            .memory_type_index(type_index)
            .build();

        let memory = unsafe {
            match self.device.allocate_memory(&alloc_info, None) {
                Ok(mem) => mem,
                Err(_) => {
                    if flags.contains(vk::MemoryPropertyFlags::DEVICE_LOCAL) {
                        // Try without device local
                        return self.try_alloc_memory(
                            flags & !vk::MemoryPropertyFlags::DEVICE_LOCAL,
                            type_mask,
                            size,
                        );
                    }
                    return Ok(false);
                }
            }
        };

        self.allocations.push(MemoryAllocation::new(
            memory,
            size,
            flags,
            type_index,
        ));
        Ok(true)
    }

    /// Tries to commit from an existing allocation.
    ///
    /// Port of `MemoryAllocator::TryCommit`.
    fn try_commit(
        &mut self,
        requirements: &vk::MemoryRequirements,
        flags: vk::MemoryPropertyFlags,
    ) -> Option<MemoryCommit> {
        for (idx, allocation) in self.allocations.iter_mut().enumerate() {
            if !allocation.is_compatible(flags, requirements.memory_type_bits) {
                continue;
            }
            if let Some((begin, end)) = allocation.commit(requirements.size, requirements.alignment)
            {
                return Some(MemoryCommit::from_parts(idx, allocation.memory, begin, end));
            }
        }
        if flags.contains(vk::MemoryPropertyFlags::DEVICE_LOCAL) {
            // Try without device local
            return self.try_commit(
                requirements,
                flags & !vk::MemoryPropertyFlags::DEVICE_LOCAL,
            );
        }
        None
    }

    /// Returns the best compatible memory property flags.
    ///
    /// Port of `MemoryAllocator::MemoryPropertyFlags`.
    fn memory_property_flags(
        &self,
        type_mask: u32,
        flags: vk::MemoryPropertyFlags,
    ) -> vk::MemoryPropertyFlags {
        if self.find_type(flags, type_mask).is_some() {
            return flags;
        }
        if flags.contains(vk::MemoryPropertyFlags::HOST_CACHED) {
            return self.memory_property_flags(
                type_mask,
                flags & !vk::MemoryPropertyFlags::HOST_CACHED,
            );
        }
        if flags.contains(vk::MemoryPropertyFlags::DEVICE_LOCAL) {
            return self.memory_property_flags(
                type_mask,
                flags & !vk::MemoryPropertyFlags::DEVICE_LOCAL,
            );
        }
        log::error!("No compatible memory types found");
        vk::MemoryPropertyFlags::empty()
    }

    /// Finds a memory type index matching the given flags and type mask.
    ///
    /// Port of `MemoryAllocator::FindType`.
    fn find_type(&self, flags: vk::MemoryPropertyFlags, type_mask: u32) -> Option<u32> {
        for type_index in 0..self.properties.memory_type_count {
            let type_flags = self.properties.memory_types[type_index as usize].property_flags;
            if (type_mask & (1u32 << type_index)) != 0 && (type_flags & flags) == flags {
                return Some(type_index);
            }
        }
        None
    }

    /// Releases a commit. Called when a `MemoryCommit` is dropped.
    pub fn release_commit(&mut self, commit: &MemoryCommit) {
        if let Some(idx) = commit.allocation_index {
            if idx < self.allocations.len() {
                let freed = self.allocations[idx].free(commit.begin);
                if freed && self.allocations[idx].is_empty() {
                    // Free the Vulkan memory and remove the allocation
                    unsafe {
                        self.device
                            .free_memory(self.allocations[idx].memory, None);
                    }
                    self.allocations.remove(idx);
                }
            }
        }
    }
}

impl Drop for MemoryAllocator {
    fn drop(&mut self) {
        // Free all remaining allocations
        for alloc in &self.allocations {
            unsafe {
                self.device.free_memory(alloc.memory, None);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// ForEachDeviceLocalHostVisibleHeap helper
// ---------------------------------------------------------------------------

/// Iterates over device-local, host-visible memory heaps.
///
/// Port of `ForEachDeviceLocalHostVisibleHeap` from `vulkan_memory_allocator.h`.
pub fn for_each_device_local_host_visible_heap<F>(
    memory_props: &vk::PhysicalDeviceMemoryProperties,
    mut f: F,
) where
    F: FnMut(usize, &vk::MemoryHeap),
{
    for i in 0..memory_props.memory_type_count as usize {
        let memory_type = &memory_props.memory_types[i];
        if memory_type
            .property_flags
            .contains(vk::MemoryPropertyFlags::DEVICE_LOCAL)
            && memory_type
                .property_flags
                .contains(vk::MemoryPropertyFlags::HOST_VISIBLE)
        {
            let heap = &memory_props.memory_heaps[memory_type.heap_index as usize];
            f(memory_type.heap_index as usize, heap);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocation_chunk_size() {
        // Small size should round up to first chunk
        assert_eq!(allocation_chunk_size(1024), ALLOCATION_CHUNK_SIZES[0]);

        // Very large size should be aligned to 4 MiB
        let large = 0x30000u64 << 10;
        let result = allocation_chunk_size(large);
        assert!(result >= large);
        assert_eq!(result % (4 << 20), 0);
    }

    #[test]
    fn test_memory_usage_property_flags() {
        let flags = memory_usage_property_flags(MemoryUsage::DeviceLocal);
        assert!(flags.contains(vk::MemoryPropertyFlags::DEVICE_LOCAL));

        let flags = memory_usage_property_flags(MemoryUsage::Upload);
        assert!(flags.contains(vk::MemoryPropertyFlags::HOST_VISIBLE));
        assert!(flags.contains(vk::MemoryPropertyFlags::HOST_COHERENT));

        let flags = memory_usage_property_flags(MemoryUsage::Download);
        assert!(flags.contains(vk::MemoryPropertyFlags::HOST_CACHED));

        let flags = memory_usage_property_flags(MemoryUsage::Stream);
        assert!(flags.contains(vk::MemoryPropertyFlags::DEVICE_LOCAL));
        assert!(flags.contains(vk::MemoryPropertyFlags::HOST_VISIBLE));
    }

    #[test]
    fn test_range_contains() {
        let range = Range { begin: 10, end: 20 };
        assert!(range.contains(10, 5));
        assert!(range.contains(15, 5));
        assert!(!range.contains(20, 5));
        assert!(!range.contains(0, 5));
        assert!(range.contains(5, 10));
    }

    #[test]
    fn test_memory_allocation_find_free_region() {
        let alloc = MemoryAllocation::new(
            vk::DeviceMemory::null(),
            1024,
            vk::MemoryPropertyFlags::DEVICE_LOCAL,
            0,
        );
        // Empty allocation should find region at offset 0
        let region = alloc.find_free_region(256, 1);
        assert_eq!(region, Some(0));
    }
}
