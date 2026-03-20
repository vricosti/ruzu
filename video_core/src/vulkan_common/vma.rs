// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `zuyu/src/video_core/vulkan_common/vma.h` and
//! `zuyu/src/video_core/vulkan_common/vma.cpp`.
//!
//! The upstream files configure and compile the Vulkan Memory Allocator (VMA) C library
//! with dynamic Vulkan function loading (`VMA_STATIC_VULKAN_FUNCTIONS 0`,
//! `VMA_DYNAMIC_VULKAN_FUNCTIONS 1`). The `.cpp` file defines `VMA_IMPLEMENTATION`
//! to trigger compilation of VMA within the translation unit.
//!
//! In Rust, the `gpu-allocator` crate (or a similar allocator) replaces VMA entirely.
//! This module exists for structural parity and re-exports the allocator interface
//! used by the rest of the port.
//!
//! See [`super::vulkan_memory_allocator`] for the higher-level allocation subsystem
//! that consumes this allocator.

// The upstream VMA configuration constants, preserved for documentation:
//
// VMA_STATIC_VULKAN_FUNCTIONS  = 0  (do not link Vulkan statically)
// VMA_DYNAMIC_VULKAN_FUNCTIONS = 1  (resolve Vulkan functions at runtime)

/// Placeholder for VMA allocator handle.
///
/// Upstream type: `VmaAllocator` (opaque handle from `vk_mem_alloc.h`).
/// In Rust this is replaced by `gpu_allocator::vulkan::Allocator` or equivalent.
/// In the full port, this would be replaced by `gpu_allocator::vulkan::Allocator`.
/// The `gpu-allocator` crate is listed in the workspace but the VMA integration
/// layer in `vulkan_memory_allocator.rs` manages the concrete allocator instance.
/// This type alias remains a placeholder because switching to the real type requires
/// wiring up the `gpu_allocator::vulkan::Allocator` construction (which needs
/// `ash::Instance`, `ash::Device`, and `vk::PhysicalDevice`) throughout the
/// Vulkan memory allocator module.
pub type VmaAllocator = ();

/// Placeholder for VMA allocation handle.
///
/// Upstream type: `VmaAllocation` (opaque handle from `vk_mem_alloc.h`).
/// In the full port, this would be replaced by `gpu_allocator::vulkan::Allocation`.
/// See `VmaAllocator` above for why this remains a placeholder.
pub type VmaAllocation = ();

/// Placeholder for VMA allocation info.
///
/// Upstream type: `VmaAllocationInfo` from `vk_mem_alloc.h`.
#[derive(Debug, Clone, Default)]
pub struct VmaAllocationInfo {
    /// Offset in bytes from the beginning of the `VkDeviceMemory` object.
    pub offset: u64,
    /// Size of the allocation in bytes.
    pub size: u64,
    /// Pointer to mapped data. `None` if not mapped.
    pub mapped_data: Option<*mut u8>,
}

/// Placeholder for VMA allocation create info.
///
/// Upstream type: `VmaAllocationCreateInfo` from `vk_mem_alloc.h`.
#[derive(Debug, Clone, Default)]
pub struct VmaAllocationCreateInfo {
    /// Intended usage of the allocation.
    pub usage: VmaMemoryUsage,
    /// Flags for the allocation.
    pub flags: u32,
    /// Required memory property flags.
    pub required_flags: ash::vk::MemoryPropertyFlags,
    /// Preferred memory property flags.
    pub preferred_flags: ash::vk::MemoryPropertyFlags,
}

/// Upstream type: `VmaMemoryUsage` from `vk_mem_alloc.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum VmaMemoryUsage {
    #[default]
    Unknown = 0,
    GpuOnly = 1,
    CpuOnly = 2,
    CpuToGpu = 3,
    GpuToCpu = 4,
    CpuCopy = 5,
    GpuLazilyAllocated = 6,
    Auto = 7,
    AutoPreferDevice = 8,
    AutoPreferHost = 9,
}
