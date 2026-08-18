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
//! The Rust port uses `vk-mem`, which wraps the same AMD Vulkan Memory Allocator
//! implementation as upstream. This module owns the binding choice while
//! [`super::vulkan_memory_allocator`] owns Eden's higher-level allocation policy.

// The upstream VMA configuration constants, preserved for documentation:
//
// VMA_STATIC_VULKAN_FUNCTIONS  = 0  (do not link Vulkan statically)
// VMA_DYNAMIC_VULKAN_FUNCTIONS = 1  (resolve Vulkan functions at runtime)

use std::sync::{Arc, Mutex};

/// Rust ownership wrapper for upstream's opaque `VmaAllocator` handle.
///
/// Eden creates VMA with `VMA_ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT`;
/// the mutex provides that external synchronization in Rust.
pub type VmaAllocator = Arc<Mutex<vk_mem::Allocator>>;

pub type VmaAllocation = vk_mem::Allocation;
pub type VmaAllocationInfo = vk_mem::AllocationInfo;
pub type VmaAllocationCreateInfo = vk_mem::AllocationCreateInfo;
pub type VmaMemoryUsage = vk_mem::MemoryUsage;
