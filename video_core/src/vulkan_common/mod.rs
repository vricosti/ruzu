// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `zuyu/src/video_core/vulkan_common/`
//!
//! Vulkan subsystem shared between the Vulkan renderer and other GPU components.
//! Provides RAII wrappers around Vulkan objects, device enumeration, instance creation,
//! surface creation, memory allocation, debug callbacks, and library loading.

pub mod nsight_aftermath_tracker;
pub mod vk_enum_string_helper;
pub mod vma;
pub mod vulkan_debug_callback;
pub mod vulkan_device;
pub mod vulkan_instance;
pub mod vulkan_library;
pub mod vulkan_memory_allocator;
pub mod vulkan_surface;
pub mod vulkan_wrapper;

// Note: `vulkan.h` from upstream is an aggregator header that configures platform
// defines (`VK_NO_PROTOTYPES`, `VK_USE_PLATFORM_*`) and includes `<vulkan/vulkan.h>`.
// In Rust, the `ash` crate handles all of this automatically, so no separate
// `vulkan.rs` file is needed. This is not a structural omission.
