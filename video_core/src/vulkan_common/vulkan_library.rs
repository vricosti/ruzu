// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `zuyu/src/video_core/vulkan_common/vulkan_library.h` and
//! `zuyu/src/video_core/vulkan_common/vulkan_library.cpp`.
//!
//! Loads the Vulkan library and returns an `ash::Entry`.
//! In C++ this uses `Common::DynamicLibrary` to dlopen libvulkan;
//! in Rust, `ash::Entry::linked()` or `ash::Entry::load()` handles this.

use super::vulkan_wrapper::VulkanError;
use ash::vk;

/// Opens the Vulkan library and returns an `ash::Entry`.
///
/// Port of `Vulkan::OpenLibrary`.
///
/// On Linux, this attempts to load `libvulkan.so.1` then `libvulkan.so`.
/// On macOS, this would look for MoltenVK paths.
/// On Android, the frontend provides the library.
///
/// Using `ash::Entry::linked()` requires linking against libvulkan at build time.
/// Using `ash::Entry::load()` dynamically loads it at runtime (matching the C++ approach).
pub fn open_library() -> Result<ash::Entry, VulkanError> {
    log::debug!("Looking for a Vulkan library");

    // ash::Entry::load() will dlopen libvulkan.so / vulkan-1.dll / etc.
    // This matches the C++ behavior of dynamically loading the Vulkan library.
    match unsafe { ash::Entry::load() } {
        Ok(entry) => {
            log::debug!("Vulkan library loaded successfully");
            Ok(entry)
        }
        Err(e) => {
            log::error!("Failed to load Vulkan library: {}", e);
            Err(VulkanError::new(vk::Result::ERROR_INITIALIZATION_FAILED))
        }
    }
}

#[cfg(test)]
mod tests {
    // Note: These tests require a Vulkan runtime to be present on the system.
    // They are kept minimal to avoid CI failures on headless systems.

    #[test]
    fn test_open_library_does_not_panic() {
        // Just verify we don't panic; the actual result depends on the host.
        let _result = super::open_library();
    }
}
