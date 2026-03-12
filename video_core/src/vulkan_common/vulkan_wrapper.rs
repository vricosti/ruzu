// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `zuyu/src/video_core/vulkan_common/vulkan_wrapper.h` and
//! `zuyu/src/video_core/vulkan_common/vulkan_wrapper.cpp`.
//!
//! Provides RAII wrappers and dispatch tables for Vulkan objects.
//! In the C++ codebase this is a large custom Vulkan abstraction layer with
//! owning handles, dispatch tables (InstanceDispatch / DeviceDispatch),
//! and utility free functions. In Rust, ash provides most of this natively.
//!
//! This module re-exports ash types and provides thin compatibility shims
//! so the rest of the port can use names similar to the C++ `vk::` namespace.

use ash::vk;
use std::ffi::CStr;

// ---------------------------------------------------------------------------
// Exception / error type — port of `vk::Exception`
// ---------------------------------------------------------------------------

/// Vulkan error generated from a `VkResult`.
///
/// Port of `vk::Exception`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VulkanError {
    pub result: vk::Result,
}

impl VulkanError {
    pub fn new(result: vk::Result) -> Self {
        Self { result }
    }
}

impl std::fmt::Display for VulkanError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Vulkan error: {:?}", self.result)
    }
}

impl std::error::Error for VulkanError {}

/// Port of `vk::Check` — returns `Err` on non-success results.
pub fn check(result: vk::Result) -> Result<(), VulkanError> {
    if result != vk::Result::SUCCESS {
        Err(VulkanError::new(result))
    } else {
        Ok(())
    }
}

/// Port of `vk::Filter` — returns `Err` only on error results (negative).
pub fn filter(result: vk::Result) -> Result<vk::Result, VulkanError> {
    if result.as_raw() < 0 {
        Err(VulkanError::new(result))
    } else {
        Ok(result)
    }
}

// ---------------------------------------------------------------------------
// Vendor IDs — used in physical device sorting
// ---------------------------------------------------------------------------

/// Nvidia vendor ID.
pub const VENDOR_ID_NVIDIA: u32 = 0x10DE;
/// AMD vendor ID.
pub const VENDOR_ID_AMD: u32 = 0x1002;
/// Intel vendor ID.
pub const VENDOR_ID_INTEL: u32 = 0x8086;

// ---------------------------------------------------------------------------
// Physical device sorting — port of anonymous-namespace helpers in vulkan_wrapper.cpp
// ---------------------------------------------------------------------------

/// Returns true if the device name contains "Microsoft" (Dozen driver).
fn is_microsoft_dozen(device_name: &str) -> bool {
    device_name.contains("Microsoft")
}

/// Sorts physical devices by preference.
///
/// Port of `SortPhysicalDevices` from `vulkan_wrapper.cpp`.
/// Preference order:
/// 1. Demote Microsoft Dozen devices
/// 2. Prefer Nvidia > AMD > Intel
/// 3. Prefer discrete GPUs
/// 4. Sort by name descending (higher model numbers first)
pub fn sort_physical_devices(
    devices: &mut Vec<vk::PhysicalDevice>,
    instance: &ash::Instance,
) {
    // We need properties for sorting. Collect them once.
    let get_props = |dev: vk::PhysicalDevice| -> vk::PhysicalDeviceProperties {
        unsafe { instance.get_physical_device_properties(dev) }
    };

    // Sort by name descending
    devices.sort_by(|&a, &b| {
        let name_a = unsafe {
            CStr::from_ptr(get_props(a).device_name.as_ptr())
                .to_string_lossy()
        };
        let name_b = unsafe {
            CStr::from_ptr(get_props(b).device_name.as_ptr())
                .to_string_lossy()
        };
        name_b.cmp(&name_a)
    });

    // Prefer discrete over non-discrete
    devices.sort_by(|&a, &b| {
        let a_discrete = get_props(a).device_type == vk::PhysicalDeviceType::DISCRETE_GPU;
        let b_discrete = get_props(b).device_type == vk::PhysicalDeviceType::DISCRETE_GPU;
        b_discrete.cmp(&a_discrete)
    });

    // Prefer Nvidia > AMD > Intel
    let vendor_priority = |vendor_id: u32| -> u32 {
        match vendor_id {
            VENDOR_ID_NVIDIA => 3,
            VENDOR_ID_AMD => 2,
            VENDOR_ID_INTEL => 1,
            _ => 0,
        }
    };
    devices.sort_by(|&a, &b| {
        let pa = vendor_priority(get_props(a).vendor_id);
        let pb = vendor_priority(get_props(b).vendor_id);
        pb.cmp(&pa)
    });

    // Demote Microsoft Dozen devices
    devices.sort_by(|&a, &b| {
        let name_a = unsafe {
            CStr::from_ptr(get_props(a).device_name.as_ptr())
                .to_string_lossy()
        };
        let name_b = unsafe {
            CStr::from_ptr(get_props(b).device_name.as_ptr())
                .to_string_lossy()
        };
        let a_dozen = is_microsoft_dozen(&name_a);
        let b_dozen = is_microsoft_dozen(&name_b);
        a_dozen.cmp(&b_dozen)
    });
}

// ---------------------------------------------------------------------------
// Instance wrapper — thin wrapper around ash::Instance
// ---------------------------------------------------------------------------

/// RAII wrapper around an `ash::Instance`.
///
/// Port of `vk::Instance` from `vulkan_wrapper.h`.
/// Owns the Vulkan instance and its entry point, destroying the instance on drop.
pub struct Instance {
    pub entry: ash::Entry,
    pub instance: ash::Instance,
}

impl Instance {
    /// Creates a Vulkan instance.
    ///
    /// Port of `vk::Instance::Create`.
    pub fn create(
        entry: ash::Entry,
        app_info: &vk::ApplicationInfo,
        layers: &[*const std::os::raw::c_char],
        extensions: &[*const std::os::raw::c_char],
        flags: vk::InstanceCreateFlags,
    ) -> Result<Self, VulkanError> {
        let create_info = vk::InstanceCreateInfo::builder()
            .application_info(app_info)
            .enabled_layer_names(layers)
            .enabled_extension_names(extensions)
            .flags(flags)
            .build();

        let instance = unsafe {
            entry
                .create_instance(&create_info, None)
                .map_err(|e| VulkanError::new(e))?
        };

        Ok(Self { entry, instance })
    }

    /// Enumerates physical devices, sorted by preference.
    ///
    /// Port of `Instance::EnumeratePhysicalDevices`.
    pub fn enumerate_physical_devices(&self) -> Result<Vec<vk::PhysicalDevice>, VulkanError> {
        let mut devices = unsafe {
            self.instance
                .enumerate_physical_devices()
                .map_err(|e| VulkanError::new(e))?
        };
        sort_physical_devices(&mut devices, &self.instance);
        Ok(devices)
    }
}

impl Drop for Instance {
    fn drop(&mut self) {
        unsafe {
            self.instance.destroy_instance(None);
        }
    }
}

// ---------------------------------------------------------------------------
// Device wrapper — thin wrapper around ash::Device
// ---------------------------------------------------------------------------

/// RAII wrapper around an `ash::Device`.
///
/// Port of `vk::Device` from `vulkan_wrapper.h`.
/// Owns the logical Vulkan device and destroys it on drop.
pub struct LogicalDevice {
    pub device: ash::Device,
}

impl LogicalDevice {
    /// Creates a logical device.
    ///
    /// Port of `vk::Device::Create`.
    pub fn create(
        instance: &ash::Instance,
        physical_device: vk::PhysicalDevice,
        create_info: &vk::DeviceCreateInfo,
    ) -> Result<Self, VulkanError> {
        let device = unsafe {
            instance
                .create_device(physical_device, create_info, None)
                .map_err(|e| VulkanError::new(e))?
        };
        Ok(Self { device })
    }

    /// Returns a queue from the device.
    ///
    /// Port of `Device::GetQueue`.
    pub fn get_queue(&self, family_index: u32) -> vk::Queue {
        unsafe { self.device.get_device_queue(family_index, 0) }
    }

    /// Waits for the device to be idle.
    pub fn wait_idle(&self) -> Result<(), VulkanError> {
        unsafe {
            self.device
                .device_wait_idle()
                .map_err(|e| VulkanError::new(e))
        }
    }
}

impl Drop for LogicalDevice {
    fn drop(&mut self) {
        unsafe {
            self.device.destroy_device(None);
        }
    }
}

// ---------------------------------------------------------------------------
// Available version query — port of `vk::AvailableVersion`
// ---------------------------------------------------------------------------

/// Queries the available Vulkan API version.
///
/// Port of `vk::AvailableVersion` from `vulkan_wrapper.cpp`.
pub fn available_version(entry: &ash::Entry) -> u32 {
    match entry.try_enumerate_instance_version() {
        Ok(Some(version)) => version,
        Ok(None) => vk::API_VERSION_1_0,
        Err(e) => {
            log::error!(
                "vkEnumerateInstanceVersion failed: {:?}, assuming Vulkan 1.1",
                e
            );
            vk::API_VERSION_1_1
        }
    }
}

// ---------------------------------------------------------------------------
// Extension / layer enumeration helpers
// ---------------------------------------------------------------------------

/// Enumerates instance extension properties.
///
/// Port of `vk::EnumerateInstanceExtensionProperties`.
pub fn enumerate_instance_extension_properties(
    entry: &ash::Entry,
) -> Option<Vec<vk::ExtensionProperties>> {
    unsafe {
        entry
            .enumerate_instance_extension_properties(None)
            .ok()
    }
}

/// Enumerates instance layer properties.
///
/// Port of `vk::EnumerateInstanceLayerProperties`.
pub fn enumerate_instance_layer_properties(
    entry: &ash::Entry,
) -> Option<Vec<vk::LayerProperties>> {
    unsafe {
        entry.enumerate_instance_layer_properties().ok()
    }
}

// ---------------------------------------------------------------------------
// Re-exports for convenience — these map to C++ `vk::` namespace types
// ---------------------------------------------------------------------------

// The C++ codebase defines many RAII handle types (Image, Buffer, BufferView,
// Fence, Semaphore, etc.) with custom Drop. In Rust with ash, these are
// managed differently: ash provides raw handles and the user manages lifetime.
//
// For now we provide type aliases; full RAII wrappers can be added as the
// renderer port progresses.

/// Type alias matching C++ `vk::Span<T>`.
/// In Rust, `&[T]` serves the same purpose.
pub type Span<'a, T> = &'a [T];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vulkan_error_display() {
        let err = VulkanError::new(vk::Result::ERROR_INITIALIZATION_FAILED);
        let msg = format!("{}", err);
        assert!(msg.contains("ERROR_INITIALIZATION_FAILED"));
    }

    #[test]
    fn test_check_success() {
        assert!(check(vk::Result::SUCCESS).is_ok());
    }

    #[test]
    fn test_check_failure() {
        assert!(check(vk::Result::ERROR_OUT_OF_HOST_MEMORY).is_err());
    }

    #[test]
    fn test_filter_success() {
        assert_eq!(filter(vk::Result::SUCCESS).unwrap(), vk::Result::SUCCESS);
    }

    #[test]
    fn test_filter_positive() {
        // VK_NOT_READY is positive (not an error)
        assert!(filter(vk::Result::NOT_READY).is_ok());
    }

    #[test]
    fn test_filter_error() {
        assert!(filter(vk::Result::ERROR_DEVICE_LOST).is_err());
    }
}
