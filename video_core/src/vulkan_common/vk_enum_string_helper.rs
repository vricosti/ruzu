// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `zuyu/src/video_core/vulkan_common/vk_enum_string_helper.h`.
//!
//! The upstream header includes `<vulkan/vk_enum_string_helper.h>`, which provides
//! `string_VkResult`, `string_VkFormat`, and similar functions that convert Vulkan
//! enum values to human-readable strings for debug output.
//!
//! In Rust, the `ash` crate derives `Debug` on all Vulkan enum types, so `{:?}`
//! formatting produces equivalent debug strings. This module provides thin wrapper
//! functions that match the upstream naming convention for call-site parity.

use ash::vk;

/// Returns a human-readable string for a `VkResult` value.
///
/// Port of `string_VkResult()` from `vk_enum_string_helper.h`.
pub fn string_vk_result(result: vk::Result) -> String {
    format!("{:?}", result)
}

/// Returns a human-readable string for a `VkFormat` value.
///
/// Port of `string_VkFormat()` from `vk_enum_string_helper.h`.
pub fn string_vk_format(format: vk::Format) -> String {
    format!("{:?}", format)
}

/// Returns a human-readable string for a `VkPresentModeKHR` value.
///
/// Port of `string_VkPresentModeKHR()` from `vk_enum_string_helper.h`.
pub fn string_vk_present_mode(mode: vk::PresentModeKHR) -> String {
    format!("{:?}", mode)
}

/// Returns a human-readable string for a `VkColorSpaceKHR` value.
///
/// Port of `string_VkColorSpaceKHR()` from `vk_enum_string_helper.h`.
pub fn string_vk_color_space(color_space: vk::ColorSpaceKHR) -> String {
    format!("{:?}", color_space)
}

/// Returns a human-readable string for a `VkImageLayout` value.
///
/// Port of `string_VkImageLayout()` from `vk_enum_string_helper.h`.
pub fn string_vk_image_layout(layout: vk::ImageLayout) -> String {
    format!("{:?}", layout)
}

/// Returns a human-readable string for a `VkImageTiling` value.
///
/// Port of `string_VkImageTiling()` from `vk_enum_string_helper.h`.
pub fn string_vk_image_tiling(tiling: vk::ImageTiling) -> String {
    format!("{:?}", tiling)
}

/// Returns a human-readable string for a `VkPhysicalDeviceType` value.
///
/// Port of `string_VkPhysicalDeviceType()` from `vk_enum_string_helper.h`.
pub fn string_vk_physical_device_type(device_type: vk::PhysicalDeviceType) -> String {
    format!("{:?}", device_type)
}

/// Returns a human-readable string for a `VkObjectType` value.
///
/// Port of `string_VkObjectType()` from `vk_enum_string_helper.h`.
pub fn string_vk_object_type(object_type: vk::ObjectType) -> String {
    format!("{:?}", object_type)
}

/// Returns a human-readable string for a `VkSharingMode` value.
///
/// Port of `string_VkSharingMode()` from `vk_enum_string_helper.h`.
pub fn string_vk_sharing_mode(mode: vk::SharingMode) -> String {
    format!("{:?}", mode)
}

/// Returns a human-readable string for a `VkDescriptorType` value.
///
/// Port of `string_VkDescriptorType()` from `vk_enum_string_helper.h`.
pub fn string_vk_descriptor_type(desc_type: vk::DescriptorType) -> String {
    format!("{:?}", desc_type)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_result_to_string() {
        let s = string_vk_result(vk::Result::SUCCESS);
        assert!(!s.is_empty());
    }

    #[test]
    fn test_format_to_string() {
        let s = string_vk_format(vk::Format::R8G8B8A8_UNORM);
        assert!(!s.is_empty());
    }
}
