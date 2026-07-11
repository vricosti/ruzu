// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `zuyu/src/video_core/vulkan_common/vulkan_device.h` and
//! `zuyu/src/video_core/vulkan_common/vulkan_device.cpp`.
//!
//! Manages a Vulkan physical+logical device pair, collecting device properties,
//! features, and extensions. Provides accessors matching the C++ `Device` class.

use ash::vk;
use std::collections::{BTreeSet, HashMap};
use std::ffi::{CStr, CString};

use super::nsight_aftermath_tracker::NsightAftermathTracker;
use super::vulkan_wrapper::{LogicalDevice, VulkanError};

// ---------------------------------------------------------------------------
// Constants — port of constants from vulkan_device.h
// ---------------------------------------------------------------------------

/// Subgroup size of the guest emulated hardware (Nvidia has 32 threads per subgroup).
///
/// Port of `GuestWarpSize` from `vulkan_device.h`.
pub const GUEST_WARP_SIZE: u32 = 32;
const ONE_GIB: u64 = 1024 * 1024 * 1024;

// ---------------------------------------------------------------------------
// FormatType — port of `Vulkan::FormatType`
// ---------------------------------------------------------------------------

/// Format usage descriptor.
///
/// Port of `Vulkan::FormatType` from `vulkan_device.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatType {
    Linear,
    Optimal,
    Buffer,
}

/// Snapshot of the Vulkan memory heaps tracked by `Device`.
///
/// Port-facing helper for runtime owners that currently store
/// `Instance + PhysicalDevice` instead of the full `Device` wrapper but still
/// need upstream `Device::GetDeviceLocalMemory`,
/// `Device::CanReportMemoryUsage`, and `Device::GetDeviceMemoryUsage`
/// semantics.
#[derive(Debug, Clone)]
pub struct DeviceMemoryInfo {
    pub device_local_memory: u64,
    pub can_report_memory_usage: bool,
    valid_heap_memory: Vec<usize>,
}

// ---------------------------------------------------------------------------
// NvidiaArchitecture — port of `Vulkan::NvidiaArchitecture`
// ---------------------------------------------------------------------------

/// NVIDIA GPU architecture classification.
///
/// Port of `Vulkan::NvidiaArchitecture` from `vulkan_device.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NvidiaArchitecture {
    KeplerOrOlder,
    Maxwell,
    Pascal,
    Volta,
    Turing,
    AmpereOrNewer,
}

// ---------------------------------------------------------------------------
// Format alternatives — port of namespace Alternatives from vulkan_device.cpp
// ---------------------------------------------------------------------------

/// Alternative format lists for unsupported formats.
///
/// Port of `namespace Alternatives` from `vulkan_device.cpp`.
mod alternatives {
    use ash::vk;

    pub const STENCIL8_UINT: &[vk::Format] = &[
        vk::Format::D16_UNORM_S8_UINT,
        vk::Format::D24_UNORM_S8_UINT,
        vk::Format::D32_SFLOAT_S8_UINT,
    ];

    pub const DEPTH24_UNORM_STENCIL8_UINT: &[vk::Format] = &[
        vk::Format::D32_SFLOAT_S8_UINT,
        vk::Format::D16_UNORM_S8_UINT,
    ];

    pub const DEPTH24_UNORM_DONTCARE8: &[vk::Format] =
        &[vk::Format::D32_SFLOAT, vk::Format::D16_UNORM];

    pub const DEPTH16_UNORM_STENCIL8_UINT: &[vk::Format] = &[
        vk::Format::D24_UNORM_S8_UINT,
        vk::Format::D32_SFLOAT_S8_UINT,
    ];

    pub const B5G6R5_UNORM_PACK16: &[vk::Format] = &[vk::Format::R5G6B5_UNORM_PACK16];

    pub const R4G4_UNORM_PACK8: &[vk::Format] = &[vk::Format::R8_UNORM];

    pub const R16G16B16_SFLOAT: &[vk::Format] = &[vk::Format::R16G16B16A16_SFLOAT];

    pub const R16G16B16_SSCALED: &[vk::Format] = &[vk::Format::R16G16B16A16_SSCALED];

    pub const R8G8B8_SSCALED: &[vk::Format] = &[vk::Format::R8G8B8A8_SSCALED];

    pub const R32G32B32_SFLOAT: &[vk::Format] = &[vk::Format::R32G32B32A32_SFLOAT];

    pub const A4B4G4R4_UNORM_PACK16: &[vk::Format] = &[vk::Format::R4G4B4A4_UNORM_PACK16];

    /// Returns the alternative formats for a given format, or `None` if no alternatives exist.
    pub fn get_format_alternatives(format: vk::Format) -> Option<&'static [vk::Format]> {
        match format {
            vk::Format::S8_UINT => Some(STENCIL8_UINT),
            vk::Format::D24_UNORM_S8_UINT => Some(DEPTH24_UNORM_STENCIL8_UINT),
            vk::Format::D16_UNORM_S8_UINT => Some(DEPTH16_UNORM_STENCIL8_UINT),
            vk::Format::B5G6R5_UNORM_PACK16 => Some(B5G6R5_UNORM_PACK16),
            vk::Format::R16G16B16_SFLOAT => Some(R16G16B16_SFLOAT),
            vk::Format::R16G16B16_SSCALED => Some(R16G16B16_SSCALED),
            vk::Format::R8G8B8_SSCALED => Some(R8G8B8_SSCALED),
            vk::Format::R32G32B32_SFLOAT => Some(R32G32B32_SFLOAT),
            vk::Format::A4B4G4R4_UNORM_PACK16_EXT => Some(A4B4G4R4_UNORM_PACK16),
            _ => None,
        }
    }
}

/// Returns the alternative formats for a given format.
///
/// Port-facing wrapper for upstream `namespace Alternatives`; runtime owners
/// that do not store the full `Device` wrapper still need the same fallback
/// list when reproducing `Device::GetSupportedFormat`.
pub fn format_alternatives(format: vk::Format) -> Option<&'static [vk::Format]> {
    alternatives::get_format_alternatives(format)
}

// ---------------------------------------------------------------------------
// Extensions tracking — port of `Device::Extensions` struct
// ---------------------------------------------------------------------------

/// Tracked device extensions.
///
/// Port of the `Device::Extensions` struct from `vulkan_device.h`.
/// Fields correspond to the `FOR_EACH_VK_FEATURE_*` and `FOR_EACH_VK_EXTENSION` macros.
#[derive(Debug, Default, Clone)]
pub struct DeviceExtensions {
    // VK features 1.1
    pub bit16_storage: bool,
    pub shader_atomic_int64: bool,
    pub shader_draw_parameters: bool,
    pub shader_float16_int8: bool,
    pub uniform_buffer_standard_layout: bool,
    pub variable_pointer: bool,

    // VK features 1.2
    pub host_query_reset: bool,
    pub bit8_storage: bool,
    pub timeline_semaphore: bool,

    // VK features 1.3
    pub shader_demote_to_helper_invocation: bool,
    pub subgroup_size_control: bool,

    // VK feature extensions
    pub custom_border_color: bool,
    pub depth_bias_control: bool,
    pub depth_clip_control: bool,
    pub extended_dynamic_state: bool,
    pub extended_dynamic_state2: bool,
    pub extended_dynamic_state3: bool,
    pub format_a4b4g4r4: bool,
    pub index_type_uint8: bool,
    pub line_rasterization: bool,
    pub primitive_topology_list_restart: bool,
    pub provoking_vertex: bool,
    pub robustness2: bool,
    pub transform_feedback: bool,
    pub vertex_input_dynamic_state: bool,
    pub pipeline_executable_properties: bool,
    pub workgroup_memory_explicit_layout: bool,

    // Misc extensions
    pub conditional_rendering: bool,
    pub conservative_rasterization: bool,
    pub depth_range_unrestricted: bool,
    pub memory_budget: bool,
    pub robustness_2: bool,
    pub sampler_filter_minmax: bool,
    pub shader_stencil_export: bool,
    pub shader_viewport_index_layer: bool,
    pub tooling_info: bool,
    pub vertex_attribute_divisor: bool,
    pub draw_indirect_count: bool,
    pub driver_properties: bool,
    pub push_descriptor: bool,
    pub sampler_mirror_clamp_to_edge: bool,
    pub shader_float_controls: bool,
    pub spirv_1_4: bool,
    pub swapchain: bool,
    pub swapchain_mutable_format: bool,
    pub image_format_list: bool,
    pub device_diagnostics_config: bool,
    pub geometry_shader_passthrough: bool,
    pub viewport_array2: bool,
    pub viewport_swizzle: bool,
}

// ---------------------------------------------------------------------------
// Device — port of `Vulkan::Device`
// ---------------------------------------------------------------------------

/// Handles data specific to a physical device.
///
/// Port of `Vulkan::Device` from `vulkan_device.h`.
/// This struct collects device properties, features, extensions, and manages
/// the logical device and queues.
pub struct Device {
    /// Vulkan instance handle.
    instance: ash::Instance,
    /// Physical device handle.
    physical: vk::PhysicalDevice,
    /// Logical device wrapper.
    logical: LogicalDevice,
    /// Device dispatch (ash device handle).
    _dld: ash::Device,
    /// Main graphics queue.
    graphics_queue: vk::Queue,
    /// Main present queue.
    present_queue: vk::Queue,
    /// Instance version.
    instance_version: u32,
    /// Main graphics queue family index.
    graphics_family: u32,
    /// Main present queue family index.
    present_family: u32,

    /// Tracked extensions.
    pub extensions: DeviceExtensions,

    /// Physical device properties.
    pub device_properties: vk::PhysicalDeviceProperties,
    /// Physical device driver properties.
    pub driver_properties: vk::PhysicalDeviceDriverProperties,
    /// Subgroup properties.
    pub subgroup_properties: vk::PhysicalDeviceSubgroupProperties,
    /// Float controls properties.
    pub float_controls_properties: vk::PhysicalDeviceFloatControlsProperties,
    /// Push descriptor properties.
    pub push_descriptor_properties: vk::PhysicalDevicePushDescriptorPropertiesKHR,
    /// Subgroup size control properties.
    pub subgroup_size_control_properties: vk::PhysicalDeviceSubgroupSizeControlProperties,

    /// Core physical device features.
    pub device_features: vk::PhysicalDeviceFeatures,
    /// Feature bit from `VkPhysicalDeviceShaderFloat16Int8Features`.
    pub shader_float16_supported: bool,
    /// Whether `VkPhysicalDeviceTimelineSemaphoreFeatures::timelineSemaphore`
    /// is supported and enabled (backs the scheduler's MasterSemaphore).
    pub timeline_semaphore_supported: bool,
    /// Feature bit from `VkPhysicalDeviceShaderFloat16Int8Features`.
    pub shader_int8_supported: bool,
    /// Feature bit from `VkPhysicalDevicePrimitiveTopologyListRestartFeaturesEXT`.
    pub primitive_topology_list_restart_supported: bool,
    /// Feature bit from `VkPhysicalDevicePrimitiveTopologyListRestartFeaturesEXT`.
    pub primitive_topology_patch_list_restart_supported: bool,

    // Misc capability flags
    pub is_optimal_astc_supported: bool,
    pub is_blit_depth24_stencil8_supported: bool,
    pub is_blit_depth32_stencil8_supported: bool,
    pub is_warp_potentially_bigger: bool,
    pub is_integrated: bool,
    pub is_virtual: bool,
    pub is_non_gpu: bool,
    pub has_broken_compute: bool,
    pub has_broken_cube_compatibility: bool,
    pub has_broken_parallel_compiling: bool,
    pub has_renderdoc: bool,
    pub has_nsight_graphics: bool,
    pub supports_d24_depth: bool,
    pub cant_blit_msaa: bool,
    pub must_emulate_scaled_formats: bool,
    pub must_emulate_bgr565: bool,
    pub dynamic_state3_blending: bool,
    pub dynamic_state3_enables: bool,
    pub supports_conditional_barriers: bool,
    pub device_access_memory: u64,
    pub sets_per_pool: u32,
    pub nvidia_arch: NvidiaArchitecture,

    /// Reported Vulkan extensions.
    pub supported_extensions: BTreeSet<String>,
    /// Loaded Vulkan extensions.
    pub loaded_extensions: BTreeSet<String>,
    /// Memory heaps used for device-accessible memory accounting.
    pub valid_heap_memory: Vec<usize>,
    /// Format properties dictionary.
    pub format_properties: HashMap<vk::Format, vk::FormatProperties>,

    /// Nsight Aftermath tracker.
    pub nsight_aftermath_tracker: NsightAftermathTracker,
}

impl Device {
    /// Creates a new Device from a physical device.
    ///
    /// Port of `Device::Device` from `vulkan_device.cpp`.
    /// This is the main constructor that probes device properties, selects queue families,
    /// creates the logical device, and initializes all tracked capabilities.
    ///
    /// The full initialization is complex (~1500 lines in C++). This provides the
    /// structural skeleton. The full extension/feature chain construction, queue
    /// family selection with surface support, and feature probing are not yet
    /// implemented because they require the full `vk::PhysicalDevice` feature
    /// chain structs (VkPhysicalDeviceFeatures2, etc.) and surface KHR query
    /// support which are not wired up yet. When those are available, this
    /// constructor should be expanded to match upstream `Device::Device`.
    pub fn new(
        instance: ash::Instance,
        physical: vk::PhysicalDevice,
        _surface: vk::SurfaceKHR,
    ) -> Result<Self, VulkanError> {
        // Query basic properties
        let mut device_properties = unsafe { instance.get_physical_device_properties(physical) };

        let device_features = unsafe { instance.get_physical_device_features(physical) };

        // Query queue families
        let queue_families =
            unsafe { instance.get_physical_device_queue_family_properties(physical) };

        // Find graphics queue family
        let graphics_family = queue_families
            .iter()
            .enumerate()
            .find(|(_, props)| props.queue_flags.contains(vk::QueueFlags::GRAPHICS))
            .map(|(i, _)| i as u32)
            .ok_or_else(|| VulkanError::new(vk::Result::ERROR_INITIALIZATION_FAILED))?;

        // Upstream checks vkGetPhysicalDeviceSurfaceSupportKHR per queue family to find
        // the present queue. This requires the VK_KHR_surface extension query which is
        // not yet wired. For now, use the same family for present (matches the common
        // case where the graphics queue also supports present).
        let present_family = graphics_family;

        // Enumerate device extensions
        let available_extensions = unsafe {
            instance
                .enumerate_device_extension_properties(physical)
                .unwrap_or_default()
        };
        let supported_extensions: BTreeSet<String> = available_extensions
            .iter()
            .map(|ext| {
                let name = unsafe { CStr::from_ptr(ext.extension_name.as_ptr()) };
                name.to_string_lossy().into_owned()
            })
            .collect();
        let mut driver_properties = vk::PhysicalDeviceDriverProperties::default();
        if device_properties.api_version >= vk::API_VERSION_1_2
            || supported_extensions.contains("VK_KHR_driver_properties")
        {
            let mut properties2 = vk::PhysicalDeviceProperties2::builder()
                .push_next(&mut driver_properties)
                .build();
            unsafe {
                instance.get_physical_device_properties2(physical, &mut properties2);
            }
            device_properties = properties2.properties;
        }
        let has_memory_budget = supported_extensions.contains("VK_EXT_memory_budget");
        let is_integrated = device_properties.device_type == vk::PhysicalDeviceType::INTEGRATED_GPU;
        let (memory_properties, memory_budget_properties) =
            physical_memory_properties(&instance, physical, has_memory_budget);

        // Build queue create infos
        let queue_priority = [1.0f32];
        let mut queue_create_infos = vec![vk::DeviceQueueCreateInfo::builder()
            .queue_family_index(graphics_family)
            .queue_priorities(&queue_priority)
            .build()];
        if present_family != graphics_family {
            queue_create_infos.push(
                vk::DeviceQueueCreateInfo::builder()
                    .queue_family_index(present_family)
                    .queue_priorities(&queue_priority)
                    .build(),
            );
        }

        let has_primitive_topology_list_restart =
            supported_extensions.contains("VK_EXT_primitive_topology_list_restart");
        // Read after the features2 query below.
        let has_portability_subset = supported_extensions.contains("VK_KHR_portability_subset");
        let has_extended_dynamic_state =
            supported_extensions.contains("VK_EXT_extended_dynamic_state");
        let has_extended_dynamic_state2 =
            supported_extensions.contains("VK_EXT_extended_dynamic_state2");
        let has_vertex_attribute_divisor =
            supported_extensions.contains("VK_EXT_vertex_attribute_divisor");
        let has_shader_demote_to_helper_invocation = supported_extensions
            .contains("VK_EXT_shader_demote_to_helper_invocation")
            || device_properties.api_version >= vk::API_VERSION_1_3;
        let mut shader_float16_int8_features =
            vk::PhysicalDeviceShaderFloat16Int8Features::default();
        let mut portability_subset_features =
            vk::PhysicalDevicePortabilitySubsetFeaturesKHR::default();
        let mut timeline_semaphore_features =
            vk::PhysicalDeviceTimelineSemaphoreFeatures::default();
        let mut primitive_topology_list_restart_features =
            vk::PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT::default();
        let mut extended_dynamic_state_features =
            vk::PhysicalDeviceExtendedDynamicStateFeaturesEXT::default();
        let mut extended_dynamic_state2_features =
            vk::PhysicalDeviceExtendedDynamicState2FeaturesEXT::default();
        let mut vertex_attribute_divisor_features =
            vk::PhysicalDeviceVertexAttributeDivisorFeaturesEXT::default();
        let mut shader_demote_features =
            vk::PhysicalDeviceShaderDemoteToHelperInvocationFeatures::default();
        {
            let mut features2_builder = vk::PhysicalDeviceFeatures2::builder()
                .push_next(&mut shader_float16_int8_features)
                .push_next(&mut timeline_semaphore_features);
            if has_portability_subset {
                features2_builder = features2_builder.push_next(&mut portability_subset_features);
            }
            if has_primitive_topology_list_restart {
                features2_builder =
                    features2_builder.push_next(&mut primitive_topology_list_restart_features);
            }
            if has_extended_dynamic_state {
                features2_builder =
                    features2_builder.push_next(&mut extended_dynamic_state_features);
            }
            if has_extended_dynamic_state2 {
                features2_builder =
                    features2_builder.push_next(&mut extended_dynamic_state2_features);
            }
            if has_vertex_attribute_divisor {
                features2_builder =
                    features2_builder.push_next(&mut vertex_attribute_divisor_features);
            }
            if has_shader_demote_to_helper_invocation {
                features2_builder = features2_builder.push_next(&mut shader_demote_features);
            }
            let mut features2 = features2_builder.build();
            unsafe {
                instance.get_physical_device_features2(physical, &mut features2);
            }
        }

        let supports_shader_float16 = shader_float16_int8_features.shader_float16 != 0;
        let supports_shader_int8 = shader_float16_int8_features.shader_int8 != 0;
        let supports_timeline_semaphore = timeline_semaphore_features.timeline_semaphore != 0;
        let supports_primitive_topology_list_restart =
            primitive_topology_list_restart_features.primitive_topology_list_restart != 0;
        let supports_primitive_topology_patch_list_restart =
            primitive_topology_list_restart_features.primitive_topology_patch_list_restart != 0;
        let supports_extended_dynamic_state = has_extended_dynamic_state
            && extended_dynamic_state_features.extended_dynamic_state != 0;
        let supports_extended_dynamic_state2 = has_extended_dynamic_state2
            && extended_dynamic_state2_features.extended_dynamic_state2 != 0;
        let supports_vertex_attribute_divisor = has_vertex_attribute_divisor
            && vertex_attribute_divisor_features.vertex_attribute_instance_rate_divisor != 0;
        let supports_shader_demote_to_helper_invocation = has_shader_demote_to_helper_invocation
            && shader_demote_features.shader_demote_to_helper_invocation != 0;
        log::info!(
            "Vulkan primitive topology restart: extension={} list={} patch={}",
            has_primitive_topology_list_restart,
            supports_primitive_topology_list_restart,
            supports_primitive_topology_patch_list_restart
        );
        if has_portability_subset {
            log::info!(
                "Vulkan portability subset features: triangle_fans={} image_view_format_reinterpretation={} image_view_format_swizzle={} separate_stencil_mask_ref={} vertex_attribute_access_beyond_stride={}",
                portability_subset_features.triangle_fans != 0,
                portability_subset_features.image_view_format_reinterpretation != 0,
                portability_subset_features.image_view_format_swizzle != 0,
                portability_subset_features.separate_stencil_mask_ref != 0,
                portability_subset_features.vertex_attribute_access_beyond_stride != 0,
            );
        }

        let mut enabled_extensions = Vec::<CString>::new();
        for name in [
            "VK_KHR_swapchain",
            "VK_KHR_portability_subset",
            "VK_KHR_shader_float16_int8",
            "VK_EXT_primitive_topology_list_restart",
            "VK_EXT_extended_dynamic_state",
            "VK_EXT_extended_dynamic_state2",
            "VK_EXT_vertex_attribute_divisor",
            "VK_EXT_shader_demote_to_helper_invocation",
        ] {
            if supported_extensions.contains(name) {
                enabled_extensions.push(CString::new(name).unwrap());
            }
        }
        let enabled_extension_ptrs: Vec<*const std::os::raw::c_char> = enabled_extensions
            .iter()
            .map(|name| name.as_ptr())
            .collect();

        // Upstream builds a full extension list and VkPhysicalDeviceFeatures2 chain here.
        // This keeps the mandatory WSI/portability extensions while the larger feature
        // chain is ported file-by-file.
        let mut enabled_timeline_semaphore_features =
            vk::PhysicalDeviceTimelineSemaphoreFeatures::builder()
                .timeline_semaphore(supports_timeline_semaphore)
                .build();
        let mut enabled_shader_float16_int8_features =
            vk::PhysicalDeviceShaderFloat16Int8Features::builder()
                .shader_float16(supports_shader_float16)
                .shader_int8(supports_shader_int8)
                .build();
        let mut enabled_portability_subset_features = portability_subset_features;
        let mut enabled_primitive_topology_list_restart_features =
            vk::PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT::builder()
                .primitive_topology_list_restart(supports_primitive_topology_list_restart)
                .primitive_topology_patch_list_restart(
                    supports_primitive_topology_patch_list_restart,
                )
                .build();
        let mut enabled_extended_dynamic_state_features =
            vk::PhysicalDeviceExtendedDynamicStateFeaturesEXT::builder()
                .extended_dynamic_state(supports_extended_dynamic_state)
                .build();
        let mut enabled_extended_dynamic_state2_features =
            vk::PhysicalDeviceExtendedDynamicState2FeaturesEXT::builder()
                .extended_dynamic_state2(supports_extended_dynamic_state2)
                .build();
        let mut enabled_vertex_attribute_divisor_features =
            vk::PhysicalDeviceVertexAttributeDivisorFeaturesEXT::builder()
                .vertex_attribute_instance_rate_divisor(supports_vertex_attribute_divisor)
                .build();
        let mut enabled_shader_demote_features =
            vk::PhysicalDeviceShaderDemoteToHelperInvocationFeatures::builder()
                .shader_demote_to_helper_invocation(supports_shader_demote_to_helper_invocation)
                .build();
        let device_create_info = {
            let mut builder = vk::DeviceCreateInfo::builder()
                .push_next(&mut enabled_shader_float16_int8_features);
            if supports_timeline_semaphore {
                builder = builder.push_next(&mut enabled_timeline_semaphore_features);
            }
            if has_portability_subset {
                builder = builder.push_next(&mut enabled_portability_subset_features);
            }
            if has_primitive_topology_list_restart {
                builder = builder.push_next(&mut enabled_primitive_topology_list_restart_features);
            }
            if has_extended_dynamic_state {
                builder = builder.push_next(&mut enabled_extended_dynamic_state_features);
            }
            if has_extended_dynamic_state2 {
                builder = builder.push_next(&mut enabled_extended_dynamic_state2_features);
            }
            if supports_vertex_attribute_divisor {
                builder = builder.push_next(&mut enabled_vertex_attribute_divisor_features);
            }
            if supports_shader_demote_to_helper_invocation {
                builder = builder.push_next(&mut enabled_shader_demote_features);
            }
            builder
                .queue_create_infos(&queue_create_infos)
                .enabled_extension_names(&enabled_extension_ptrs)
                .build()
        };

        let logical = LogicalDevice::create(&instance, physical, &device_create_info)?;
        let graphics_queue = logical.get_queue(graphics_family);
        let present_queue = logical.get_queue(present_family);

        let instance_version = unsafe {
            let props = instance.get_physical_device_properties(physical);
            props.api_version
        };

        let device_name =
            unsafe { CStr::from_ptr(device_properties.device_name.as_ptr()).to_string_lossy() };
        log::info!("Vulkan device: {}", device_name);
        log::info!(
            "Vulkan API version: {}.{}.{}",
            vk::api_version_major(device_properties.api_version),
            vk::api_version_minor(device_properties.api_version),
            vk::api_version_patch(device_properties.api_version),
        );

        let is_virtual = device_properties.device_type == vk::PhysicalDeviceType::VIRTUAL_GPU;
        let is_non_gpu = device_properties.device_type == vk::PhysicalDeviceType::CPU
            || device_properties.device_type == vk::PhysicalDeviceType::OTHER;
        let (device_access_memory, valid_heap_memory) = collect_physical_memory_info(
            &memory_properties,
            memory_budget_properties.as_ref(),
            is_integrated,
        );

        Ok(Self {
            instance,
            physical,
            _dld: ash::Device::clone(&logical.device),
            logical,
            graphics_queue,
            present_queue,
            instance_version,
            graphics_family,
            present_family,
            extensions: DeviceExtensions {
                driver_properties: supported_extensions.contains("VK_KHR_driver_properties"),
                memory_budget: has_memory_budget,
                shader_float16_int8: supported_extensions.contains("VK_KHR_shader_float16_int8"),
                // MoltenVK devices are portability-subset devices. Upstream's generic
                // feature chain enables supported feature structs before device creation;
                // keep the extension visible here for the same ownership boundary.
                primitive_topology_list_restart: has_primitive_topology_list_restart,
                extended_dynamic_state: supports_extended_dynamic_state,
                extended_dynamic_state2: supports_extended_dynamic_state2,
                vertex_attribute_divisor: supports_vertex_attribute_divisor,
                shader_demote_to_helper_invocation: supports_shader_demote_to_helper_invocation,
                swapchain: supported_extensions.contains("VK_KHR_swapchain"),
                ..DeviceExtensions::default()
            },
            device_properties,
            driver_properties,
            subgroup_properties: vk::PhysicalDeviceSubgroupProperties::default(),
            float_controls_properties: vk::PhysicalDeviceFloatControlsProperties::default(),
            push_descriptor_properties: vk::PhysicalDevicePushDescriptorPropertiesKHR::default(),
            subgroup_size_control_properties:
                vk::PhysicalDeviceSubgroupSizeControlProperties::default(),
            device_features,
            shader_float16_supported: supports_shader_float16,
            timeline_semaphore_supported: supports_timeline_semaphore,
            shader_int8_supported: supports_shader_int8,
            primitive_topology_list_restart_supported: supports_primitive_topology_list_restart,
            primitive_topology_patch_list_restart_supported:
                supports_primitive_topology_patch_list_restart,
            is_optimal_astc_supported: false,
            is_blit_depth24_stencil8_supported: false,
            is_blit_depth32_stencil8_supported: false,
            is_warp_potentially_bigger: false,
            is_integrated,
            is_virtual,
            is_non_gpu,
            has_broken_compute: false,
            has_broken_cube_compatibility: false,
            has_broken_parallel_compiling: false,
            has_renderdoc: false,
            has_nsight_graphics: false,
            supports_d24_depth: false,
            cant_blit_msaa: false,
            must_emulate_scaled_formats: false,
            must_emulate_bgr565: false,
            dynamic_state3_blending: false,
            dynamic_state3_enables: false,
            supports_conditional_barriers: false,
            device_access_memory,
            sets_per_pool: 64,
            nvidia_arch: NvidiaArchitecture::AmpereOrNewer,
            supported_extensions,
            loaded_extensions: enabled_extensions
                .iter()
                .map(|name| name.to_string_lossy().into_owned())
                .collect(),
            valid_heap_memory,
            format_properties: HashMap::new(),
            nsight_aftermath_tracker: NsightAftermathTracker::new(),
        })
    }

    // -----------------------------------------------------------------------
    // Accessors — port of `Device` public accessors from vulkan_device.h
    // -----------------------------------------------------------------------

    /// Returns a format supported by the device for the passed requirements.
    ///
    /// Port of `Device::GetSupportedFormat`.
    pub fn get_supported_format(
        &self,
        wanted_format: vk::Format,
        wanted_usage: vk::FormatFeatureFlags,
        format_type: FormatType,
    ) -> vk::Format {
        if self.is_format_supported(wanted_format, wanted_usage, format_type) {
            return wanted_format;
        }
        // Try alternatives
        if let Some(alts) = alternatives::get_format_alternatives(wanted_format) {
            for &alt in alts {
                if self.is_format_supported(alt, wanted_usage, format_type) {
                    return alt;
                }
            }
        }
        wanted_format
    }

    /// Returns true if a format is supported.
    ///
    /// Port of `Device::IsFormatSupported`.
    pub fn is_format_supported(
        &self,
        wanted_format: vk::Format,
        wanted_usage: vk::FormatFeatureFlags,
        format_type: FormatType,
    ) -> bool {
        let props = self.get_format_properties(wanted_format);
        let supported = match format_type {
            FormatType::Linear => props.linear_tiling_features,
            FormatType::Optimal => props.optimal_tiling_features,
            FormatType::Buffer => props.buffer_features,
        };
        (supported & wanted_usage) == wanted_usage
    }

    /// Gets the format properties for a format.
    ///
    /// Port-facing accessor for upstream `vk::PhysicalDevice::GetFormatProperties`.
    pub fn format_properties(&self, format: vk::Format) -> vk::FormatProperties {
        self.get_format_properties(format)
    }

    /// Gets the format properties for a format, caching results.
    fn get_format_properties(&self, format: vk::Format) -> vk::FormatProperties {
        if let Some(&cached) = self.format_properties.get(&format) {
            return cached;
        }
        unsafe {
            self.instance
                .get_physical_device_format_properties(self.physical, format)
        }
    }

    /// Reports a device loss.
    ///
    /// Port of `Device::ReportLoss`.
    pub fn report_loss(&self) {
        log::error!("GPU lost");
    }

    /// Reports a shader to Nsight Aftermath.
    ///
    /// Port of `Device::SaveShader`.
    pub fn save_shader(&self, spirv: &[u32]) {
        self.nsight_aftermath_tracker.save_shader(spirv);
    }

    /// Returns the name of the VkDriverId reported from Vulkan.
    ///
    /// Port of `Device::GetDriverName`.
    pub fn get_driver_name(&self) -> String {
        if let Some(name) = driver_name_from_id(self.driver_properties.driver_id) {
            return name.to_string();
        }
        self.get_vendor_name()
    }

    /// Returns the vendor name reported from Vulkan.
    ///
    /// Port of `Device::GetVendorName`.
    pub fn get_vendor_name(&self) -> String {
        let name = unsafe { CStr::from_ptr(self.driver_properties.driver_name.as_ptr()) };
        name.to_string_lossy().into_owned()
    }

    /// Returns the logical ash device.
    pub fn get_logical(&self) -> &ash::Device {
        &self.logical.device
    }

    /// Returns the physical device.
    pub fn get_physical(&self) -> vk::PhysicalDevice {
        self.physical
    }

    /// Returns the main graphics queue.
    pub fn get_graphics_queue(&self) -> vk::Queue {
        self.graphics_queue
    }

    /// Returns the main present queue.
    pub fn get_present_queue(&self) -> vk::Queue {
        self.present_queue
    }

    /// Returns the main graphics queue family index.
    pub fn get_graphics_family(&self) -> u32 {
        self.graphics_family
    }

    /// Returns the main present queue family index.
    pub fn get_present_family(&self) -> u32 {
        self.present_family
    }

    /// Returns the current Vulkan API version.
    pub fn api_version(&self) -> u32 {
        self.device_properties.api_version
    }

    /// Returns the current driver version.
    pub fn get_driver_version(&self) -> u32 {
        self.device_properties.driver_version
    }

    /// Returns the device name.
    pub fn get_model_name(&self) -> String {
        let name = unsafe { CStr::from_ptr(self.device_properties.device_name.as_ptr()) };
        name.to_string_lossy().into_owned()
    }

    /// Returns the driver ID.
    pub fn get_driver_id(&self) -> vk::DriverId {
        self.driver_properties.driver_id
    }

    /// Returns true if clocks should be boosted.
    ///
    /// Port of `Device::ShouldBoostClocks`.
    pub fn should_boost_clocks(&self) -> bool {
        self.driver_properties.driver_id == vk::DriverId::NVIDIA_PROPRIETARY
    }

    /// Returns uniform buffer alignment requirement.
    pub fn get_uniform_buffer_alignment(&self) -> vk::DeviceSize {
        self.device_properties
            .limits
            .min_uniform_buffer_offset_alignment
    }

    /// Returns storage buffer alignment requirement.
    pub fn get_storage_buffer_alignment(&self) -> vk::DeviceSize {
        self.device_properties
            .limits
            .min_storage_buffer_offset_alignment
    }

    /// Returns the maximum range for storage buffers.
    pub fn get_max_storage_buffer_range(&self) -> vk::DeviceSize {
        self.device_properties.limits.max_storage_buffer_range as u64
    }

    /// Returns the maximum size for push constants.
    pub fn get_max_push_constants_size(&self) -> vk::DeviceSize {
        self.device_properties.limits.max_push_constants_size as u64
    }

    /// Returns the maximum size for shared memory.
    pub fn get_max_compute_shared_memory_size(&self) -> u32 {
        self.device_properties.limits.max_compute_shared_memory_size
    }

    /// Returns true if ASTC is natively supported.
    pub fn is_optimal_astc_supported(&self) -> bool {
        self.device_features.texture_compression_astc_ldr != 0
    }

    /// Returns true if BCn is natively supported.
    pub fn is_optimal_bcn_supported(&self) -> bool {
        self.device_features.texture_compression_bc != 0
    }

    /// Returns true if the device supports float64 natively.
    pub fn is_float64_supported(&self) -> bool {
        self.device_features.shader_float64 != 0
    }

    /// Returns true if the device supports float16 natively.
    ///
    /// Port of `Device::IsFloat16Supported`.
    pub fn is_float16_supported(&self) -> bool {
        self.shader_float16_supported
    }

    /// Returns true if timeline semaphores are supported and enabled.
    pub fn is_timeline_semaphore_supported(&self) -> bool {
        self.timeline_semaphore_supported
    }

    /// Returns true if the device supports VK_EXT_primitive_topology_list_restart.
    ///
    /// Port of `Device::IsTopologyListPrimitiveRestartSupported`.
    pub fn is_topology_list_primitive_restart_supported(&self) -> bool {
        self.primitive_topology_list_restart_supported
    }

    /// Returns true if the device supports patch-list primitive restart.
    ///
    /// Port of `Device::IsPatchListPrimitiveRestartSupported`.
    pub fn is_patch_list_primitive_restart_supported(&self) -> bool {
        self.primitive_topology_patch_list_restart_supported
    }

    /// Returns true if the device supports int64 natively.
    pub fn is_shader_int64_supported(&self) -> bool {
        self.device_features.shader_int64 != 0
    }

    /// Returns true if the device supports int16 natively.
    pub fn is_shader_int16_supported(&self) -> bool {
        self.device_features.shader_int16 != 0
    }

    /// Returns true if depth bounds is supported.
    pub fn is_depth_bounds_supported(&self) -> bool {
        self.device_features.depth_bounds != 0
    }

    /// Returns true if the device supports binding multisample images as storage images.
    pub fn is_storage_image_multisample_supported(&self) -> bool {
        self.device_features.shader_storage_image_multisample != 0
    }

    /// Returns true if formatless image load is supported.
    pub fn is_formatless_image_load_supported(&self) -> bool {
        self.device_features
            .shader_storage_image_read_without_format
            != 0
    }

    /// Returns the maximum number of push descriptors.
    pub fn max_push_descriptors(&self) -> u32 {
        self.push_descriptor_properties.max_push_descriptors
    }

    /// Returns true when a known debugging tool is attached.
    pub fn has_debugging_tool_attached(&self) -> bool {
        self.has_renderdoc || self.has_nsight_graphics
    }

    /// Returns true if compute pipelines can cause crashing.
    pub fn has_broken_compute(&self) -> bool {
        self.has_broken_compute
    }

    /// Returns true if the device is an NVIDIA GPU.
    pub fn is_nvidia(&self) -> bool {
        self.driver_properties.driver_id == vk::DriverId::NVIDIA_PROPRIETARY
    }

    /// Returns the NVIDIA architecture classification.
    pub fn get_nvidia_arch(&self) -> NvidiaArchitecture {
        self.nvidia_arch
    }

    /// Returns the minimum supported SPIR-V version.
    ///
    /// Port of `Device::SupportedSpirvVersion`.
    pub fn supported_spirv_version(&self) -> u32 {
        if self.instance_version >= vk::API_VERSION_1_3 {
            0x0001_0600
        } else if self.extensions.spirv_1_4 {
            0x0001_0400
        } else {
            0x0001_0300
        }
    }

    /// Checks if a driver version has broken compute.
    ///
    /// Port of `Device::CheckBrokenCompute`.
    pub const fn check_broken_compute(driver_id: vk::DriverId, driver_version: u32) -> bool {
        if driver_id.as_raw() == vk::DriverId::INTEL_PROPRIETARY_WINDOWS.as_raw() {
            let major = vk::api_version_major(driver_version);
            let minor = vk::api_version_minor(driver_version);
            let patch = vk::api_version_patch(driver_version);
            if major == 0 && minor == 405 && patch < 286 {
                return true;
            }
        }
        false
    }

    /// Returns the available extensions set.
    pub fn get_available_extensions(&self) -> &BTreeSet<String> {
        &self.supported_extensions
    }

    /// Returns the device local memory size.
    pub fn get_device_local_memory(&self) -> u64 {
        self.device_access_memory
    }

    /// Returns true if memory budget reporting is supported.
    pub fn can_report_memory_usage(&self) -> bool {
        self.extensions.memory_budget
    }

    /// Returns currently used memory across the heaps tracked by
    /// `CollectPhysicalMemoryInfo`.
    ///
    /// Port of `Device::GetDeviceMemoryUsage`.
    pub fn get_device_memory_usage(&self) -> u64 {
        if !self.extensions.memory_budget {
            return 0;
        }

        let (_, Some(budget)) = physical_memory_properties(&self.instance, self.physical, true)
        else {
            return 0;
        };
        device_memory_usage_from_budget(&budget, &self.valid_heap_memory)
    }

    /// Returns the number of descriptor sets per pool.
    pub fn get_sets_per_pool(&self) -> u32 {
        self.sets_per_pool
    }

    /// Returns the max vertex input attributes.
    pub fn get_max_vertex_input_attributes(&self) -> u32 {
        self.device_properties.limits.max_vertex_input_attributes
    }

    /// Returns the max vertex input bindings.
    pub fn get_max_vertex_input_bindings(&self) -> u32 {
        self.device_properties.limits.max_vertex_input_bindings
    }

    /// Returns the max viewports.
    pub fn get_max_viewports(&self) -> u32 {
        self.device_properties.limits.max_viewports
    }

    /// Returns the max user clip distances.
    pub fn get_max_user_clip_distances(&self) -> u32 {
        self.device_properties.limits.max_clip_distances
    }

    /// Returns true if multi-viewport is supported.
    pub fn supports_multi_viewport(&self) -> bool {
        self.device_features.multi_viewport != 0
    }

    // -----------------------------------------------------------------------
    // Extension query helpers — port of the Is*Supported accessors
    // -----------------------------------------------------------------------

    pub fn is_nv_viewport_swizzle_supported(&self) -> bool {
        self.extensions.viewport_swizzle
    }

    pub fn is_nv_viewport_array2_supported(&self) -> bool {
        self.extensions.viewport_array2
    }

    pub fn is_nv_geometry_shader_passthrough_supported(&self) -> bool {
        self.extensions.geometry_shader_passthrough
    }

    pub fn is_khr_push_descriptor_supported(&self) -> bool {
        self.extensions.push_descriptor
    }

    pub fn is_ext_transform_feedback_supported(&self) -> bool {
        self.extensions.transform_feedback
    }

    pub fn is_ext_custom_border_color_supported(&self) -> bool {
        self.extensions.custom_border_color
    }

    pub fn is_ext_extended_dynamic_state_supported(&self) -> bool {
        self.extensions.extended_dynamic_state
    }

    pub fn is_ext_extended_dynamic_state2_supported(&self) -> bool {
        self.extensions.extended_dynamic_state2
    }

    pub fn is_ext_extended_dynamic_state3_supported(&self) -> bool {
        self.extensions.extended_dynamic_state3
    }

    pub fn is_ext_vertex_input_dynamic_state_supported(&self) -> bool {
        self.extensions.vertex_input_dynamic_state
    }

    pub fn is_ext_depth_clip_control_supported(&self) -> bool {
        self.extensions.depth_clip_control
    }

    pub fn is_ext_depth_bias_control_supported(&self) -> bool {
        self.extensions.depth_bias_control
    }

    pub fn is_ext_index_type_uint8_supported(&self) -> bool {
        self.extensions.index_type_uint8
    }

    pub fn is_ext_sampler_filter_minmax_supported(&self) -> bool {
        self.extensions.sampler_filter_minmax
    }

    pub fn is_ext_shader_stencil_export_supported(&self) -> bool {
        self.extensions.shader_stencil_export
    }

    pub fn is_ext_depth_range_unrestricted_supported(&self) -> bool {
        self.extensions.depth_range_unrestricted
    }

    pub fn is_ext_shader_viewport_index_layer_supported(&self) -> bool {
        self.extensions.shader_viewport_index_layer
    }

    pub fn is_ext_subgroup_size_control_supported(&self) -> bool {
        self.extensions.subgroup_size_control
    }

    pub fn is_ext_conservative_rasterization_supported(&self) -> bool {
        self.extensions.conservative_rasterization
    }

    pub fn is_ext_provoking_vertex_supported(&self) -> bool {
        self.extensions.provoking_vertex
    }

    pub fn is_ext_shader_atomic_int64_supported(&self) -> bool {
        self.extensions.shader_atomic_int64
    }

    pub fn is_ext_conditional_rendering(&self) -> bool {
        self.extensions.conditional_rendering
    }

    pub fn is_ext_line_rasterization_supported(&self) -> bool {
        self.extensions.line_rasterization
    }

    pub fn is_ext_vertex_attribute_divisor_supported(&self) -> bool {
        self.extensions.vertex_attribute_divisor
    }

    pub fn is_ext_shader_demote_to_helper_invocation_supported(&self) -> bool {
        self.extensions.shader_demote_to_helper_invocation
    }

    pub fn is_khr_shader_float_controls_supported(&self) -> bool {
        self.extensions.shader_float_controls
    }

    pub fn is_khr_workgroup_memory_explicit_layout_supported(&self) -> bool {
        self.extensions.workgroup_memory_explicit_layout
    }

    pub fn is_khr_image_format_list_supported(&self) -> bool {
        self.extensions.image_format_list || self.instance_version >= vk::API_VERSION_1_2
    }

    pub fn is_khr_swapchain_mutable_format_enabled(&self) -> bool {
        self.extensions.swapchain_mutable_format
    }

    pub fn is_khr_pipeline_executable_properties_enabled(&self) -> bool {
        self.extensions.pipeline_executable_properties
    }

    pub fn is_khr_uniform_buffer_standard_layout_supported(&self) -> bool {
        self.extensions.uniform_buffer_standard_layout
    }

    pub fn is_blit_depth24_stencil8_supported(&self) -> bool {
        self.is_blit_depth24_stencil8_supported
    }

    pub fn is_blit_depth32_stencil8_supported(&self) -> bool {
        self.is_blit_depth32_stencil8_supported
    }

    pub fn is_warp_size_potentially_bigger_than_guest(&self) -> bool {
        self.is_warp_potentially_bigger
    }

    pub fn supports_d24_depth_buffer(&self) -> bool {
        self.supports_d24_depth
    }

    pub fn is_ext_extended_dynamic_state3_blending_supported(&self) -> bool {
        self.dynamic_state3_blending
    }

    pub fn is_ext_extended_dynamic_state3_enables_supported(&self) -> bool {
        self.dynamic_state3_enables
    }

    pub fn supports_conditional_barriers(&self) -> bool {
        self.supports_conditional_barriers
    }

    pub fn cant_blit_msaa(&self) -> bool {
        self.cant_blit_msaa
    }

    pub fn must_emulate_scaled_formats(&self) -> bool {
        self.must_emulate_scaled_formats
    }

    pub fn must_emulate_bgr565(&self) -> bool {
        self.must_emulate_bgr565
    }
}

fn physical_memory_properties(
    instance: &ash::Instance,
    physical: vk::PhysicalDevice,
    has_memory_budget: bool,
) -> (
    vk::PhysicalDeviceMemoryProperties,
    Option<vk::PhysicalDeviceMemoryBudgetPropertiesEXT>,
) {
    if has_memory_budget {
        let mut budget = vk::PhysicalDeviceMemoryBudgetPropertiesEXT::default();
        let mut properties2 = vk::PhysicalDeviceMemoryProperties2::builder()
            .push_next(&mut budget)
            .build();
        unsafe {
            instance.get_physical_device_memory_properties2(physical, &mut properties2);
        }
        return (properties2.memory_properties, Some(budget));
    }

    let properties = unsafe { instance.get_physical_device_memory_properties(physical) };
    (properties, None)
}

/// Query the memory information used by upstream `Device` memory accessors.
pub fn query_device_memory_info(
    instance: &ash::Instance,
    physical: vk::PhysicalDevice,
) -> DeviceMemoryInfo {
    let available_extensions = unsafe {
        instance
            .enumerate_device_extension_properties(physical)
            .unwrap_or_default()
    };
    let supported_extensions: BTreeSet<String> = available_extensions
        .iter()
        .map(|ext| {
            let name = unsafe { CStr::from_ptr(ext.extension_name.as_ptr()) };
            name.to_string_lossy().into_owned()
        })
        .collect();
    let has_memory_budget = supported_extensions.contains("VK_EXT_memory_budget");
    let device_properties = unsafe { instance.get_physical_device_properties(physical) };
    let is_integrated = device_properties.device_type == vk::PhysicalDeviceType::INTEGRATED_GPU;
    let (memory_properties, memory_budget) =
        physical_memory_properties(instance, physical, has_memory_budget);
    let (device_local_memory, valid_heap_memory) =
        collect_physical_memory_info(&memory_properties, memory_budget.as_ref(), is_integrated);

    DeviceMemoryInfo {
        device_local_memory,
        can_report_memory_usage: has_memory_budget,
        valid_heap_memory,
    }
}

fn collect_physical_memory_info(
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    memory_budget: Option<&vk::PhysicalDeviceMemoryBudgetPropertiesEXT>,
    is_integrated: bool,
) -> (u64, Vec<usize>) {
    let mut device_access_memory = 0_u64;
    let mut device_initial_usage = 0_u64;
    let mut local_memory = 0_u64;
    let mut valid_heap_memory = Vec::new();

    for element in 0..memory_properties.memory_heap_count as usize {
        let heap = memory_properties.memory_heaps[element];
        let is_heap_local = heap.flags.contains(vk::MemoryHeapFlags::DEVICE_LOCAL);
        if !is_integrated && !is_heap_local {
            continue;
        }

        valid_heap_memory.push(element);
        if is_heap_local {
            local_memory = local_memory.saturating_add(heap.size);
        }

        if let Some(budget) = memory_budget {
            device_initial_usage = device_initial_usage.saturating_add(budget.heap_usage[element]);
            device_access_memory = device_access_memory.saturating_add(budget.heap_budget[element]);
        } else {
            device_access_memory = device_access_memory.saturating_add(heap.size);
        }
    }

    if !is_integrated {
        let reserve_memory = std::cmp::min(device_access_memory / 8, ONE_GIB);
        device_access_memory = device_access_memory.saturating_sub(reserve_memory);

        if *common::settings::values().vram_usage_mode.get_value()
            != common::settings::VramUsageMode::Aggressive
        {
            let normal_memory = 6 * ONE_GIB;
            let scaler_memory =
                ONE_GIB * common::settings::values().resolution_info.scale_up_u32(1) as u64;
            device_access_memory =
                std::cmp::min(device_access_memory, normal_memory + scaler_memory);
        }

        return (device_access_memory, valid_heap_memory);
    }

    let available_memory = device_access_memory.wrapping_sub(device_initial_usage) as i64;
    let upper = std::cmp::min(available_memory - 8 * ONE_GIB as i64, 4 * ONE_GIB as i64);
    let lower = std::cmp::min(local_memory as i64, 4 * ONE_GIB as i64);
    (std::cmp::max(upper, lower) as u64, valid_heap_memory)
}

fn device_memory_usage_from_budget(
    memory_budget: &vk::PhysicalDeviceMemoryBudgetPropertiesEXT,
    valid_heap_memory: &[usize],
) -> u64 {
    valid_heap_memory
        .iter()
        .map(|&heap| memory_budget.heap_usage[heap])
        .sum()
}

/// Query current memory usage with the same heap filter as `Device`.
pub fn query_device_memory_usage(
    instance: &ash::Instance,
    physical: vk::PhysicalDevice,
    memory_info: &DeviceMemoryInfo,
) -> u64 {
    if !memory_info.can_report_memory_usage {
        return 0;
    }
    let (_, Some(budget)) = physical_memory_properties(instance, physical, true) else {
        return 0;
    };
    device_memory_usage_from_budget(&budget, &memory_info.valid_heap_memory)
}

fn driver_name_from_id(driver_id: vk::DriverId) -> Option<&'static str> {
    match driver_id {
        vk::DriverId::AMD_PROPRIETARY => Some("AMD"),
        vk::DriverId::AMD_OPEN_SOURCE => Some("AMDVLK"),
        vk::DriverId::MESA_RADV => Some("RADV"),
        vk::DriverId::NVIDIA_PROPRIETARY => Some("NVIDIA"),
        vk::DriverId::INTEL_PROPRIETARY_WINDOWS => Some("Intel"),
        vk::DriverId::INTEL_OPEN_SOURCE_MESA => Some("ANV"),
        vk::DriverId::IMAGINATION_PROPRIETARY => Some("PowerVR"),
        vk::DriverId::QUALCOMM_PROPRIETARY => Some("Qualcomm"),
        vk::DriverId::ARM_PROPRIETARY => Some("Mali"),
        vk::DriverId::SAMSUNG_PROPRIETARY => Some("Xclipse"),
        vk::DriverId::GOOGLE_SWIFTSHADER => Some("SwiftShader"),
        vk::DriverId::BROADCOM_PROPRIETARY => Some("Broadcom"),
        vk::DriverId::MESA_LLVMPIPE => Some("Lavapipe"),
        vk::DriverId::MOLTENVK => Some("MoltenVK"),
        vk::DriverId::VERISILICON_PROPRIETARY => Some("Vivante"),
        vk::DriverId::MESA_TURNIP => Some("Turnip"),
        vk::DriverId::MESA_V3DV => Some("V3DV"),
        vk::DriverId::MESA_PANVK => Some("PanVK"),
        vk::DriverId::MESA_VENUS => Some("Venus"),
        vk::DriverId::MESA_DOZEN => Some("Dozen"),
        vk::DriverId::MESA_NVK => Some("NVK"),
        vk::DriverId::IMAGINATION_OPEN_SOURCE_MESA => Some("PVR"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collect_physical_memory_info_sums_device_local_heaps_for_discrete_gpus() {
        let mut properties = vk::PhysicalDeviceMemoryProperties::default();
        properties.memory_heap_count = 3;
        properties.memory_heaps[0].size = 2 * 1024 * 1024;
        properties.memory_heaps[0].flags = vk::MemoryHeapFlags::DEVICE_LOCAL;
        properties.memory_heaps[1].size = 4 * 1024 * 1024;
        properties.memory_heaps[1].flags = vk::MemoryHeapFlags::empty();
        properties.memory_heaps[2].size = 8 * 1024 * 1024;
        properties.memory_heaps[2].flags = vk::MemoryHeapFlags::DEVICE_LOCAL;

        let (memory, heaps) = collect_physical_memory_info(&properties, None, false);

        assert_eq!(memory, 10 * 1024 * 1024 - (10 * 1024 * 1024 / 8));
        assert_eq!(heaps, vec![0, 2]);
    }

    #[test]
    fn collect_physical_memory_info_caps_discrete_gpu_memory_like_upstream() {
        let mut properties = vk::PhysicalDeviceMemoryProperties::default();
        properties.memory_heap_count = 1;
        properties.memory_heaps[0].size = 16 * ONE_GIB;
        properties.memory_heaps[0].flags = vk::MemoryHeapFlags::DEVICE_LOCAL;

        let (memory, heaps) = collect_physical_memory_info(&properties, None, false);

        assert_eq!(memory, 7 * ONE_GIB);
        assert_eq!(heaps, vec![0]);
    }

    #[test]
    fn collect_physical_memory_info_uses_budget_when_available() {
        let mut properties = vk::PhysicalDeviceMemoryProperties::default();
        properties.memory_heap_count = 1;
        properties.memory_heaps[0].size = 16 * ONE_GIB;
        properties.memory_heaps[0].flags = vk::MemoryHeapFlags::DEVICE_LOCAL;
        let mut budget = vk::PhysicalDeviceMemoryBudgetPropertiesEXT::default();
        budget.heap_budget[0] = 8 * ONE_GIB;
        budget.heap_usage[0] = ONE_GIB;

        let (memory, heaps) = collect_physical_memory_info(&properties, Some(&budget), false);

        assert_eq!(memory, 7 * ONE_GIB);
        assert_eq!(heaps, vec![0]);
    }

    #[test]
    fn device_memory_usage_from_budget_sums_valid_heaps() {
        let mut budget = vk::PhysicalDeviceMemoryBudgetPropertiesEXT::default();
        budget.heap_usage[0] = ONE_GIB;
        budget.heap_usage[1] = 2 * ONE_GIB;
        budget.heap_usage[2] = 4 * ONE_GIB;

        assert_eq!(
            device_memory_usage_from_budget(&budget, &[0, 2]),
            5 * ONE_GIB
        );
    }

    #[test]
    fn driver_name_from_id_matches_upstream_names() {
        assert_eq!(
            driver_name_from_id(vk::DriverId::AMD_PROPRIETARY),
            Some("AMD")
        );
        assert_eq!(driver_name_from_id(vk::DriverId::MESA_RADV), Some("RADV"));
        assert_eq!(
            driver_name_from_id(vk::DriverId::NVIDIA_PROPRIETARY),
            Some("NVIDIA")
        );
        assert_eq!(
            driver_name_from_id(vk::DriverId::MOLTENVK),
            Some("MoltenVK")
        );
        assert_eq!(driver_name_from_id(vk::DriverId::MESA_NVK), Some("NVK"));
        assert_eq!(driver_name_from_id(vk::DriverId::from_raw(-1)), None);
    }
}
