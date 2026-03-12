// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Banked descriptor set allocation pool.
//!
//! Ref: zuyu `vk_descriptor_pool.h` — manages multiple VkDescriptorPools to
//! handle allocation failures by creating new pools as needed.

use ash::vk;
use log::debug;

/// Allocates VkDescriptorSets from pooled VkDescriptorPools.
///
/// Ref: zuyu DescriptorPool — when the current pool runs out of space,
/// a new pool is created. Old pools are recycled at the start of each frame.
pub struct DescriptorPool {
    device: ash::Device,
    pools: Vec<vk::DescriptorPool>,
    current_pool: usize,
}

impl DescriptorPool {
    /// Maximum sets per pool.
    const MAX_SETS_PER_POOL: u32 = 64;

    pub fn new(device: ash::Device) -> Result<Self, vk::Result> {
        let pool = Self::create_pool(&device)?;
        Ok(Self {
            device,
            pools: vec![pool],
            current_pool: 0,
        })
    }

    /// Allocate a descriptor set from the current pool.
    ///
    /// If allocation fails, creates a new pool and retries.
    pub fn allocate(
        &mut self,
        layout: vk::DescriptorSetLayout,
    ) -> Result<vk::DescriptorSet, vk::Result> {
        let alloc_info = vk::DescriptorSetAllocateInfo::builder()
            .descriptor_pool(self.pools[self.current_pool])
            .set_layouts(std::slice::from_ref(&layout))
            .build();

        match unsafe { self.device.allocate_descriptor_sets(&alloc_info) } {
            Ok(sets) => Ok(sets[0]),
            Err(_) => {
                // Pool is full — create a new one
                let new_pool = Self::create_pool(&self.device)?;
                self.pools.push(new_pool);
                self.current_pool = self.pools.len() - 1;
                debug!(
                    "DescriptorPool: created new pool (total: {})",
                    self.pools.len()
                );

                let alloc_info = vk::DescriptorSetAllocateInfo::builder()
                    .descriptor_pool(self.pools[self.current_pool])
                    .set_layouts(std::slice::from_ref(&layout))
                    .build();
                let sets = unsafe { self.device.allocate_descriptor_sets(&alloc_info)? };
                Ok(sets[0])
            }
        }
    }

    /// Reset all pools for the next frame.
    pub fn reset_pools(&mut self) {
        for pool in &self.pools {
            unsafe {
                self.device
                    .reset_descriptor_pool(*pool, vk::DescriptorPoolResetFlags::empty())
                    .ok();
            }
        }
        self.current_pool = 0;
    }

    fn create_pool(device: &ash::Device) -> Result<vk::DescriptorPool, vk::Result> {
        let pool_sizes = [
            vk::DescriptorPoolSize {
                ty: vk::DescriptorType::UNIFORM_BUFFER,
                descriptor_count: 128,
            },
            vk::DescriptorPoolSize {
                ty: vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
                descriptor_count: 128,
            },
            vk::DescriptorPoolSize {
                ty: vk::DescriptorType::STORAGE_BUFFER,
                descriptor_count: 64,
            },
        ];

        let pool_info = vk::DescriptorPoolCreateInfo::builder()
            .pool_sizes(&pool_sizes)
            .max_sets(Self::MAX_SETS_PER_POOL)
            .build();

        unsafe { device.create_descriptor_pool(&pool_info, None) }
    }
}

impl Drop for DescriptorPool {
    fn drop(&mut self) {
        for pool in &self.pools {
            unsafe {
                self.device.destroy_descriptor_pool(*pool, None);
            }
        }
    }
}
