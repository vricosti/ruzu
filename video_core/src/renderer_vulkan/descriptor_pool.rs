// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `vk_descriptor_pool.h` / `vk_descriptor_pool.cpp`.
//!
//! Banked descriptor set allocation pool. Manages multiple VkDescriptorPools
//! organized into banks by descriptor type requirements.

use std::sync::RwLock;

use ash::vk;
use log::debug;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Prefer small grow rates to avoid saturating the descriptor pool with
/// barely used pipelines.
///
/// Port of `SETS_GROW_RATE` from `vk_descriptor_pool.cpp`.
const SETS_GROW_RATE: usize = 16;

/// Score difference threshold for bank reuse.
///
/// Port of `SCORE_THRESHOLD` from `vk_descriptor_pool.cpp`.
const SCORE_THRESHOLD: i32 = 3;

// ---------------------------------------------------------------------------
// DescriptorBankInfo
// ---------------------------------------------------------------------------

/// Descriptor type counts for a descriptor bank.
///
/// Port of `DescriptorBankInfo` from `vk_descriptor_pool.h`.
#[derive(Debug, Clone, Copy, Default)]
pub struct DescriptorBankInfo {
    /// Number of uniform buffer descriptors.
    pub uniform_buffers: u32,
    /// Number of storage buffer descriptors.
    pub storage_buffers: u32,
    /// Number of texture buffer descriptors.
    pub texture_buffers: u32,
    /// Number of image buffer descriptors.
    pub image_buffers: u32,
    /// Number of texture descriptors.
    pub textures: u32,
    /// Number of image descriptors.
    pub images: u32,
    /// Total number of descriptors (score).
    pub score: i32,
}

impl DescriptorBankInfo {
    /// Port of `DescriptorBankInfo::IsSuperset`.
    ///
    /// Returns true if this bank can satisfy the given subset's requirements.
    pub fn is_superset(&self, subset: &DescriptorBankInfo) -> bool {
        self.uniform_buffers >= subset.uniform_buffers
            && self.storage_buffers >= subset.storage_buffers
            && self.texture_buffers >= subset.texture_buffers
            && self.image_buffers >= subset.image_buffers
            && self.textures >= subset.textures
            && self.images >= subset.image_buffers
    }
}

// ---------------------------------------------------------------------------
// DescriptorBank
// ---------------------------------------------------------------------------

/// A bank of descriptor pools with a specific descriptor type configuration.
///
/// Port of `DescriptorBank` from `vk_descriptor_pool.cpp`.
struct DescriptorBank {
    info: DescriptorBankInfo,
    pools: Vec<vk::DescriptorPool>,
}

// ---------------------------------------------------------------------------
// Helper: AllocatePool
// ---------------------------------------------------------------------------

/// Allocate a new VkDescriptorPool for the given bank.
///
/// Port of `AllocatePool` from `vk_descriptor_pool.cpp`.
fn allocate_pool(
    device: &ash::Device,
    bank: &mut DescriptorBank,
    sets_per_pool: u32,
) -> Result<(), vk::Result> {
    let mut pool_sizes = Vec::with_capacity(6);
    let info = &bank.info;

    let add = |pool_sizes: &mut Vec<vk::DescriptorPoolSize>, ty: vk::DescriptorType, count: u32| {
        if count > 0 {
            pool_sizes.push(vk::DescriptorPoolSize {
                ty,
                descriptor_count: count * sets_per_pool,
            });
        }
    };

    add(
        &mut pool_sizes,
        vk::DescriptorType::UNIFORM_BUFFER,
        info.uniform_buffers,
    );
    add(
        &mut pool_sizes,
        vk::DescriptorType::STORAGE_BUFFER,
        info.storage_buffers,
    );
    add(
        &mut pool_sizes,
        vk::DescriptorType::UNIFORM_TEXEL_BUFFER,
        info.texture_buffers,
    );
    add(
        &mut pool_sizes,
        vk::DescriptorType::STORAGE_TEXEL_BUFFER,
        info.image_buffers,
    );
    add(
        &mut pool_sizes,
        vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
        info.textures,
    );
    add(
        &mut pool_sizes,
        vk::DescriptorType::STORAGE_IMAGE,
        info.images,
    );

    // If no descriptors are needed, add a dummy entry
    if pool_sizes.is_empty() {
        pool_sizes.push(vk::DescriptorPoolSize {
            ty: vk::DescriptorType::UNIFORM_BUFFER,
            descriptor_count: sets_per_pool,
        });
    }

    let pool_ci = vk::DescriptorPoolCreateInfo::builder()
        .max_sets(sets_per_pool)
        .pool_sizes(&pool_sizes)
        .build();

    let pool = unsafe { device.create_descriptor_pool(&pool_ci, None)? };
    bank.pools.push(pool);
    Ok(())
}

// ---------------------------------------------------------------------------
// DescriptorPool
// ---------------------------------------------------------------------------

/// Banked descriptor pool manager.
///
/// Port of `DescriptorPool` from `vk_descriptor_pool.h`.
///
/// Manages multiple descriptor banks, each containing VkDescriptorPools
/// configured for specific descriptor type requirements. Banks are reused
/// when their descriptor counts are close enough (within SCORE_THRESHOLD).
pub struct DescriptorPool {
    device: ash::Device,
    sets_per_pool: u32,
    banks_lock: RwLock<BanksState>,
}

struct BanksState {
    bank_infos: Vec<DescriptorBankInfo>,
    banks: Vec<DescriptorBank>,
}

impl DescriptorPool {
    /// Port of `DescriptorPool::DescriptorPool`.
    pub fn new(device: ash::Device, sets_per_pool: u32) -> Self {
        DescriptorPool {
            device,
            sets_per_pool,
            banks_lock: RwLock::new(BanksState {
                bank_infos: Vec::new(),
                banks: Vec::new(),
            }),
        }
    }

    /// Allocate a descriptor set for the given layout and bank requirements.
    ///
    /// Port of `DescriptorAllocator::Commit` / `AllocateDescriptors`.
    pub fn allocate(
        &self,
        layout: vk::DescriptorSetLayout,
        info: &DescriptorBankInfo,
    ) -> Result<vk::DescriptorSet, vk::Result> {
        let bank_pool = self.get_or_create_bank_pool(info)?;

        let layouts = [layout];
        let alloc_info = vk::DescriptorSetAllocateInfo::builder()
            .descriptor_pool(bank_pool)
            .set_layouts(&layouts)
            .build();

        match unsafe { self.device.allocate_descriptor_sets(&alloc_info) } {
            Ok(sets) => Ok(sets[0]),
            Err(vk::Result::ERROR_OUT_OF_POOL_MEMORY) => {
                // Pool is full — create a new pool in the bank and retry
                let bank_pool = self.grow_bank(info)?;
                let alloc_info = vk::DescriptorSetAllocateInfo::builder()
                    .descriptor_pool(bank_pool)
                    .set_layouts(&layouts)
                    .build();
                let sets = unsafe { self.device.allocate_descriptor_sets(&alloc_info)? };
                Ok(sets[0])
            }
            Err(e) => Err(e),
        }
    }

    /// Allocate multiple descriptor sets at once.
    pub fn allocate_many(
        &self,
        layout: vk::DescriptorSetLayout,
        info: &DescriptorBankInfo,
        count: usize,
    ) -> Result<Vec<vk::DescriptorSet>, vk::Result> {
        let bank_pool = self.get_or_create_bank_pool(info)?;
        let layouts: Vec<_> = std::iter::repeat(layout).take(count).collect();

        let alloc_info = vk::DescriptorSetAllocateInfo::builder()
            .descriptor_pool(bank_pool)
            .set_layouts(&layouts)
            .build();

        match unsafe { self.device.allocate_descriptor_sets(&alloc_info) } {
            Ok(sets) => Ok(sets),
            Err(vk::Result::ERROR_OUT_OF_POOL_MEMORY) => {
                let bank_pool = self.grow_bank(info)?;
                let alloc_info = vk::DescriptorSetAllocateInfo::builder()
                    .descriptor_pool(bank_pool)
                    .set_layouts(&layouts)
                    .build();
                Ok(unsafe { self.device.allocate_descriptor_sets(&alloc_info)? })
            }
            Err(e) => Err(e),
        }
    }

    /// Reset all pools for the next frame.
    pub fn reset_pools(&self) {
        let state = self.banks_lock.write().unwrap();
        for bank in &state.banks {
            for pool in &bank.pools {
                unsafe {
                    self.device
                        .reset_descriptor_pool(*pool, vk::DescriptorPoolResetFlags::empty())
                        .ok();
                }
            }
        }
    }

    /// Find or create a bank matching the requirements, return its last pool.
    ///
    /// Port of `DescriptorPool::Bank`.
    fn get_or_create_bank_pool(
        &self,
        reqs: &DescriptorBankInfo,
    ) -> Result<vk::DescriptorPool, vk::Result> {
        // Try to find an existing bank (read lock)
        {
            let state = self.banks_lock.read().unwrap();
            for (i, bank_info) in state.bank_infos.iter().enumerate() {
                if (bank_info.score - reqs.score).abs() < SCORE_THRESHOLD
                    && bank_info.is_superset(reqs)
                {
                    return Ok(*state.banks[i].pools.last().unwrap());
                }
            }
        }

        // Not found, create a new bank (write lock)
        let mut state = self.banks_lock.write().unwrap();

        // Double-check after acquiring write lock
        for (i, bank_info) in state.bank_infos.iter().enumerate() {
            if (bank_info.score - reqs.score).abs() < SCORE_THRESHOLD && bank_info.is_superset(reqs)
            {
                return Ok(*state.banks[i].pools.last().unwrap());
            }
        }

        state.bank_infos.push(*reqs);
        let mut bank = DescriptorBank {
            info: *reqs,
            pools: Vec::new(),
        };
        allocate_pool(&self.device, &mut bank, self.sets_per_pool)?;
        let pool = *bank.pools.last().unwrap();
        state.banks.push(bank);

        debug!(
            "DescriptorPool: created new bank (total: {}, score: {})",
            state.banks.len(),
            reqs.score
        );

        Ok(pool)
    }

    /// Grow the bank matching the requirements by adding a new pool.
    fn grow_bank(&self, reqs: &DescriptorBankInfo) -> Result<vk::DescriptorPool, vk::Result> {
        let mut state = self.banks_lock.write().unwrap();
        for (i, bank_info) in state.bank_infos.iter().enumerate() {
            if (bank_info.score - reqs.score).abs() < SCORE_THRESHOLD && bank_info.is_superset(reqs)
            {
                allocate_pool(&self.device, &mut state.banks[i], self.sets_per_pool)?;
                return Ok(*state.banks[i].pools.last().unwrap());
            }
        }
        // Shouldn't reach here, but create a new bank just in case
        self.get_or_create_bank_pool(reqs)
    }
}

impl Drop for DescriptorPool {
    fn drop(&mut self) {
        let state = self.banks_lock.write().unwrap();
        for bank in &state.banks {
            for pool in &bank.pools {
                unsafe {
                    self.device.destroy_descriptor_pool(*pool, None);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bank_info_superset() {
        let big = DescriptorBankInfo {
            uniform_buffers: 10,
            storage_buffers: 10,
            texture_buffers: 10,
            image_buffers: 10,
            textures: 10,
            images: 10,
            score: 60,
        };
        let small = DescriptorBankInfo {
            uniform_buffers: 5,
            storage_buffers: 5,
            texture_buffers: 5,
            image_buffers: 5,
            textures: 5,
            images: 5,
            score: 30,
        };
        assert!(big.is_superset(&small));
        assert!(!small.is_superset(&big));
    }

    #[test]
    fn constants() {
        assert_eq!(SETS_GROW_RATE, 16);
        assert_eq!(SCORE_THRESHOLD, 3);
    }
}
