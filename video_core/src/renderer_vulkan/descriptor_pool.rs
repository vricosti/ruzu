// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `vk_descriptor_pool.h` / `vk_descriptor_pool.cpp`.
//!
//! Banked descriptor set allocation pool. Manages multiple VkDescriptorPools
//! organized into banks by descriptor type requirements.

use std::sync::{Arc, Mutex, RwLock};

use ash::vk;
use log::debug;

use super::resource_pool::ResourcePool;

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
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
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
) -> Result<vk::DescriptorPool, vk::Result> {
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

    let pool_ci = vk::DescriptorPoolCreateInfo::builder()
        .max_sets(sets_per_pool)
        .pool_sizes(&pool_sizes)
        .build();

    let pool = unsafe { device.create_descriptor_pool(&pool_ci, None)? };
    bank.pools.push(pool);
    Ok(pool)
}

struct DescriptorAllocatorState {
    resource_pool: ResourcePool,
    sets: Vec<Vec<vk::DescriptorSet>>,
}

/// Port of upstream `DescriptorAllocator`.
///
/// Each pipeline owns one allocator. The allocator reserves descriptor sets
/// in groups of `SETS_GROW_RATE` and tags every set with the scheduler tick
/// that references it.
pub struct DescriptorAllocator {
    device: ash::Device,
    bank: Arc<Mutex<DescriptorBank>>,
    layout: vk::DescriptorSetLayout,
    sets_per_pool: u32,
    state: Mutex<DescriptorAllocatorState>,
}

impl DescriptorAllocator {
    fn new(
        device: ash::Device,
        bank: Arc<Mutex<DescriptorBank>>,
        layout: vk::DescriptorSetLayout,
        sets_per_pool: u32,
    ) -> Self {
        Self {
            device,
            bank,
            layout,
            sets_per_pool,
            state: Mutex::new(DescriptorAllocatorState {
                resource_pool: ResourcePool::new_with_external_ticks(SETS_GROW_RATE),
                sets: Vec::new(),
            }),
        }
    }

    /// Port of `DescriptorAllocator::Commit`.
    pub fn commit(
        &self,
        known_gpu_tick: u64,
        current_tick: u64,
    ) -> Result<vk::DescriptorSet, vk::Result> {
        let mut state = self.state.lock().unwrap();
        let device = self.device.clone();
        let bank = Arc::clone(&self.bank);
        let layout = self.layout;
        let sets_per_pool = self.sets_per_pool;
        let DescriptorAllocatorState {
            resource_pool,
            sets,
        } = &mut *state;
        let mut allocate = |begin: usize, end: usize| {
            sets.push(Self::allocate_descriptors(
                &device,
                &bank,
                layout,
                end - begin,
                sets_per_pool,
            )?);
            Ok(())
        };
        let index = resource_pool.try_commit_resource_with_ticks(
            known_gpu_tick,
            current_tick,
            &mut allocate,
        )?;
        Ok(sets[index / SETS_GROW_RATE][index % SETS_GROW_RATE])
    }

    /// Port of `DescriptorAllocator::AllocateDescriptors`.
    fn allocate_descriptors(
        device: &ash::Device,
        bank: &Arc<Mutex<DescriptorBank>>,
        layout: vk::DescriptorSetLayout,
        count: usize,
        sets_per_pool: u32,
    ) -> Result<Vec<vk::DescriptorSet>, vk::Result> {
        let layouts = vec![layout; count];
        let mut bank = bank.lock().unwrap();
        let mut allocate_info = vk::DescriptorSetAllocateInfo::builder()
            .descriptor_pool(*bank.pools.last().expect("descriptor bank has no pool"))
            .set_layouts(&layouts)
            .build();
        match unsafe { device.allocate_descriptor_sets(&allocate_info) } {
            Ok(sets) => Ok(sets),
            Err(vk::Result::ERROR_OUT_OF_POOL_MEMORY) => {
                let pool = allocate_pool(device, &mut bank, sets_per_pool)?;
                allocate_info.descriptor_pool = pool;
                unsafe { device.allocate_descriptor_sets(&allocate_info) }
            }
            Err(error) => Err(error),
        }
    }
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
    banks: Vec<Arc<Mutex<DescriptorBank>>>,
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

    /// Port of `DescriptorPool::Allocator`.
    pub fn allocator(
        &self,
        layout: vk::DescriptorSetLayout,
        info: &DescriptorBankInfo,
    ) -> Result<DescriptorAllocator, vk::Result> {
        let bank = self.bank(info)?;
        Ok(DescriptorAllocator::new(
            self.device.clone(),
            bank,
            layout,
            self.sets_per_pool,
        ))
    }

    /// Port of `DescriptorPool::Bank`.
    fn bank(&self, reqs: &DescriptorBankInfo) -> Result<Arc<Mutex<DescriptorBank>>, vk::Result> {
        {
            let state = self.banks_lock.read().unwrap();
            for (i, bank_info) in state.bank_infos.iter().enumerate() {
                if (bank_info.score - reqs.score).abs() < SCORE_THRESHOLD
                    && bank_info.is_superset(reqs)
                {
                    return Ok(Arc::clone(&state.banks[i]));
                }
            }
        }

        let mut state = self.banks_lock.write().unwrap();
        state.bank_infos.push(*reqs);
        let mut bank = DescriptorBank {
            info: *reqs,
            pools: Vec::new(),
        };
        allocate_pool(&self.device, &mut bank, self.sets_per_pool)?;
        let bank = Arc::new(Mutex::new(bank));
        state.banks.push(Arc::clone(&bank));

        debug!(
            "DescriptorPool: created new bank (total: {}, score: {})",
            state.banks.len(),
            reqs.score
        );

        Ok(bank)
    }
}

impl Drop for DescriptorPool {
    fn drop(&mut self) {
        let state = self.banks_lock.write().unwrap();
        for bank in &state.banks {
            for pool in &bank.lock().unwrap().pools {
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
    fn bank_info_superset_matches_upstream_image_buffer_comparison() {
        let bank = DescriptorBankInfo {
            image_buffers: 8,
            images: 1,
            score: 9,
            ..DescriptorBankInfo::default()
        };
        let request = DescriptorBankInfo {
            image_buffers: 1,
            images: 2,
            score: 3,
            ..DescriptorBankInfo::default()
        };

        // Keep the exact comparison in local upstream
        // `DescriptorBankInfo::IsSuperset`: `images` is compared with the
        // requested image-buffer count.
        assert!(bank.is_superset(&request));
    }

    #[test]
    fn constants() {
        assert_eq!(SETS_GROW_RATE, 16);
        assert_eq!(SCORE_THRESHOLD, 3);
    }
}
