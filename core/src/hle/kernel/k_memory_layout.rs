//! Port of zuyu/src/core/hle/kernel/k_memory_layout.h and k_memory_layout.cpp
//! Status: Ported (structures and constants)
//! Derniere synchro: 2026-03-11

use super::k_memory_block::PAGE_SIZE;
use super::k_memory_region::{KMemoryRegion, KMemoryRegionTree};
use super::k_memory_region_type::*;

// ---------------------------------------------------------------------------
// Constants from k_memory_layout.h
// ---------------------------------------------------------------------------

pub const L1_BLOCK_SIZE: usize = 1 << 30; // 1 GiB
pub const L2_BLOCK_SIZE: usize = 2 << 20; // 2 MiB

pub const fn get_maximum_overhead_size(size: usize) -> usize {
    (common::alignment::divide_up(size as u64, L1_BLOCK_SIZE as u64)
        + common::alignment::divide_up(size as u64, L2_BLOCK_SIZE as u64)) as usize
        * PAGE_SIZE
}

pub const MAIN_MEMORY_SIZE: usize = 4 * (1 << 30); // 4 GiB
pub const MAIN_MEMORY_SIZE_MAX: usize = 8 * (1 << 30); // 8 GiB

pub const RESERVED_EARLY_DRAM_SIZE: usize = 384 * 1024;
pub const DRAM_PHYSICAL_ADDRESS: usize = 0x80000000;

pub const KERNEL_ASLR_ALIGNMENT: usize = 2 * (1 << 20); // 2 MiB
pub const KERNEL_VIRTUAL_ADDRESS_SPACE_WIDTH: usize = 1 << 39;
pub const KERNEL_PHYSICAL_ADDRESS_SPACE_WIDTH: usize = 1 << 48;

pub const KERNEL_VIRTUAL_ADDRESS_SPACE_BASE: usize = 0usize.wrapping_sub(KERNEL_VIRTUAL_ADDRESS_SPACE_WIDTH);
pub const KERNEL_VIRTUAL_ADDRESS_SPACE_END: usize =
    KERNEL_VIRTUAL_ADDRESS_SPACE_BASE + (KERNEL_VIRTUAL_ADDRESS_SPACE_WIDTH - KERNEL_ASLR_ALIGNMENT);
pub const KERNEL_VIRTUAL_ADDRESS_SPACE_LAST: usize = KERNEL_VIRTUAL_ADDRESS_SPACE_END - 1;
pub const KERNEL_VIRTUAL_ADDRESS_SPACE_SIZE: usize =
    KERNEL_VIRTUAL_ADDRESS_SPACE_END - KERNEL_VIRTUAL_ADDRESS_SPACE_BASE;
pub const KERNEL_VIRTUAL_ADDRESS_CODE_BASE: usize = KERNEL_VIRTUAL_ADDRESS_SPACE_BASE;
pub const KERNEL_VIRTUAL_ADDRESS_CODE_SIZE: usize = 392 * 1024;
pub const KERNEL_VIRTUAL_ADDRESS_CODE_END: usize =
    KERNEL_VIRTUAL_ADDRESS_CODE_BASE + KERNEL_VIRTUAL_ADDRESS_CODE_SIZE;

pub const KERNEL_PHYSICAL_ADDRESS_SPACE_BASE: usize = 0;
pub const KERNEL_PHYSICAL_ADDRESS_SPACE_END: usize =
    KERNEL_PHYSICAL_ADDRESS_SPACE_BASE + KERNEL_PHYSICAL_ADDRESS_SPACE_WIDTH;
pub const KERNEL_PHYSICAL_ADDRESS_SPACE_LAST: usize = KERNEL_PHYSICAL_ADDRESS_SPACE_END - 1;
pub const KERNEL_PHYSICAL_ADDRESS_SPACE_SIZE: usize =
    KERNEL_PHYSICAL_ADDRESS_SPACE_END - KERNEL_PHYSICAL_ADDRESS_SPACE_BASE;
pub const KERNEL_PHYSICAL_ADDRESS_CODE_BASE: usize =
    DRAM_PHYSICAL_ADDRESS + RESERVED_EARLY_DRAM_SIZE;

pub const KERNEL_PAGE_TABLE_HEAP_SIZE: usize = get_maximum_overhead_size(MAIN_MEMORY_SIZE_MAX);
pub const KERNEL_INITIAL_PAGE_HEAP_SIZE: usize = 128 * 1024;

pub const KERNEL_SLAB_HEAP_DATA_SIZE: usize = 5 * (1 << 20);
pub const KERNEL_SLAB_HEAP_GAPS_SIZE_MAX: usize = 2 * (1 << 20) - 64 * 1024;
pub const KERNEL_SLAB_HEAP_SIZE: usize = KERNEL_SLAB_HEAP_DATA_SIZE + KERNEL_SLAB_HEAP_GAPS_SIZE_MAX;

pub const KERNEL_PAGE_BUFFER_HEAP_SIZE: usize = 0x3E0000;
pub const KERNEL_SLAB_HEAP_ADDITIONAL_SIZE: usize = 0x148000;
pub const KERNEL_PAGE_BUFFER_ADDITIONAL_SIZE: usize = 0x33C000;

pub const KERNEL_RESOURCE_SIZE: usize = KERNEL_PAGE_TABLE_HEAP_SIZE
    + KERNEL_INITIAL_PAGE_HEAP_SIZE
    + KERNEL_SLAB_HEAP_SIZE
    + KERNEL_PAGE_BUFFER_HEAP_SIZE;

pub fn is_kernel_address(address: usize) -> bool {
    KERNEL_VIRTUAL_ADDRESS_SPACE_BASE <= address && address < KERNEL_VIRTUAL_ADDRESS_SPACE_END
}

// ---------------------------------------------------------------------------
// KMemoryLayout
// ---------------------------------------------------------------------------

/// Port of Kernel::KMemoryLayout.
pub struct KMemoryLayout {
    m_linear_phys_to_virt_diff: u64,
    m_linear_virt_to_phys_diff: u64,
    m_virtual_tree: KMemoryRegionTree,
    m_physical_tree: KMemoryRegionTree,
    m_virtual_linear_tree: KMemoryRegionTree,
    m_physical_linear_tree: KMemoryRegionTree,
}

impl KMemoryLayout {
    pub fn new() -> Self {
        Self {
            m_linear_phys_to_virt_diff: 0,
            m_linear_virt_to_phys_diff: 0,
            m_virtual_tree: KMemoryRegionTree::new(),
            m_physical_tree: KMemoryRegionTree::new(),
            m_virtual_linear_tree: KMemoryRegionTree::new(),
            m_physical_linear_tree: KMemoryRegionTree::new(),
        }
    }

    pub fn get_virtual_memory_region_tree(&self) -> &KMemoryRegionTree {
        &self.m_virtual_tree
    }
    pub fn get_virtual_memory_region_tree_mut(&mut self) -> &mut KMemoryRegionTree {
        &mut self.m_virtual_tree
    }
    pub fn get_physical_memory_region_tree(&self) -> &KMemoryRegionTree {
        &self.m_physical_tree
    }
    pub fn get_physical_memory_region_tree_mut(&mut self) -> &mut KMemoryRegionTree {
        &mut self.m_physical_tree
    }
    pub fn get_virtual_linear_memory_region_tree(&self) -> &KMemoryRegionTree {
        &self.m_virtual_linear_tree
    }
    pub fn get_virtual_linear_memory_region_tree_mut(&mut self) -> &mut KMemoryRegionTree {
        &mut self.m_virtual_linear_tree
    }
    pub fn get_physical_linear_memory_region_tree(&self) -> &KMemoryRegionTree {
        &self.m_physical_linear_tree
    }
    pub fn get_physical_linear_memory_region_tree_mut(&mut self) -> &mut KMemoryRegionTree {
        &mut self.m_physical_linear_tree
    }

    pub fn get_linear_virtual_address(&self, address: u64) -> u64 {
        address.wrapping_add(self.m_linear_phys_to_virt_diff)
    }

    pub fn get_linear_physical_address(&self, address: u64) -> u64 {
        address.wrapping_add(self.m_linear_virt_to_phys_diff)
    }

    pub fn find_virtual(&self, address: u64) -> Option<&KMemoryRegion> {
        self.m_virtual_tree.find(address)
    }

    pub fn find_physical(&self, address: u64) -> Option<&KMemoryRegion> {
        self.m_physical_tree.find(address)
    }

    pub fn find_virtual_linear(&self, address: u64) -> Option<&KMemoryRegion> {
        self.m_virtual_linear_tree.find(address)
    }

    pub fn find_physical_linear(&self, address: u64) -> Option<&KMemoryRegion> {
        self.m_physical_linear_tree.find(address)
    }

    pub fn initialize_linear_memory_region_trees(
        &mut self,
        aligned_linear_phys_start: u64,
        linear_virtual_start: u64,
    ) {
        self.m_linear_phys_to_virt_diff =
            linear_virtual_start.wrapping_sub(aligned_linear_phys_start);
        self.m_linear_virt_to_phys_diff =
            aligned_linear_phys_start.wrapping_sub(linear_virtual_start);

        // Copy linear-mapped physical regions.
        let phys_regions: Vec<KMemoryRegion> = self
            .m_physical_tree
            .iter()
            .filter(|r| r.has_type_attribute(K_MEMORY_REGION_ATTR_LINEAR_MAPPED))
            .cloned()
            .collect();
        for region in phys_regions {
            self.m_physical_linear_tree.insert_directly(
                region.get_address(),
                region.get_last_address(),
                region.get_attributes(),
                region.get_type(),
            );
        }

        // Copy DRAM-derived virtual regions.
        let virt_regions: Vec<KMemoryRegion> = self
            .m_virtual_tree
            .iter()
            .filter(|r| r.is_derived_from(K_MEMORY_REGION_TYPE_DRAM))
            .cloned()
            .collect();
        for region in virt_regions {
            self.m_virtual_linear_tree.insert_directly(
                region.get_address(),
                region.get_last_address(),
                region.get_attributes(),
                region.get_type(),
            );
        }
    }

    /// Port of KMemoryLayout::GetResourceRegionSizeForInit.
    /// Note: KSystemControl::SecureAppletMemorySize is 0 in most configurations.
    pub fn get_resource_region_size_for_init(use_extra_resource: bool) -> usize {
        let secure_applet_memory_size: usize = 0; // KSystemControl::SecureAppletMemorySize
        KERNEL_RESOURCE_SIZE
            + secure_applet_memory_size
            + if use_extra_resource {
                KERNEL_SLAB_HEAP_ADDITIONAL_SIZE + KERNEL_PAGE_BUFFER_ADDITIONAL_SIZE
            } else {
                0
            }
    }

    pub fn get_total_and_kernel_memory_sizes(&self) -> (usize, usize) {
        let mut total_size: usize = 0;
        let mut kernel_size: usize = 0;
        for region in self.m_physical_tree.iter() {
            if region.is_derived_from(K_MEMORY_REGION_TYPE_DRAM) {
                total_size += region.get_size();
                if !region.is_derived_from(K_MEMORY_REGION_TYPE_DRAM_USER_POOL) {
                    kernel_size += region.get_size();
                }
            }
        }
        (total_size, kernel_size)
    }
}

impl Default for KMemoryLayout {
    fn default() -> Self {
        Self::new()
    }
}
