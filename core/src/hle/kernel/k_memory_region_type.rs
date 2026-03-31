//! Port of zuyu/src/core/hle/kernel/k_memory_region_type.h
//! Status: Ported
//! Derniere synchro: 2026-03-11
//!
//! Memory region type derivation system and all region type constants.
//! Includes ARM64 architecture device types and Nintendo NX board device types.

/// Attribute flags for memory region types.
pub const K_MEMORY_REGION_ATTR_CARVEOUT_PROTECTED: u32 = 0x02000000;
pub const K_MEMORY_REGION_ATTR_UNCACHED: u32 = 0x04000000;
pub const K_MEMORY_REGION_ATTR_DID_KERNEL_MAP: u32 = 0x08000000;
pub const K_MEMORY_REGION_ATTR_SHOULD_KERNEL_MAP: u32 = 0x10000000;
pub const K_MEMORY_REGION_ATTR_USER_READ_ONLY: u32 = 0x20000000;
pub const K_MEMORY_REGION_ATTR_NO_USER_MAP: u32 = 0x40000000;
pub const K_MEMORY_REGION_ATTR_LINEAR_MAPPED: u32 = 0x80000000;

/// Convenience alias for the enum-like KMemoryRegionType.
pub type KMemoryRegionType = u32;

// ---------------------------------------------------------------------------
// KMemoryRegionTypeValue — derivation helper (mirrors impl::KMemoryRegionTypeValue)
// ---------------------------------------------------------------------------

/// A compile-time region type value builder.
/// All methods are const to mirror constexpr upstream.
#[derive(Debug, Clone, Copy)]
pub struct KMemoryRegionTypeValue {
    value: u32,
    next_bit: usize,
}

impl KMemoryRegionTypeValue {
    pub const fn new() -> Self {
        Self {
            value: 0,
            next_bit: 0,
        }
    }

    pub const fn get_value(&self) -> u32 {
        self.value
    }

    pub const fn derive_initial(self, i: usize, next: usize) -> Self {
        Self {
            value: 1u32 << i,
            next_bit: next,
        }
    }

    pub const fn derive_attribute(self, attr: u32) -> Self {
        Self {
            value: self.value | attr,
            next_bit: self.next_bit,
        }
    }

    pub const fn set_attribute(self, attr: u32) -> Self {
        Self {
            value: self.value | attr,
            next_bit: self.next_bit,
        }
    }

    pub const fn derive_transition_with(self, ofs: usize, adv: usize) -> Self {
        Self {
            value: self.value | (1u32 << (self.next_bit + ofs)),
            next_bit: self.next_bit + adv,
        }
    }

    pub const fn derive_transition(self) -> Self {
        self.derive_transition_with(0, 1)
    }

    pub const fn derive_sparse(self, ofs: usize, n: usize, i: usize) -> Self {
        Self {
            value: self.value
                | (1u32 << (self.next_bit + ofs))
                | (1u32 << (self.next_bit + ofs + 1 + i)),
            next_bit: self.next_bit + ofs + n + 1,
        }
    }

    /// Dense derivation: uses the bit-pair encoding from upstream BitsForDeriveDense.
    pub const fn derive(self, n: usize, i: usize) -> Self {
        let mut low: usize = 0;
        let mut high: usize = 1;
        let mut j: usize = 0;
        while j < i {
            low += 1;
            if low == high {
                high += 1;
                low = 0;
            }
            j += 1;
        }

        let new_value =
            self.value | (1u32 << (self.next_bit + low)) | (1u32 << (self.next_bit + high));
        let bits_for_dense = {
            let mut lo: usize = 0;
            let mut hi: usize = 1;
            let mut k: usize = 0;
            while k < n - 1 {
                lo += 1;
                if lo == hi {
                    hi += 1;
                    lo = 0;
                }
                k += 1;
            }
            hi + 1
        };
        Self {
            value: new_value,
            next_bit: self.next_bit + bits_for_dense,
        }
    }

    pub const fn advance(self, n: usize) -> Self {
        Self {
            value: self.value,
            next_bit: self.next_bit + n,
        }
    }

    pub const fn is_ancestor_of(&self, v: u32) -> bool {
        (self.value | v) == v
    }
}

// ---------------------------------------------------------------------------
// Region type constants
// ---------------------------------------------------------------------------

pub const K_MEMORY_REGION_TYPE_NONE: KMemoryRegionTypeValue = KMemoryRegionTypeValue::new();

pub const K_MEMORY_REGION_TYPE_KERNEL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_NONE.derive_initial(0, 2);
pub const K_MEMORY_REGION_TYPE_DRAM: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_NONE.derive_initial(1, 2);

// Dram sub-regions
pub const K_MEMORY_REGION_TYPE_DRAM_KERNEL_BASE: KMemoryRegionTypeValue = K_MEMORY_REGION_TYPE_DRAM
    .derive_sparse(0, 3, 0)
    .set_attribute(K_MEMORY_REGION_ATTR_NO_USER_MAP)
    .set_attribute(K_MEMORY_REGION_ATTR_CARVEOUT_PROTECTED);
pub const K_MEMORY_REGION_TYPE_DRAM_RESERVED_BASE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM.derive_sparse(0, 3, 1);
pub const K_MEMORY_REGION_TYPE_DRAM_HEAP_BASE: KMemoryRegionTypeValue = K_MEMORY_REGION_TYPE_DRAM
    .derive_sparse(0, 3, 2)
    .set_attribute(K_MEMORY_REGION_ATTR_LINEAR_MAPPED);

// Dram kernel code/slab/pt
pub const K_MEMORY_REGION_TYPE_DRAM_KERNEL_CODE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_KERNEL_BASE.derive_sparse(0, 4, 0);
pub const K_MEMORY_REGION_TYPE_DRAM_KERNEL_SLAB: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_KERNEL_BASE.derive_sparse(0, 4, 1);
pub const K_MEMORY_REGION_TYPE_DRAM_KERNEL_PT_HEAP: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_KERNEL_BASE
        .derive_sparse(0, 4, 2)
        .set_attribute(K_MEMORY_REGION_ATTR_LINEAR_MAPPED);
pub const K_MEMORY_REGION_TYPE_DRAM_KERNEL_INIT_PT: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_KERNEL_BASE
        .derive_sparse(0, 4, 3)
        .set_attribute(K_MEMORY_REGION_ATTR_LINEAR_MAPPED);

// Dram kernel secure
pub const K_MEMORY_REGION_TYPE_DRAM_KERNEL_SECURE_APPLET_MEMORY: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_KERNEL_BASE
        .derive_sparse(1, 3, 0)
        .set_attribute(K_MEMORY_REGION_ATTR_LINEAR_MAPPED);
pub const K_MEMORY_REGION_TYPE_DRAM_KERNEL_SECURE_UNKNOWN: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_KERNEL_BASE
        .derive_sparse(1, 3, 1)
        .set_attribute(K_MEMORY_REGION_ATTR_LINEAR_MAPPED);

// Dram reserved
pub const K_MEMORY_REGION_TYPE_DRAM_RESERVED_EARLY: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_RESERVED_BASE.derive_attribute(K_MEMORY_REGION_ATTR_NO_USER_MAP);
pub const K_MEMORY_REGION_TYPE_KERNEL_TRACE_BUFFER: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_RESERVED_BASE
        .derive_sparse(0, 3, 0)
        .set_attribute(K_MEMORY_REGION_ATTR_LINEAR_MAPPED)
        .set_attribute(K_MEMORY_REGION_ATTR_USER_READ_ONLY);
pub const K_MEMORY_REGION_TYPE_ON_MEMORY_BOOT_IMAGE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_RESERVED_BASE.derive_sparse(0, 3, 1);
pub const K_MEMORY_REGION_TYPE_DTB: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_RESERVED_BASE.derive_sparse(0, 3, 2);

// Dram pool partition
pub const K_MEMORY_REGION_TYPE_DRAM_POOL_PARTITION: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_HEAP_BASE.derive_attribute(K_MEMORY_REGION_ATTR_NO_USER_MAP);
pub const K_MEMORY_REGION_TYPE_DRAM_POOL_MANAGEMENT: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_POOL_PARTITION
        .derive(4, 0)
        .set_attribute(K_MEMORY_REGION_ATTR_CARVEOUT_PROTECTED);
pub const K_MEMORY_REGION_TYPE_DRAM_USER_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_POOL_PARTITION.derive(4, 3);
pub const K_MEMORY_REGION_TYPE_DRAM_APPLICATION_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_USER_POOL.derive(4, 0);
pub const K_MEMORY_REGION_TYPE_DRAM_APPLET_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_USER_POOL.derive(4, 1);
pub const K_MEMORY_REGION_TYPE_DRAM_SYSTEM_NON_SECURE_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_USER_POOL.derive(4, 2);
pub const K_MEMORY_REGION_TYPE_DRAM_SYSTEM_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM_USER_POOL
        .derive(4, 3)
        .set_attribute(K_MEMORY_REGION_ATTR_CARVEOUT_PROTECTED);

// Virtual Dram regions
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_HEAP_BASE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM.derive_sparse(1, 4, 0);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_PT_HEAP: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM.derive_sparse(1, 4, 1);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_TRACE_BUFFER: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM.derive_sparse(1, 4, 2);

pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_UNKNOWN_DEBUG: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM.advance(2).derive(4, 0);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_SECURE_APPLET_MEMORY: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM.advance(2).derive(4, 1);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_SECURE_UNKNOWN: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_DRAM.advance(2).derive(4, 3);

pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_INIT_PT: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_HEAP_BASE.derive(4, 0);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_POOL_MANAGEMENT: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_HEAP_BASE.derive(4, 1);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_USER_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_HEAP_BASE.derive(4, 2);

pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_APPLICATION_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_USER_POOL.derive(4, 0);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_APPLET_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_USER_POOL.derive(4, 1);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_SYSTEM_NON_SECURE_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_USER_POOL.derive(4, 2);
pub const K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_SYSTEM_POOL: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_USER_POOL.derive(4, 3);

// Kernel virtual regions
pub const K_MEMORY_REGION_TYPE_KERNEL_CODE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL.derive_sparse(1, 4, 0);
pub const K_MEMORY_REGION_TYPE_KERNEL_STACK: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL.derive_sparse(1, 4, 1);
pub const K_MEMORY_REGION_TYPE_KERNEL_MISC: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL.derive_sparse(1, 4, 2);
pub const K_MEMORY_REGION_TYPE_KERNEL_SLAB: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL.derive_sparse(1, 4, 3);

pub const K_MEMORY_REGION_TYPE_KERNEL_MISC_DERIVED_BASE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL_MISC.derive_transition();

pub const K_MEMORY_REGION_TYPE_KERNEL_MISC_MAIN_STACK: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL_MISC_DERIVED_BASE.derive(7, 1);
pub const K_MEMORY_REGION_TYPE_KERNEL_MISC_MAPPED_DEVICE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL_MISC_DERIVED_BASE.derive(7, 2);
pub const K_MEMORY_REGION_TYPE_KERNEL_MISC_EXCEPTION_STACK: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL_MISC_DERIVED_BASE.derive(7, 3);
pub const K_MEMORY_REGION_TYPE_KERNEL_MISC_UNKNOWN_DEBUG: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL_MISC_DERIVED_BASE.derive(7, 4);
pub const K_MEMORY_REGION_TYPE_KERNEL_MISC_IDLE_STACK: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL_MISC_DERIVED_BASE.derive(7, 6);

pub const K_MEMORY_REGION_TYPE_KERNEL_TEMP: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL.advance(2).derive(2, 0);

// Architecture device base (ARM64)
pub const K_MEMORY_REGION_TYPE_ARCH_DEVICE_BASE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL.derive_transition_with(0, 1);
// Board device base (NX) — same value, different derivation path
pub const K_MEMORY_REGION_TYPE_BOARD_DEVICE_BASE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_KERNEL.derive_transition_with(0, 2);

// ARM64 architecture device regions
pub const NUM_ARCHITECTURE_DEVICE_REGIONS: usize = 3;
pub const K_MEMORY_REGION_TYPE_UART: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_ARCH_DEVICE_BASE.derive_sparse(0, NUM_ARCHITECTURE_DEVICE_REGIONS, 0);
pub const K_MEMORY_REGION_TYPE_INTERRUPT_CPU_INTERFACE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_ARCH_DEVICE_BASE
        .derive_sparse(0, NUM_ARCHITECTURE_DEVICE_REGIONS, 1)
        .set_attribute(K_MEMORY_REGION_ATTR_NO_USER_MAP);
pub const K_MEMORY_REGION_TYPE_INTERRUPT_DISTRIBUTOR: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_ARCH_DEVICE_BASE
        .derive_sparse(0, NUM_ARCHITECTURE_DEVICE_REGIONS, 2)
        .set_attribute(K_MEMORY_REGION_ATTR_NO_USER_MAP);

// Nintendo NX board device regions
pub const NUM_BOARD_DEVICE_REGIONS: usize = 6;
pub const K_MEMORY_REGION_TYPE_MEMORY_CONTROLLER: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_BOARD_DEVICE_BASE
        .derive(NUM_BOARD_DEVICE_REGIONS, 1)
        .set_attribute(K_MEMORY_REGION_ATTR_NO_USER_MAP);
pub const K_MEMORY_REGION_TYPE_MEMORY_CONTROLLER1: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_BOARD_DEVICE_BASE
        .derive(NUM_BOARD_DEVICE_REGIONS, 2)
        .set_attribute(K_MEMORY_REGION_ATTR_NO_USER_MAP);
pub const K_MEMORY_REGION_TYPE_MEMORY_CONTROLLER0: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_BOARD_DEVICE_BASE
        .derive(NUM_BOARD_DEVICE_REGIONS, 3)
        .set_attribute(K_MEMORY_REGION_ATTR_NO_USER_MAP);
pub const K_MEMORY_REGION_TYPE_POWER_MANAGEMENT_CONTROLLER: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_BOARD_DEVICE_BASE
        .derive(NUM_BOARD_DEVICE_REGIONS, 4)
        .derive_transition();
pub const K_MEMORY_REGION_TYPE_LEGACY_LPS_DEVICES: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_BOARD_DEVICE_BASE.derive(NUM_BOARD_DEVICE_REGIONS, 5);

pub const NUM_LEGACY_LPS_DEVICES: usize = 7;
pub const K_MEMORY_REGION_TYPE_LEGACY_LPS_EXCEPTION_VECTORS: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_LEGACY_LPS_DEVICES.derive(NUM_LEGACY_LPS_DEVICES, 0);
pub const K_MEMORY_REGION_TYPE_LEGACY_LPS_IRAM: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_LEGACY_LPS_DEVICES.derive(NUM_LEGACY_LPS_DEVICES, 1);
pub const K_MEMORY_REGION_TYPE_LEGACY_LPS_FLOW_CONTROLLER: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_LEGACY_LPS_DEVICES.derive(NUM_LEGACY_LPS_DEVICES, 2);
pub const K_MEMORY_REGION_TYPE_LEGACY_LPS_PRIMARY_ICTLR: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_LEGACY_LPS_DEVICES.derive(NUM_LEGACY_LPS_DEVICES, 3);
pub const K_MEMORY_REGION_TYPE_LEGACY_LPS_SEMAPHORE: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_LEGACY_LPS_DEVICES.derive(NUM_LEGACY_LPS_DEVICES, 4);
pub const K_MEMORY_REGION_TYPE_LEGACY_LPS_ATOMICS: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_LEGACY_LPS_DEVICES.derive(NUM_LEGACY_LPS_DEVICES, 5);
pub const K_MEMORY_REGION_TYPE_LEGACY_LPS_CLK_RST: KMemoryRegionTypeValue =
    K_MEMORY_REGION_TYPE_LEGACY_LPS_DEVICES.derive(NUM_LEGACY_LPS_DEVICES, 6);

// ---------------------------------------------------------------------------
// GetTypeForVirtualLinearMapping
// ---------------------------------------------------------------------------

pub const fn get_type_for_virtual_linear_mapping(type_id: u32) -> KMemoryRegionTypeValue {
    if K_MEMORY_REGION_TYPE_DRAM_KERNEL_PT_HEAP.is_ancestor_of(type_id) {
        K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_PT_HEAP
    } else if K_MEMORY_REGION_TYPE_DRAM_KERNEL_SECURE_APPLET_MEMORY.is_ancestor_of(type_id) {
        K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_SECURE_APPLET_MEMORY
    } else if K_MEMORY_REGION_TYPE_DRAM_KERNEL_SECURE_UNKNOWN.is_ancestor_of(type_id) {
        K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_SECURE_UNKNOWN
    } else if K_MEMORY_REGION_TYPE_KERNEL_TRACE_BUFFER.is_ancestor_of(type_id) {
        K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_TRACE_BUFFER
    } else if (type_id | K_MEMORY_REGION_ATTR_SHOULD_KERNEL_MAP) == type_id {
        K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_UNKNOWN_DEBUG
    } else {
        K_MEMORY_REGION_TYPE_DRAM
    }
}

// ---------------------------------------------------------------------------
// Static assertions (compile-time)
// ---------------------------------------------------------------------------

const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL.get_value() == 0x1);
const _: () = assert!(K_MEMORY_REGION_TYPE_DRAM.get_value() == 0x2);

const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL_CODE.get_value() == 0x19);
const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL_STACK.get_value() == 0x29);
const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL_MISC.get_value() == 0x49);
const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL_SLAB.get_value() == 0x89);

const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL_MISC_DERIVED_BASE.get_value() == 0x149);
const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL_MISC_MAIN_STACK.get_value() == 0xB49);
const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL_MISC_IDLE_STACK.get_value() == 0x2349);
const _: () = assert!(K_MEMORY_REGION_TYPE_KERNEL_TEMP.get_value() == 0x31);

const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_HEAP_BASE.get_value() == 0x1A);
const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_PT_HEAP.get_value() == 0x2A);
const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_TRACE_BUFFER.get_value() == 0x4A);

const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_KERNEL_INIT_PT.get_value() == 0x31A);
const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_POOL_MANAGEMENT.get_value() == 0x51A);
const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_USER_POOL.get_value() == 0x61A);

const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_APPLICATION_POOL.get_value() == 0x361A);
const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_APPLET_POOL.get_value() == 0x561A);
const _: () =
    assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_SYSTEM_NON_SECURE_POOL.get_value() == 0x661A);
const _: () = assert!(K_MEMORY_REGION_TYPE_VIRTUAL_DRAM_SYSTEM_POOL.get_value() == 0x961A);

const _: () = assert!(K_MEMORY_REGION_TYPE_UART.get_value() == 0x1D);
