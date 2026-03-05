//! Port of zuyu/src/core/hle/kernel/k_address_space_info.h and k_address_space_info.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Address space configuration for 32/36/39-bit address space modes.
//! Provides the base address and size for each region type depending on the
//! address space width (as specified by CreateProcessFlag).

/// Type of address space region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AddressSpaceType {
    MapSmall = 0,
    MapLarge = 1,
    Map39Bit = 2,
    Heap = 3,
    Stack = 4,
    Alias = 5,
}

/// Address space info entry: describes a region's base address and size for a given bit width.
#[derive(Debug, Clone, Copy)]
pub struct AddressSpaceInfo {
    pub bit_width: usize,
    pub address: u64,
    pub size: u64,
    pub region_type: AddressSpaceType,
}

/// Sentinel value meaning "address is determined at runtime" (not fixed).
const SIZE_INVALID: u64 = u64::MAX;

const KIB: u64 = 1024;
const MIB: u64 = 1024 * KIB;
const GIB: u64 = 1024 * MIB;

/// The address space info table, matching zuyu's AddressSpaceInfos array.
static ADDRESS_SPACE_INFOS: &[AddressSpaceInfo] = &[
    // 32-bit address space
    AddressSpaceInfo { bit_width: 32, address: 2 * MIB,       size: 1 * GIB - 2 * MIB,  region_type: AddressSpaceType::MapSmall },
    AddressSpaceInfo { bit_width: 32, address: 1 * GIB,       size: 4 * GIB - 1 * GIB,  region_type: AddressSpaceType::MapLarge },
    AddressSpaceInfo { bit_width: 32, address: SIZE_INVALID,   size: 1 * GIB,             region_type: AddressSpaceType::Alias },
    AddressSpaceInfo { bit_width: 32, address: SIZE_INVALID,   size: 1 * GIB,             region_type: AddressSpaceType::Heap },
    // 36-bit address space
    AddressSpaceInfo { bit_width: 36, address: 128 * MIB,      size: 2 * GIB - 128 * MIB, region_type: AddressSpaceType::MapSmall },
    AddressSpaceInfo { bit_width: 36, address: 2 * GIB,        size: 64 * GIB - 2 * GIB,  region_type: AddressSpaceType::MapLarge },
    AddressSpaceInfo { bit_width: 36, address: SIZE_INVALID,    size: 8 * GIB,              region_type: AddressSpaceType::Heap },
    AddressSpaceInfo { bit_width: 36, address: SIZE_INVALID,    size: 6 * GIB,              region_type: AddressSpaceType::Alias },
    // 39-bit address space
    AddressSpaceInfo { bit_width: 39, address: 128 * MIB,      size: 512 * GIB - 128 * MIB, region_type: AddressSpaceType::Map39Bit },
    AddressSpaceInfo { bit_width: 39, address: SIZE_INVALID,    size: 64 * GIB,               region_type: AddressSpaceType::MapSmall },
    AddressSpaceInfo { bit_width: 39, address: SIZE_INVALID,    size: 8 * GIB,                region_type: AddressSpaceType::Heap },
    AddressSpaceInfo { bit_width: 39, address: SIZE_INVALID,    size: 64 * GIB,               region_type: AddressSpaceType::Alias },
    AddressSpaceInfo { bit_width: 39, address: SIZE_INVALID,    size: 2 * GIB,                region_type: AddressSpaceType::Stack },
];

/// Look up the address space info entry for a given bit width and region type.
fn get_address_space_info(width: usize, region_type: AddressSpaceType) -> &'static AddressSpaceInfo {
    ADDRESS_SPACE_INFOS
        .iter()
        .find(|info| info.bit_width == width && info.region_type == region_type)
        .unwrap_or_else(|| panic!("Could not find AddressSpaceInfo for width={}, type={:?}", width, region_type))
}

/// Get the start address for a given address space width and region type.
///
/// Returns `SIZE_INVALID` (u64::MAX) if the address is determined at runtime.
pub fn get_address_space_start(width: usize, region_type: AddressSpaceType) -> u64 {
    get_address_space_info(width, region_type).address
}

/// Get the size for a given address space width and region type.
pub fn get_address_space_size(width: usize, region_type: AddressSpaceType) -> u64 {
    get_address_space_info(width, region_type).size
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_39bit_map39bit() {
        let start = get_address_space_start(39, AddressSpaceType::Map39Bit);
        assert_eq!(start, 128 * MIB);
        let size = get_address_space_size(39, AddressSpaceType::Map39Bit);
        assert_eq!(size, 512 * GIB - 128 * MIB);
    }

    #[test]
    fn test_32bit_heap() {
        let start = get_address_space_start(32, AddressSpaceType::Heap);
        assert_eq!(start, SIZE_INVALID);
        let size = get_address_space_size(32, AddressSpaceType::Heap);
        assert_eq!(size, 1 * GIB);
    }

    #[test]
    fn test_39bit_stack() {
        let size = get_address_space_size(39, AddressSpaceType::Stack);
        assert_eq!(size, 2 * GIB);
    }

    #[test]
    fn test_36bit_map_large() {
        let start = get_address_space_start(36, AddressSpaceType::MapLarge);
        assert_eq!(start, 2 * GIB);
        let size = get_address_space_size(36, AddressSpaceType::MapLarge);
        assert_eq!(size, 64 * GIB - 2 * GIB);
    }

    #[test]
    #[should_panic]
    fn test_invalid_combination() {
        // 32-bit has no Stack type
        get_address_space_start(32, AddressSpaceType::Stack);
    }
}
