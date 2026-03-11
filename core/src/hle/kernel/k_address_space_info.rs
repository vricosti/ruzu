//! Port of zuyu/src/core/hle/kernel/k_address_space_info.h and k_address_space_info.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-11

const SIZE_INVALID: u64 = u64::MAX;

const KI_B: u64 = 1024;
const MI_B: u64 = 1024 * KI_B;
const GI_B: u64 = 1024 * MI_B;

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressSpaceInfoType {
    MapSmall = 0,
    MapLarge = 1,
    Map39Bit = 2,
    Heap = 3,
    Stack = 4,
    Alias = 5,
    Count = 6,
}

#[derive(Debug, Clone, Copy)]
pub struct KAddressSpaceInfo {
    pub bit_width: usize,
    pub address: u64,
    pub size: u64,
    pub info_type: AddressSpaceInfoType,
}

/// The address space info table matching upstream exactly.
static ADDRESS_SPACE_INFOS: [KAddressSpaceInfo; 13] = [
    KAddressSpaceInfo { bit_width: 32, address: 2 * MI_B,        size: 1 * GI_B - 2 * MI_B,   info_type: AddressSpaceInfoType::MapSmall },
    KAddressSpaceInfo { bit_width: 32, address: 1 * GI_B,        size: 4 * GI_B - 1 * GI_B,   info_type: AddressSpaceInfoType::MapLarge },
    KAddressSpaceInfo { bit_width: 32, address: SIZE_INVALID,     size: 1 * GI_B,               info_type: AddressSpaceInfoType::Alias },
    KAddressSpaceInfo { bit_width: 32, address: SIZE_INVALID,     size: 1 * GI_B,               info_type: AddressSpaceInfoType::Heap },
    KAddressSpaceInfo { bit_width: 36, address: 128 * MI_B,      size: 2 * GI_B - 128 * MI_B,  info_type: AddressSpaceInfoType::MapSmall },
    KAddressSpaceInfo { bit_width: 36, address: 2 * GI_B,        size: 64 * GI_B - 2 * GI_B,   info_type: AddressSpaceInfoType::MapLarge },
    KAddressSpaceInfo { bit_width: 36, address: SIZE_INVALID,     size: 8 * GI_B,               info_type: AddressSpaceInfoType::Heap },
    KAddressSpaceInfo { bit_width: 36, address: SIZE_INVALID,     size: 6 * GI_B,               info_type: AddressSpaceInfoType::Alias },
    KAddressSpaceInfo { bit_width: 39, address: 128 * MI_B,      size: 512 * GI_B - 128 * MI_B, info_type: AddressSpaceInfoType::Map39Bit },
    KAddressSpaceInfo { bit_width: 39, address: SIZE_INVALID,     size: 64 * GI_B,              info_type: AddressSpaceInfoType::MapSmall },
    KAddressSpaceInfo { bit_width: 39, address: SIZE_INVALID,     size: 8 * GI_B,               info_type: AddressSpaceInfoType::Heap },
    KAddressSpaceInfo { bit_width: 39, address: SIZE_INVALID,     size: 64 * GI_B,              info_type: AddressSpaceInfoType::Alias },
    KAddressSpaceInfo { bit_width: 39, address: SIZE_INVALID,     size: 2 * GI_B,               info_type: AddressSpaceInfoType::Stack },
];

fn get_address_space_info(
    width: usize,
    info_type: AddressSpaceInfoType,
) -> &'static KAddressSpaceInfo {
    for info in &ADDRESS_SPACE_INFOS {
        if info.bit_width == width && info.info_type as u32 == info_type as u32 {
            return info;
        }
    }
    panic!("Could not find AddressSpaceInfo for width={}, type={:?}", width, info_type);
}

impl KAddressSpaceInfo {
    pub fn get_address_space_start(width: usize, info_type: AddressSpaceInfoType) -> u64 {
        get_address_space_info(width, info_type).address
    }

    pub fn get_address_space_size(width: usize, info_type: AddressSpaceInfoType) -> u64 {
        get_address_space_info(width, info_type).size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_address_space_info_32bit() {
        assert_eq!(
            KAddressSpaceInfo::get_address_space_start(32, AddressSpaceInfoType::MapSmall),
            2 * MI_B
        );
        assert_eq!(
            KAddressSpaceInfo::get_address_space_size(32, AddressSpaceInfoType::MapSmall),
            1 * GI_B - 2 * MI_B
        );
    }

    #[test]
    fn test_address_space_info_39bit() {
        assert_eq!(
            KAddressSpaceInfo::get_address_space_size(39, AddressSpaceInfoType::Map39Bit),
            512 * GI_B - 128 * MI_B
        );
    }
}
