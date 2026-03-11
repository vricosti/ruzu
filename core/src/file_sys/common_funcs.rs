// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/common_funcs.h

/// Mask for extracting the AOC (Add-On Content / DLC) title ID.
pub const AOC_TITLE_ID_MASK: u64 = 0x7FF;

/// Offset added to base title ID to get AOC base title ID.
pub const AOC_TITLE_ID_OFFSET: u64 = 0x1000;

/// Mask for extracting the base title ID from any title ID.
pub const BASE_TITLE_ID_MASK: u64 = 0xFFFF_FFFF_FFFF_E000;

/// Gets the base title ID from a given title ID.
#[inline]
pub const fn get_base_title_id(title_id: u64) -> u64 {
    title_id & BASE_TITLE_ID_MASK
}

/// Gets the base title ID with a program index offset from a given title ID.
#[inline]
pub const fn get_base_title_id_with_program_index(title_id: u64, program_index: u64) -> u64 {
    get_base_title_id(title_id) + program_index
}

/// Gets the AOC (Add-On Content) base title ID from a given title ID.
#[inline]
pub const fn get_aoc_base_title_id(title_id: u64) -> u64 {
    get_base_title_id(title_id) + AOC_TITLE_ID_OFFSET
}

/// Gets the AOC (Add-On Content) ID from a given AOC title ID.
#[inline]
pub const fn get_aoc_id(aoc_title_id: u64) -> u64 {
    aoc_title_id & AOC_TITLE_ID_MASK
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base_title_id() {
        // A typical title ID
        let title_id: u64 = 0x0100_0000_0000_1000;
        assert_eq!(get_base_title_id(title_id), 0x0100_0000_0000_0000);
    }

    #[test]
    fn test_aoc_base_title_id() {
        let title_id: u64 = 0x0100_0000_0000_0000;
        assert_eq!(get_aoc_base_title_id(title_id), 0x0100_0000_0000_1000);
    }

    #[test]
    fn test_aoc_id() {
        let aoc_title_id: u64 = 0x0100_0000_0000_1001;
        assert_eq!(get_aoc_id(aoc_title_id), 1);
    }

    #[test]
    fn test_base_title_id_with_program_index() {
        let title_id: u64 = 0x0100_0000_0000_1000;
        assert_eq!(
            get_base_title_id_with_program_index(title_id, 2),
            0x0100_0000_0000_0002
        );
    }
}
