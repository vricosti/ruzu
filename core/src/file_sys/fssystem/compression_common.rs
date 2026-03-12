// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_compression_common.h

use common::ResultCode;

/// Compression type used in NCA compressed storage.
/// Corresponds to upstream `CompressionType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CompressionType {
    None = 0,
    Zeros = 1,
    Two = 2,
    Lz4 = 3,
    Unknown = 4,
}

/// Function pointer type for decompression.
/// Corresponds to upstream `DecompressorFunction`.
pub type DecompressorFunction = fn(dst: &mut [u8], src: &[u8]) -> Result<(), ResultCode>;

/// Function pointer type for getting a decompressor by compression type.
/// Corresponds to upstream `GetDecompressorFunction`.
pub type GetDecompressorFunction = fn(CompressionType) -> Option<DecompressorFunction>;

/// Block alignment required for compressed data.
pub const COMPRESSION_BLOCK_ALIGNMENT: i64 = 0x10;

/// Utility functions for compression type queries.
/// Corresponds to upstream `CompressionTypeUtility` namespace.
pub mod compression_type_utility {
    use super::CompressionType;

    pub const fn is_block_alignment_required(ct: CompressionType) -> bool {
        !matches!(ct, CompressionType::None | CompressionType::Zeros)
    }

    pub const fn is_data_storage_access_required(ct: CompressionType) -> bool {
        !matches!(ct, CompressionType::Zeros)
    }

    pub const fn is_random_accessible(ct: CompressionType) -> bool {
        matches!(ct, CompressionType::None)
    }

    pub const fn is_unknown_type(ct: CompressionType) -> bool {
        (ct as u8) >= (CompressionType::Unknown as u8)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compression_type_utility::*;

    #[test]
    fn test_is_block_alignment_required() {
        assert!(!is_block_alignment_required(CompressionType::None));
        assert!(!is_block_alignment_required(CompressionType::Zeros));
        assert!(is_block_alignment_required(CompressionType::Two));
        assert!(is_block_alignment_required(CompressionType::Lz4));
    }

    #[test]
    fn test_is_data_storage_access_required() {
        assert!(is_data_storage_access_required(CompressionType::None));
        assert!(!is_data_storage_access_required(CompressionType::Zeros));
        assert!(is_data_storage_access_required(CompressionType::Lz4));
    }

    #[test]
    fn test_is_random_accessible() {
        assert!(is_random_accessible(CompressionType::None));
        assert!(!is_random_accessible(CompressionType::Zeros));
        assert!(!is_random_accessible(CompressionType::Lz4));
    }

    #[test]
    fn test_is_unknown_type() {
        assert!(!is_unknown_type(CompressionType::None));
        assert!(!is_unknown_type(CompressionType::Lz4));
        assert!(is_unknown_type(CompressionType::Unknown));
    }

    #[test]
    fn test_compression_block_alignment() {
        assert_eq!(COMPRESSION_BLOCK_ALIGNMENT, 0x10);
    }
}
