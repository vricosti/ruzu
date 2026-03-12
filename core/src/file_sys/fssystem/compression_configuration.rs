// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/file_sys/fssystem/fssystem_compression_configuration.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-12
//!
//! Provides the global NCA compression configuration with decompressor functions.

use super::compression_common::{CompressionType, DecompressorFunction, GetDecompressorFunction};
use super::nca_file_system_driver::NcaCompressionConfiguration;
use crate::file_sys::errors;
use common::ResultCode;

/// Decompress LZ4-compressed data.
///
/// Corresponds to upstream `DecompressLz4`.
fn decompress_lz4(dst: &mut [u8], src: &[u8]) -> Result<(), ResultCode> {
    // Use the lz4_flex crate for decompression if available,
    // otherwise fall back to a basic implementation.
    #[cfg(feature = "lz4")]
    {
        let result = lz4_flex::decompress_into(src, dst);
        match result {
            Ok(size) if size == dst.len() => Ok(()),
            _ => Err(errors::RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_C), // ResultUnexpectedInCompressedStorageC
        }
    }
    #[cfg(not(feature = "lz4"))]
    {
        // Without LZ4 support, we cannot decompress.
        log::warn!(
            "LZ4 decompression requested but lz4 feature not enabled (src={}, dst={})",
            src.len(),
            dst.len()
        );
        let _ = (dst, src);
        Err(errors::RESULT_UNEXPECTED_IN_COMPRESSED_STORAGE_C) // ResultUnexpectedInCompressedStorageC
    }
}

/// Get the NCA decompressor function for the given compression type.
///
/// Corresponds to upstream `GetNcaDecompressorFunction`.
fn get_nca_decompressor_function(
    compression_type: CompressionType,
) -> Option<DecompressorFunction> {
    match compression_type {
        CompressionType::Lz4 => Some(decompress_lz4),
        _ => None,
    }
}

/// Get the global NCA compression configuration.
///
/// Corresponds to upstream `GetNcaCompressionConfiguration`.
pub fn get_nca_compression_configuration() -> NcaCompressionConfiguration {
    NcaCompressionConfiguration {
        get_decompressor: Some(get_nca_decompressor_function),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_nca_decompressor_lz4() {
        let func = get_nca_decompressor_function(CompressionType::Lz4);
        assert!(func.is_some());
    }

    #[test]
    fn test_get_nca_decompressor_none() {
        let func = get_nca_decompressor_function(CompressionType::None);
        assert!(func.is_none());
    }

    #[test]
    fn test_get_nca_decompressor_unknown() {
        let func = get_nca_decompressor_function(CompressionType::Unknown);
        assert!(func.is_none());
    }

    #[test]
    fn test_get_nca_compression_configuration() {
        let config = get_nca_compression_configuration();
        assert!(config.get_decompressor.is_some());
    }

    #[test]
    fn test_decompress_lz4_no_feature() {
        // Without the lz4 feature, decompress_lz4 should return an error.
        let mut dst = [0u8; 16];
        let src = [0u8; 8];
        let result = decompress_lz4(&mut dst, &src);
        // Whether we have the feature or not, this should either succeed
        // (with valid data) or fail. With invalid data, it should fail.
        assert!(result.is_err());
    }
}
