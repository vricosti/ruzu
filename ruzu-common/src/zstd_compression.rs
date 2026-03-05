//! Port of zuyu/src/common/zstd_compression.h and zuyu/src/common/zstd_compression.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

/// Compresses a source memory region with Zstandard and returns the compressed data.
///
/// # Arguments
/// * `source` - The uncompressed source data.
/// * `compression_level` - The compression level (1-22). Will be clamped to valid range.
///
/// # Returns
/// The compressed data, or an empty vector on failure.
pub fn compress_data_zstd(source: &[u8], compression_level: i32) -> Vec<u8> {
    let level = compression_level.clamp(1, zstd::compression_level_range().end().to_owned());

    match zstd::encode_all(source, level) {
        Ok(compressed) => compressed,
        Err(e) => {
            log::error!("Zstd compression failed: {}", e);
            Vec::new()
        }
    }
}

/// Compresses with the default Zstd compression level (3).
pub fn compress_data_zstd_default(source: &[u8]) -> Vec<u8> {
    compress_data_zstd(source, zstd::DEFAULT_COMPRESSION_LEVEL)
}

/// Decompresses a source memory region with Zstandard and returns the uncompressed data.
///
/// # Returns
/// The decompressed data, or an empty vector on failure.
pub fn decompress_data_zstd(compressed: &[u8]) -> Vec<u8> {
    match zstd::decode_all(compressed) {
        Ok(decompressed) => decompressed,
        Err(e) => {
            log::error!("Zstd decompression failed: {}", e);
            Vec::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compress_decompress() {
        // Use a large enough string with enough repetition for compression to actually shrink it
        let original: Vec<u8> = b"Hello, World! This is a test of Zstd compression. "
            .iter()
            .copied()
            .cycle()
            .take(1024)
            .collect();
        let compressed = compress_data_zstd(&original, 3);
        assert!(!compressed.is_empty());
        assert!(compressed.len() < original.len());

        let decompressed = decompress_data_zstd(&compressed);
        assert_eq!(decompressed, original);
    }

    #[test]
    fn test_compress_default() {
        let original = b"Default compression level test data with some repetition BBBBBBBBBBBBB";
        let compressed = compress_data_zstd_default(original);
        assert!(!compressed.is_empty());

        let decompressed = decompress_data_zstd(&compressed);
        assert_eq!(&decompressed, original);
    }

    #[test]
    fn test_empty_data() {
        let compressed = compress_data_zstd(b"", 1);
        let decompressed = decompress_data_zstd(&compressed);
        assert!(decompressed.is_empty());
    }

    #[test]
    fn test_invalid_decompression() {
        let garbage = vec![0xFF, 0xFE, 0xFD, 0xFC];
        let result = decompress_data_zstd(&garbage);
        assert!(result.is_empty());
    }
}
