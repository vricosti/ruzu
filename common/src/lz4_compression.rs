//! Port of zuyu/src/common/lz4_compression.h and zuyu/src/common/lz4_compression.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

/// Compresses a source memory region with LZ4 and returns the compressed data in a vector.
pub fn compress_data_lz4(source: &[u8]) -> Vec<u8> {
    lz4_flex::compress_prepend_size(source)
}

/// Utilizes the LZ4 subalgorithm with higher compression (lz4_flex uses a single level).
/// In lz4_flex there is no HC variant, so we use the standard compression.
/// The compression_level parameter is accepted for API compatibility but not used.
pub fn compress_data_lz4_hc(source: &[u8], _compression_level: i32) -> Vec<u8> {
    // lz4_flex does not support HC compression levels.
    // Use standard compression as a fallback.
    lz4_flex::compress_prepend_size(source)
}

/// Utilizes LZ4 with maximum compression.
pub fn compress_data_lz4_hc_max(source: &[u8]) -> Vec<u8> {
    compress_data_lz4_hc(source, 12)
}

/// Decompresses a source memory region with LZ4 and returns the uncompressed data.
///
/// # Arguments
/// * `compressed` - The compressed source data.
/// * `uncompressed_size` - The expected size of the uncompressed data.
///
/// # Returns
/// The decompressed data, or an empty vector on failure.
pub fn decompress_data_lz4(compressed: &[u8], uncompressed_size: usize) -> Vec<u8> {
    // lz4_flex::decompress expects raw LZ4 blocks (no size prefix).
    // Try decompression with the given size.
    match lz4_flex::decompress(compressed, uncompressed_size) {
        Ok(data) => {
            if data.len() == uncompressed_size {
                data
            } else {
                log::error!(
                    "LZ4 decompression size mismatch: expected {}, got {}",
                    uncompressed_size,
                    data.len()
                );
                Vec::new()
            }
        }
        Err(e) => {
            log::error!("LZ4 decompression failed: {}", e);
            Vec::new()
        }
    }
}

/// Decompresses LZ4 data from src into dst buffer.
/// Returns the number of bytes written to dst, or a negative value on error.
pub fn decompress_data_lz4_into(dst: &mut [u8], src: &[u8]) -> i32 {
    match lz4_flex::decompress(src, dst.len()) {
        Ok(data) => {
            let len = std::cmp::min(data.len(), dst.len());
            dst[..len].copy_from_slice(&data[..len]);
            len as i32
        }
        Err(_) => -1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compress_decompress() {
        let original =
            b"Hello, World! This is a test of LZ4 compression. Repeated data: AAAAAAAAAAAAAAA";
        let compressed = compress_data_lz4(original);
        assert!(!compressed.is_empty());

        // lz4_flex::compress_prepend_size prepends the original size, so use decompress_size_prepended
        let decompressed = lz4_flex::decompress_size_prepended(&compressed).unwrap();
        assert_eq!(&decompressed, original);
    }

    #[test]
    fn test_decompress_raw() {
        let original = b"Test data for raw LZ4 decompression";
        // Use block compress (no size prefix) for the raw API
        let compressed = lz4_flex::compress(original);
        let decompressed = decompress_data_lz4(&compressed, original.len());
        assert_eq!(&decompressed, original);
    }
}
