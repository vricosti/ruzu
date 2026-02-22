// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! NSO (Nintendo Switch Object) executable loader.
//!
//! NSO is the standard executable format for Switch games (found inside ExeFS).
//! It contains three segments (text, rodata, data) that may be LZ4-compressed,
//! with optional SHA-256 hash verification per segment.

use crate::nro::CodeSet;
use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;
use thiserror::Error;

use ruzu_common::{align_up, PAGE_SIZE_U64};

/// NSO0 magic.
const NSO_MAGIC: u32 = u32::from_le_bytes([b'N', b'S', b'O', b'0']);

/// NSO header size.
const NSO_HEADER_SIZE: usize = 0x100;

/// Flag bit: text segment is compressed.
const FLAG_TEXT_COMPRESS: u32 = 1 << 0;
/// Flag bit: rodata segment is compressed.
const FLAG_RODATA_COMPRESS: u32 = 1 << 1;
/// Flag bit: data segment is compressed.
const FLAG_DATA_COMPRESS: u32 = 1 << 2;
/// Flag bit: verify text SHA-256 hash.
const FLAG_TEXT_HASH: u32 = 1 << 3;
/// Flag bit: verify rodata SHA-256 hash.
const FLAG_RODATA_HASH: u32 = 1 << 4;
/// Flag bit: verify data SHA-256 hash.
const FLAG_DATA_HASH: u32 = 1 << 5;

/// Segment index constants.
const SEG_TEXT: usize = 0;
const SEG_RODATA: usize = 1;
const SEG_DATA: usize = 2;

/// Errors from NSO loading.
#[derive(Debug, Error)]
pub enum NsoError {
    #[error("NSO data too small: need at least {NSO_HEADER_SIZE} bytes, got {0}")]
    DataTooSmall(usize),

    #[error("invalid NSO magic: expected 0x{:08X}, got 0x{actual:08X}", NSO_MAGIC)]
    InvalidMagic { actual: u32 },

    #[error("segment {index} file data out of bounds: offset={offset}, size={size}, file_len={file_len}")]
    SegmentOutOfBounds {
        index: usize,
        offset: u32,
        size: u32,
        file_len: usize,
    },

    #[error("LZ4 decompression failed for segment {index}: {detail}")]
    DecompressFailed { index: usize, detail: String },

    #[error("SHA-256 hash mismatch for segment {index}")]
    HashMismatch { index: usize },

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}

/// Parsed NSO segment descriptor from the header.
#[derive(Debug, Clone)]
struct NsoSegmentInfo {
    /// Memory offset where this segment is loaded.
    memory_offset: u32,
    /// File offset of the (possibly compressed) data.
    file_offset: u32,
    /// Decompressed size in memory.
    decompressed_size: u32,
    /// Compressed size in the file (0 if uncompressed).
    compressed_size: u32,
    /// Whether this segment is LZ4-compressed.
    is_compressed: bool,
    /// Whether to verify the SHA-256 hash.
    check_hash: bool,
    /// Expected SHA-256 hash (32 bytes).
    hash: [u8; 32],
}

/// Load an NSO executable from raw bytes, producing a [`CodeSet`].
///
/// Each segment is decompressed (if flagged) and optionally hash-verified.
/// The resulting CodeSet has the same structure as the NRO loader output.
pub fn load_nso(data: &[u8]) -> Result<CodeSet, NsoError> {
    if data.len() < NSO_HEADER_SIZE {
        return Err(NsoError::DataTooSmall(data.len()));
    }

    let mut cur = Cursor::new(data);

    // [0x00] magic
    let magic = cur.read_u32::<LittleEndian>()?;
    if magic != NSO_MAGIC {
        return Err(NsoError::InvalidMagic { actual: magic });
    }

    // [0x04] version
    let _version = cur.read_u32::<LittleEndian>()?;

    // [0x08] reserved
    let _reserved = cur.read_u32::<LittleEndian>()?;

    // [0x0C] flags
    let flags = cur.read_u32::<LittleEndian>()?;

    // Segment headers: [text, rodata, data]
    // Each is: memory_offset(4) + file_offset(4) + decompressed_size(4) = 12 bytes
    // [0x10] text segment header
    let text_mem_offset = cur.read_u32::<LittleEndian>()?;
    let text_file_offset = cur.read_u32::<LittleEndian>()?;
    let text_decompressed_size = cur.read_u32::<LittleEndian>()?;

    // [0x1C] module_name_offset
    let _module_name_offset = cur.read_u32::<LittleEndian>()?;

    // [0x20] rodata segment header
    let rodata_mem_offset = cur.read_u32::<LittleEndian>()?;
    let rodata_file_offset = cur.read_u32::<LittleEndian>()?;
    let rodata_decompressed_size = cur.read_u32::<LittleEndian>()?;

    // [0x2C] module_name_size
    let _module_name_size = cur.read_u32::<LittleEndian>()?;

    // [0x30] data segment header
    let data_mem_offset = cur.read_u32::<LittleEndian>()?;
    let data_file_offset = cur.read_u32::<LittleEndian>()?;
    let data_decompressed_size = cur.read_u32::<LittleEndian>()?;

    // [0x3C] BSS size
    let bss_size = cur.read_u32::<LittleEndian>()?;

    // [0x40] build_id (32 bytes, skip)
    for _ in 0..8 {
        cur.read_u32::<LittleEndian>()?;
    }

    // [0x60] compressed sizes
    let text_compressed_size = cur.read_u32::<LittleEndian>()?;
    let rodata_compressed_size = cur.read_u32::<LittleEndian>()?;
    let data_compressed_size = cur.read_u32::<LittleEndian>()?;

    // [0x6C] padding (28 bytes)
    for _ in 0..7 {
        cur.read_u32::<LittleEndian>()?;
    }

    // [0x88] api_info (rodata-relative offset + size, 8 bytes)
    let _api_info_offset = cur.read_u32::<LittleEndian>()?;
    let _api_info_size = cur.read_u32::<LittleEndian>()?;

    // [0x90] dynstr (rodata-relative offset + size, 8 bytes)
    let _dynstr_offset = cur.read_u32::<LittleEndian>()?;
    let _dynstr_size = cur.read_u32::<LittleEndian>()?;

    // [0x98] dynsym (rodata-relative offset + size, 8 bytes)
    let _dynsym_offset = cur.read_u32::<LittleEndian>()?;
    let _dynsym_size = cur.read_u32::<LittleEndian>()?;

    // [0xA0] SHA-256 hashes: text(32) + rodata(32) + data(32)
    let mut text_hash = [0u8; 32];
    let mut rodata_hash = [0u8; 32];
    let mut data_hash = [0u8; 32];
    for b in &mut text_hash {
        *b = cur.read_u8()?;
    }
    for b in &mut rodata_hash {
        *b = cur.read_u8()?;
    }
    for b in &mut data_hash {
        *b = cur.read_u8()?;
    }

    let segments_info = [
        NsoSegmentInfo {
            memory_offset: text_mem_offset,
            file_offset: text_file_offset,
            decompressed_size: text_decompressed_size,
            compressed_size: text_compressed_size,
            is_compressed: flags & FLAG_TEXT_COMPRESS != 0,
            check_hash: flags & FLAG_TEXT_HASH != 0,
            hash: text_hash,
        },
        NsoSegmentInfo {
            memory_offset: rodata_mem_offset,
            file_offset: rodata_file_offset,
            decompressed_size: rodata_decompressed_size,
            compressed_size: rodata_compressed_size,
            is_compressed: flags & FLAG_RODATA_COMPRESS != 0,
            check_hash: flags & FLAG_RODATA_HASH != 0,
            hash: rodata_hash,
        },
        NsoSegmentInfo {
            memory_offset: data_mem_offset,
            file_offset: data_file_offset,
            decompressed_size: data_decompressed_size,
            compressed_size: data_compressed_size,
            is_compressed: flags & FLAG_DATA_COMPRESS != 0,
            check_hash: flags & FLAG_DATA_HASH != 0,
            hash: data_hash,
        },
    ];

    // Decompress each segment
    let mut decompressed = [Vec::new(), Vec::new(), Vec::new()];
    for (i, seg) in segments_info.iter().enumerate() {
        let file_size = if seg.is_compressed {
            seg.compressed_size
        } else {
            seg.decompressed_size
        };

        let start = seg.file_offset as usize;
        let end = start + file_size as usize;
        if end > data.len() {
            return Err(NsoError::SegmentOutOfBounds {
                index: i,
                offset: seg.file_offset,
                size: file_size,
                file_len: data.len(),
            });
        }

        let raw = &data[start..end];

        let segment_data = if seg.is_compressed {
            lz4_flex::decompress(raw, seg.decompressed_size as usize).map_err(|e| {
                NsoError::DecompressFailed {
                    index: i,
                    detail: e.to_string(),
                }
            })?
        } else {
            raw.to_vec()
        };

        // Verify hash if flagged
        if seg.check_hash {
            use sha2::{Digest, Sha256};
            let computed = Sha256::digest(&segment_data);
            if computed.as_slice() != seg.hash {
                return Err(NsoError::HashMismatch { index: i });
            }
        }

        decompressed[i] = segment_data;
    }

    // Build program image
    let page_align = |v: u32| align_up(v as u64, PAGE_SIZE_U64) as u32;

    let text_size = page_align(decompressed[SEG_TEXT].len() as u32);
    let rodata_size = page_align(decompressed[SEG_RODATA].len() as u32);
    let data_size = page_align(decompressed[SEG_DATA].len() as u32);
    let bss_aligned = page_align(bss_size);

    let total_size = (segments_info[SEG_DATA].memory_offset + data_size + bss_aligned) as usize;
    let mut memory = vec![0u8; total_size];

    // Copy segments into memory at their specified offsets
    let text_off = segments_info[SEG_TEXT].memory_offset as usize;
    memory[text_off..text_off + decompressed[SEG_TEXT].len()]
        .copy_from_slice(&decompressed[SEG_TEXT]);

    let rodata_off = segments_info[SEG_RODATA].memory_offset as usize;
    memory[rodata_off..rodata_off + decompressed[SEG_RODATA].len()]
        .copy_from_slice(&decompressed[SEG_RODATA]);

    let data_off = segments_info[SEG_DATA].memory_offset as usize;
    memory[data_off..data_off + decompressed[SEG_DATA].len()]
        .copy_from_slice(&decompressed[SEG_DATA]);

    use crate::nro::CodeSegment;

    let segments = [
        CodeSegment {
            addr: segments_info[SEG_TEXT].memory_offset,
            offset: segments_info[SEG_TEXT].memory_offset,
            size: text_size,
        },
        CodeSegment {
            addr: segments_info[SEG_RODATA].memory_offset,
            offset: segments_info[SEG_RODATA].memory_offset,
            size: rodata_size,
        },
        CodeSegment {
            addr: segments_info[SEG_DATA].memory_offset,
            offset: segments_info[SEG_DATA].memory_offset,
            size: data_size + bss_aligned,
        },
    ];

    Ok(CodeSet {
        segments,
        memory,
        bss_size: bss_aligned,
        is_homebrew: false,
        asset_header: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a minimal NSO binary for testing.
    fn build_nso(
        text: &[u8],
        rodata: &[u8],
        data_seg: &[u8],
        bss_size: u32,
        compress: bool,
    ) -> Vec<u8> {
        let compress_segment = |input: &[u8]| -> Vec<u8> {
            if compress {
                lz4_flex::compress_prepend_size(input)
            } else {
                input.to_vec()
            }
        };

        // For lz4_flex compress_prepend_size, the decompressor needs
        // decompress_size_prepended. But our NSO loader uses decompress()
        // with an explicit size. So we use compress() without size prefix.
        let text_compressed = if compress {
            lz4_flex::compress(text)
        } else {
            text.to_vec()
        };
        let rodata_compressed = if compress {
            lz4_flex::compress(rodata)
        } else {
            rodata.to_vec()
        };
        let data_compressed = if compress {
            lz4_flex::compress(data_seg)
        } else {
            data_seg.to_vec()
        };

        let mut flags: u32 = 0;
        if compress {
            flags |= FLAG_TEXT_COMPRESS | FLAG_RODATA_COMPRESS | FLAG_DATA_COMPRESS;
        }

        // Compute SHA-256 hashes
        use sha2::{Digest, Sha256};
        let text_hash: [u8; 32] = Sha256::digest(text).into();
        let rodata_hash: [u8; 32] = Sha256::digest(rodata).into();
        let data_hash: [u8; 32] = Sha256::digest(data_seg).into();
        flags |= FLAG_TEXT_HASH | FLAG_RODATA_HASH | FLAG_DATA_HASH;

        // Layout segments after header
        let text_file_offset = NSO_HEADER_SIZE as u32;
        let rodata_file_offset = text_file_offset + text_compressed.len() as u32;
        let data_file_offset = rodata_file_offset + rodata_compressed.len() as u32;

        // Memory offsets: text at 0, rodata after text, data after rodata
        let text_mem_offset = 0u32;
        let rodata_mem_offset = align_up(text.len() as u64, PAGE_SIZE_U64) as u32;
        let data_mem_offset =
            rodata_mem_offset + align_up(rodata.len() as u64, PAGE_SIZE_U64) as u32;

        let mut buf = vec![0u8; NSO_HEADER_SIZE];

        let put_u32 = |buf: &mut Vec<u8>, off: usize, val: u32| {
            buf[off..off + 4].copy_from_slice(&val.to_le_bytes());
        };

        put_u32(&mut buf, 0x00, NSO_MAGIC);
        put_u32(&mut buf, 0x04, 0); // version
        put_u32(&mut buf, 0x08, 0); // reserved
        put_u32(&mut buf, 0x0C, flags);

        // text segment header
        put_u32(&mut buf, 0x10, text_mem_offset);
        put_u32(&mut buf, 0x14, text_file_offset);
        put_u32(&mut buf, 0x18, text.len() as u32);

        put_u32(&mut buf, 0x1C, 0); // module_name_offset

        // rodata segment header
        put_u32(&mut buf, 0x20, rodata_mem_offset);
        put_u32(&mut buf, 0x24, rodata_file_offset);
        put_u32(&mut buf, 0x28, rodata.len() as u32);

        put_u32(&mut buf, 0x2C, 0); // module_name_size

        // data segment header
        put_u32(&mut buf, 0x30, data_mem_offset);
        put_u32(&mut buf, 0x34, data_file_offset);
        put_u32(&mut buf, 0x38, data_seg.len() as u32);

        // BSS size
        put_u32(&mut buf, 0x3C, bss_size);

        // build_id (32 bytes) - skip, already zeroed

        // compressed sizes at 0x60
        put_u32(&mut buf, 0x60, text_compressed.len() as u32);
        put_u32(&mut buf, 0x64, rodata_compressed.len() as u32);
        put_u32(&mut buf, 0x68, data_compressed.len() as u32);

        // padding (28 bytes at 0x6C) - already zeroed

        // api_info, dynstr, dynsym - already zeroed

        // SHA-256 hashes at 0xA0
        buf[0xA0..0xC0].copy_from_slice(&text_hash);
        buf[0xC0..0xE0].copy_from_slice(&rodata_hash);
        buf[0xE0..0x100].copy_from_slice(&data_hash);

        // Append compressed segment data
        buf.extend_from_slice(&text_compressed);
        buf.extend_from_slice(&rodata_compressed);
        buf.extend_from_slice(&data_compressed);

        buf
    }

    #[test]
    fn test_load_nso_uncompressed() {
        let text = vec![0xCC; 256];
        let rodata = vec![0xAA; 128];
        let data_seg = vec![0xDD; 64];

        let nso_data = build_nso(&text, &rodata, &data_seg, 0x1000, false);
        let cs = load_nso(&nso_data).expect("load_nso should succeed");

        assert!(!cs.is_homebrew);
        assert_eq!(cs.segments[0].addr, 0);
        assert_eq!(cs.segments[0].size, align_up(256, PAGE_SIZE_U64) as u32);

        // Verify text content
        let t = &cs.segments[0];
        assert_eq!(&cs.memory[t.addr as usize..t.addr as usize + 256], &text[..]);
    }

    #[test]
    fn test_load_nso_compressed() {
        let text = vec![0xCC; 4096];
        let rodata = vec![0xAA; 2048];
        let data_seg = vec![0xDD; 1024];

        let nso_data = build_nso(&text, &rodata, &data_seg, 0, true);

        // Compressed NSO should be smaller than uncompressed
        assert!(nso_data.len() < NSO_HEADER_SIZE + 4096 + 2048 + 1024);

        let cs = load_nso(&nso_data).expect("load_nso compressed should succeed");

        // Verify decompressed text
        let t = &cs.segments[0];
        assert_eq!(
            &cs.memory[t.addr as usize..t.addr as usize + 4096],
            &text[..]
        );

        // Verify decompressed rodata
        let r = &cs.segments[1];
        assert_eq!(
            &cs.memory[r.addr as usize..r.addr as usize + 2048],
            &rodata[..]
        );
    }

    #[test]
    fn test_nso_invalid_magic() {
        let mut data = vec![0u8; NSO_HEADER_SIZE + 64];
        data[0..4].copy_from_slice(&[0xFF, 0xFF, 0xFF, 0xFF]);
        assert!(matches!(
            load_nso(&data),
            Err(NsoError::InvalidMagic { .. })
        ));
    }

    #[test]
    fn test_nso_too_small() {
        let data = vec![0u8; 16];
        assert!(matches!(load_nso(&data), Err(NsoError::DataTooSmall(_))));
    }

    #[test]
    fn test_nso_hash_verification() {
        let text = vec![0xCC; 256];
        let rodata = vec![0xAA; 128];
        let data_seg = vec![0xDD; 64];

        let mut nso_data = build_nso(&text, &rodata, &data_seg, 0, false);

        // Corrupt text segment data (after header)
        nso_data[NSO_HEADER_SIZE] ^= 0xFF;

        let result = load_nso(&nso_data);
        assert!(matches!(result, Err(NsoError::HashMismatch { index: 0 })));
    }

    #[test]
    fn test_nso_bss_region() {
        let text = vec![0xCC; 64];
        let rodata = vec![0xAA; 64];
        let data_seg = vec![0xDD; 64];

        let cs = load_nso(&build_nso(&text, &rodata, &data_seg, 0x2000, false)).unwrap();
        let bss_aligned = align_up(0x2000, PAGE_SIZE_U64) as u32;
        assert_eq!(cs.bss_size, bss_aligned);

        // Data segment size includes BSS
        let d = &cs.segments[2];
        assert!(d.size >= align_up(64, PAGE_SIZE_U64) as u32 + bss_aligned);
    }
}
