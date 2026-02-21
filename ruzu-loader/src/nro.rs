// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Nintendo Switch NRO (Nintendo Relocatable Object) file loader.
//!
//! The NRO format is used for homebrew applications on the Switch. It contains
//! three code segments (text, rodata, data) plus optional BSS, and may include
//! an ASET (asset) trailer with icon, NACP, and RomFS data.

use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;
use thiserror::Error;

use ruzu_common::{align_up, PAGE_SIZE_U64};

// ---------------------------------------------------------------------------
// Magic constants
// ---------------------------------------------------------------------------

/// NRO header magic: 'N', 'R', 'O', '0' in little-endian.
const NRO_MAGIC: u32 = u32::from_le_bytes([b'N', b'R', b'O', b'0']);

/// MOD0 header magic: 'M', 'O', 'D', '0' in little-endian.
const MOD0_MAGIC: u32 = u32::from_le_bytes([b'M', b'O', b'D', b'0']);

/// Homebrew magic_ext1: 'H', 'O', 'M', 'E' in little-endian.
const HOMEBREW_MAGIC_EXT1: u32 = u32::from_le_bytes([b'H', b'O', b'M', b'E']);

/// Homebrew magic_ext2: 'B', 'R', 'E', 'W' in little-endian.
const HOMEBREW_MAGIC_EXT2: u32 = u32::from_le_bytes([b'B', b'R', b'E', b'W']);

/// Asset header magic: 'A', 'S', 'E', 'T' in little-endian.
const ASET_MAGIC: u32 = u32::from_le_bytes([b'A', b'S', b'E', b'T']);

// ---------------------------------------------------------------------------
// Header sizes
// ---------------------------------------------------------------------------

/// Size of the NRO header in bytes.
const NRO_HEADER_SIZE: usize = 0x80;

/// Size of the MOD0 header in bytes.
const MOD_HEADER_SIZE: usize = 0x1C;

/// Size of the asset header in bytes.
const ASSET_HEADER_SIZE: usize = 0x38;

// ---------------------------------------------------------------------------
// Segment indices
// ---------------------------------------------------------------------------

/// Text (code) segment index.
pub const SEG_TEXT: usize = 0;

/// Read-only data segment index.
pub const SEG_RODATA: usize = 1;

/// Read-write data segment index.
pub const SEG_DATA: usize = 2;

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

/// Errors that can occur while loading an NRO file.
#[derive(Debug, Error)]
pub enum NroError {
    #[error("NRO data too small: expected at least {expected} bytes, got {actual}")]
    DataTooSmall { expected: usize, actual: usize },

    #[error("invalid NRO magic: expected 0x{expected:08X}, got 0x{actual:08X}")]
    InvalidMagic { expected: u32, actual: u32 },

    #[error("NRO header file_size ({header_size}) exceeds actual data length ({data_len})")]
    FileSizeMismatch { header_size: u32, data_len: usize },

    #[error("segment {index} extends past file_size: offset={offset}, size={size}, file_size={file_size}")]
    SegmentOutOfBounds {
        index: usize,
        offset: u32,
        size: u32,
        file_size: u32,
    },

    #[error("module_header_offset {offset} is out of bounds (file_size={file_size})")]
    ModHeaderOutOfBounds { offset: u32, file_size: u32 },

    #[error("I/O error reading NRO: {0}")]
    Io(#[from] std::io::Error),
}

// ---------------------------------------------------------------------------
// Raw header structs (parsed from binary, not repr(C) / Pod)
// ---------------------------------------------------------------------------

/// A single segment descriptor within the NRO header (offset + size, 8 bytes).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NroSegmentHeader {
    pub offset: u32,
    pub size: u32,
}

/// Parsed NRO header (0x80 bytes on disk).
#[derive(Debug, Clone)]
pub struct NroHeader {
    pub module_header_offset: u32,
    pub magic_ext1: u32,
    pub magic_ext2: u32,
    pub magic: u32,
    pub file_size: u32,
    pub segments: [NroSegmentHeader; 3],
    pub bss_size: u32,
}

/// Parsed MOD0 header (0x1C bytes on disk).
#[derive(Debug, Clone)]
pub struct ModHeader {
    pub magic: u32,
    pub dynamic_offset: u32,
    pub bss_start_offset: u32,
    pub bss_end_offset: u32,
    pub unwind_start_offset: u32,
    pub unwind_end_offset: u32,
    pub module_offset: u32,
}

/// A single asset section descriptor (offset + size, 16 bytes).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AssetSection {
    pub offset: u64,
    pub size: u64,
}

/// Parsed ASET header (0x38 bytes on disk), present after the NRO data.
#[derive(Debug, Clone)]
pub struct AssetHeader {
    pub magic: u32,
    pub format_version: u32,
    pub icon: AssetSection,
    pub nacp: AssetSection,
    pub romfs: AssetSection,
}

// ---------------------------------------------------------------------------
// Output types
// ---------------------------------------------------------------------------

/// A single segment within a [`CodeSet`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeSegment {
    /// Address (relative offset within the image) where this segment is mapped.
    pub addr: u32,
    /// Byte offset into [`CodeSet::memory`] where the segment data starts.
    pub offset: u32,
    /// Size of this segment in bytes (page-aligned).
    pub size: u32,
}

/// Loaded executable image produced by [`load_nro`].
///
/// Contains the three code segments (text, rodata, data) backed by a single
/// contiguous memory buffer that includes the BSS zero-fill region.
#[derive(Debug, Clone)]
pub struct CodeSet {
    /// Segment descriptors: \[text, rodata, data\].
    pub segments: [CodeSegment; 3],
    /// Full program image including BSS (zero-initialized).
    pub memory: Vec<u8>,
    /// BSS size in bytes (page-aligned).
    pub bss_size: u32,
    /// Whether this NRO is a homebrew application.
    pub is_homebrew: bool,
    /// Parsed asset header, if present after the NRO data.
    pub asset_header: Option<AssetHeader>,
}

impl CodeSet {
    /// Convenience accessor for the text (code) segment.
    #[inline]
    pub fn text_segment(&self) -> &CodeSegment {
        &self.segments[SEG_TEXT]
    }

    /// Convenience accessor for the read-only data segment.
    #[inline]
    pub fn rodata_segment(&self) -> &CodeSegment {
        &self.segments[SEG_RODATA]
    }

    /// Convenience accessor for the data segment.
    #[inline]
    pub fn data_segment(&self) -> &CodeSegment {
        &self.segments[SEG_DATA]
    }
}

// ---------------------------------------------------------------------------
// Parsing helpers
// ---------------------------------------------------------------------------

/// Read an [`NroSegmentHeader`] from a cursor.
fn read_segment_header(cur: &mut Cursor<&[u8]>) -> Result<NroSegmentHeader, NroError> {
    Ok(NroSegmentHeader {
        offset: cur.read_u32::<LittleEndian>()?,
        size: cur.read_u32::<LittleEndian>()?,
    })
}

/// Read an [`AssetSection`] from a cursor.
fn read_asset_section(cur: &mut Cursor<&[u8]>) -> Result<AssetSection, NroError> {
    Ok(AssetSection {
        offset: cur.read_u64::<LittleEndian>()?,
        size: cur.read_u64::<LittleEndian>()?,
    })
}

/// Parse the NRO header from the first 0x80 bytes of `data`.
fn parse_nro_header(data: &[u8]) -> Result<NroHeader, NroError> {
    if data.len() < NRO_HEADER_SIZE {
        return Err(NroError::DataTooSmall {
            expected: NRO_HEADER_SIZE,
            actual: data.len(),
        });
    }

    let mut cur = Cursor::new(data);

    // [0x00] 4 bytes padding
    cur.read_u32::<LittleEndian>()?;

    // [0x04] module_header_offset
    let module_header_offset = cur.read_u32::<LittleEndian>()?;

    // [0x08] magic_ext1 (HOME for homebrew)
    let magic_ext1 = cur.read_u32::<LittleEndian>()?;

    // [0x0C] magic_ext2 (BREW for homebrew)
    let magic_ext2 = cur.read_u32::<LittleEndian>()?;

    // [0x10] magic (NRO0)
    let magic = cur.read_u32::<LittleEndian>()?;

    // [0x14] 4 bytes padding
    cur.read_u32::<LittleEndian>()?;

    // [0x18] file_size
    let file_size = cur.read_u32::<LittleEndian>()?;

    // [0x1C] 4 bytes padding
    cur.read_u32::<LittleEndian>()?;

    // [0x20] segments[3] (text, rodata, data) -- each 8 bytes = 24 bytes total
    let segments = [
        read_segment_header(&mut cur)?,
        read_segment_header(&mut cur)?,
        read_segment_header(&mut cur)?,
    ];

    // [0x38] bss_size
    let bss_size = cur.read_u32::<LittleEndian>()?;

    // Remaining 0x44 bytes of padding are skipped (not needed).

    Ok(NroHeader {
        module_header_offset,
        magic_ext1,
        magic_ext2,
        magic,
        file_size,
        segments,
        bss_size,
    })
}

/// Parse the MOD0 header at the given offset in `data`.
///
/// Returns `None` if the magic does not match `MOD0`.
fn parse_mod_header(data: &[u8], offset: u32) -> Result<Option<ModHeader>, NroError> {
    let start = offset as usize;
    let end = start + MOD_HEADER_SIZE;
    if end > data.len() {
        return Ok(None);
    }

    let mut cur = Cursor::new(&data[start..end]);

    let magic = cur.read_u32::<LittleEndian>()?;
    if magic != MOD0_MAGIC {
        return Ok(None);
    }

    Ok(Some(ModHeader {
        magic,
        dynamic_offset: cur.read_u32::<LittleEndian>()?,
        bss_start_offset: cur.read_u32::<LittleEndian>()?,
        bss_end_offset: cur.read_u32::<LittleEndian>()?,
        unwind_start_offset: cur.read_u32::<LittleEndian>()?,
        unwind_end_offset: cur.read_u32::<LittleEndian>()?,
        module_offset: cur.read_u32::<LittleEndian>()?,
    }))
}

/// Parse the ASET header located at `nro_file_size` bytes into `data`.
///
/// Returns `None` if there is no room for an asset header or the magic does
/// not match.
fn parse_asset_header(data: &[u8], nro_file_size: u32) -> Result<Option<AssetHeader>, NroError> {
    let start = nro_file_size as usize;
    if data.len() < start + ASSET_HEADER_SIZE {
        return Ok(None);
    }

    let mut cur = Cursor::new(&data[start..start + ASSET_HEADER_SIZE]);

    let magic = cur.read_u32::<LittleEndian>()?;
    if magic != ASET_MAGIC {
        return Ok(None);
    }

    let format_version = cur.read_u32::<LittleEndian>()?;
    if format_version != 0 {
        log::warn!(
            "NRO asset header has format version {}, expected 0. \
             Metadata may not parse correctly.",
            format_version
        );
    }

    let icon = read_asset_section(&mut cur)?;
    let nacp = read_asset_section(&mut cur)?;
    let romfs = read_asset_section(&mut cur)?;

    Ok(Some(AssetHeader {
        magic,
        format_version,
        icon,
        nacp,
        romfs,
    }))
}

/// Page-align a size value upward.
#[inline]
fn page_align(size: u32) -> u32 {
    align_up(size as u64, PAGE_SIZE_U64) as u32
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Load an NRO file from raw bytes, producing a [`CodeSet`] ready for the
/// kernel to map into a process address space.
///
/// The returned `CodeSet` contains:
/// - Page-aligned segment descriptors for text, rodata, and data.
/// - A contiguous `memory` buffer with the program image and zero-filled BSS.
/// - Homebrew detection via the `HOME`/`BREW` magic fields.
/// - An optional parsed asset header (icon, NACP, RomFS offsets).
///
/// # Errors
///
/// Returns [`NroError`] if the data is malformed, too small, or fails magic
/// validation.
pub fn load_nro(data: &[u8]) -> Result<CodeSet, NroError> {
    // -- Parse and validate NRO header -----------------------------------

    let header = parse_nro_header(data)?;

    if header.magic != NRO_MAGIC {
        return Err(NroError::InvalidMagic {
            expected: NRO_MAGIC,
            actual: header.magic,
        });
    }

    if (header.file_size as usize) > data.len() {
        return Err(NroError::FileSizeMismatch {
            header_size: header.file_size,
            data_len: data.len(),
        });
    }

    // Validate segment bounds against file_size.
    for (i, seg) in header.segments.iter().enumerate() {
        let end = seg.offset.checked_add(seg.size).unwrap_or(u32::MAX);
        if end > header.file_size {
            return Err(NroError::SegmentOutOfBounds {
                index: i,
                offset: seg.offset,
                size: seg.size,
                file_size: header.file_size,
            });
        }
    }

    // -- Detect homebrew -------------------------------------------------

    let is_homebrew =
        header.magic_ext1 == HOMEBREW_MAGIC_EXT1 && header.magic_ext2 == HOMEBREW_MAGIC_EXT2;

    // -- Build program image (page-aligned copy of NRO data) -------------

    let aligned_file_size = page_align(header.file_size) as usize;
    let mut program_image = vec![0u8; aligned_file_size];
    let copy_len = std::cmp::min(header.file_size as usize, data.len());
    program_image[..copy_len].copy_from_slice(&data[..copy_len]);

    // -- Build segment descriptors ---------------------------------------

    let mut segments = [
        CodeSegment {
            addr: header.segments[SEG_TEXT].offset,
            offset: header.segments[SEG_TEXT].offset,
            size: page_align(header.segments[SEG_TEXT].size),
        },
        CodeSegment {
            addr: header.segments[SEG_RODATA].offset,
            offset: header.segments[SEG_RODATA].offset,
            size: page_align(header.segments[SEG_RODATA].size),
        },
        CodeSegment {
            addr: header.segments[SEG_DATA].offset,
            offset: header.segments[SEG_DATA].offset,
            size: page_align(header.segments[SEG_DATA].size),
        },
    ];

    // -- Determine BSS size (prefer MOD0 if valid) -----------------------

    let mut bss_size = page_align(header.bss_size);

    if (header.module_header_offset as usize) + MOD_HEADER_SIZE <= header.file_size as usize {
        if let Some(mod_header) = parse_mod_header(data, header.module_header_offset)? {
            if mod_header.bss_end_offset >= mod_header.bss_start_offset {
                bss_size = page_align(mod_header.bss_end_offset - mod_header.bss_start_offset);
            }
        }
    }

    // Extend the data segment to include BSS and grow the program image.
    segments[SEG_DATA].size += bss_size;
    program_image.resize(program_image.len() + bss_size as usize, 0);

    // -- Parse optional asset header -------------------------------------

    let asset_header = parse_asset_header(data, header.file_size)?;

    // -- Produce CodeSet -------------------------------------------------

    Ok(CodeSet {
        segments,
        memory: program_image,
        bss_size,
        is_homebrew,
        asset_header,
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: build a minimal valid NRO binary in a byte vector.
    ///
    /// Layout:
    /// - 0x00..0x80: NRO header
    /// - 0x80..0x80+text_size: text segment
    /// - text_end..text_end+rodata_size: rodata segment
    /// - rodata_end..rodata_end+data_size: data segment
    ///
    /// `bss_size` is written into the header but does not occupy file space.
    fn build_nro(
        text_size: u32,
        rodata_size: u32,
        data_size: u32,
        bss_size: u32,
        homebrew: bool,
        mod_header: Option<(u32, u32)>, // (bss_start, bss_end) offsets for MOD0
        append_asset: bool,
    ) -> Vec<u8> {
        let text_offset = NRO_HEADER_SIZE as u32;
        let rodata_offset = text_offset + text_size;
        let data_offset = rodata_offset + rodata_size;
        let file_size = data_offset + data_size;

        let mut buf = vec![0u8; file_size as usize];

        // Write NRO header using a helper closure for little-endian u32.
        let put_u32 = |buf: &mut Vec<u8>, offset: usize, val: u32| {
            buf[offset..offset + 4].copy_from_slice(&val.to_le_bytes());
        };

        // module_header_offset -- point to start of text where we may place MOD0
        let mod_offset = if mod_header.is_some() {
            text_offset
        } else {
            0
        };
        put_u32(&mut buf, 0x04, mod_offset);

        // Homebrew magic fields
        if homebrew {
            put_u32(&mut buf, 0x08, HOMEBREW_MAGIC_EXT1);
            put_u32(&mut buf, 0x0C, HOMEBREW_MAGIC_EXT2);
        }

        // NRO0 magic
        put_u32(&mut buf, 0x10, NRO_MAGIC);

        // file_size
        put_u32(&mut buf, 0x18, file_size);

        // segments[0] text
        put_u32(&mut buf, 0x20, text_offset);
        put_u32(&mut buf, 0x24, text_size);

        // segments[1] rodata
        put_u32(&mut buf, 0x28, rodata_offset);
        put_u32(&mut buf, 0x2C, rodata_size);

        // segments[2] data
        put_u32(&mut buf, 0x30, data_offset);
        put_u32(&mut buf, 0x34, data_size);

        // bss_size
        put_u32(&mut buf, 0x38, bss_size);

        // Fill text with a recognisable pattern (0xCC = INT3-like).
        for b in &mut buf[text_offset as usize..(text_offset + text_size) as usize] {
            *b = 0xCC;
        }

        // Fill rodata with 0xAA.
        for b in &mut buf[rodata_offset as usize..(rodata_offset + rodata_size) as usize] {
            *b = 0xAA;
        }

        // Fill data with 0xDD.
        for b in &mut buf[data_offset as usize..(data_offset + data_size) as usize] {
            *b = 0xDD;
        }

        // Optionally write MOD0 header at module_header_offset.
        if let Some((bss_start, bss_end)) = mod_header {
            let mo = mod_offset as usize;
            put_u32(&mut buf, mo, MOD0_MAGIC);
            put_u32(&mut buf, mo + 4, 0);         // dynamic_offset
            put_u32(&mut buf, mo + 8, bss_start); // bss_start_offset
            put_u32(&mut buf, mo + 12, bss_end);  // bss_end_offset
            put_u32(&mut buf, mo + 16, 0);        // unwind_start_offset
            put_u32(&mut buf, mo + 20, 0);        // unwind_end_offset
            put_u32(&mut buf, mo + 24, 0);        // module_offset
        }

        // Optionally append an ASET header after the NRO data.
        if append_asset {
            let asset_start = buf.len();
            buf.resize(asset_start + ASSET_HEADER_SIZE, 0);

            put_u32(&mut buf, asset_start, ASET_MAGIC);
            put_u32(&mut buf, asset_start + 4, 0); // format_version = 0

            // icon section: offset=0x38 (right after asset header), size=4
            let put_u64 = |buf: &mut Vec<u8>, offset: usize, val: u64| {
                buf[offset..offset + 8].copy_from_slice(&val.to_le_bytes());
            };
            put_u64(&mut buf, asset_start + 8, ASSET_HEADER_SIZE as u64); // icon offset
            put_u64(&mut buf, asset_start + 16, 4);                       // icon size

            // nacp section: offset=0x3C, size=8
            put_u64(&mut buf, asset_start + 24, ASSET_HEADER_SIZE as u64 + 4); // nacp offset
            put_u64(&mut buf, asset_start + 32, 8);                            // nacp size

            // romfs section: offset=0, size=0 (none)
            put_u64(&mut buf, asset_start + 40, 0);
            put_u64(&mut buf, asset_start + 48, 0);

            // Write dummy icon and nacp data so the file is large enough.
            buf.extend_from_slice(&[0x89, 0x50, 0x4E, 0x47]); // fake icon (4 bytes)
            buf.extend_from_slice(&[0x01; 8]);                 // fake nacp (8 bytes)
        }

        buf
    }

    #[test]
    fn test_minimal_nro() {
        let data = build_nro(0x100, 0x80, 0x40, 0x1000, false, None, false);
        let cs = load_nro(&data).expect("load_nro should succeed");

        assert!(!cs.is_homebrew);
        assert!(cs.asset_header.is_none());

        // Text segment
        assert_eq!(cs.segments[SEG_TEXT].offset, NRO_HEADER_SIZE as u32);
        assert_eq!(cs.segments[SEG_TEXT].addr, NRO_HEADER_SIZE as u32);
        assert_eq!(cs.segments[SEG_TEXT].size, page_align(0x100));

        // Rodata segment
        assert_eq!(cs.segments[SEG_RODATA].offset, NRO_HEADER_SIZE as u32 + 0x100);
        assert_eq!(cs.segments[SEG_RODATA].size, page_align(0x80));

        // Data segment: page_align(data_size) + bss_size
        assert_eq!(
            cs.segments[SEG_DATA].size,
            page_align(0x40) + page_align(0x1000)
        );

        // BSS
        assert_eq!(cs.bss_size, page_align(0x1000));

        // Memory contains file data + BSS zeros
        let expected_memory_len =
            page_align(NRO_HEADER_SIZE as u32 + 0x100 + 0x80 + 0x40) as usize
                + page_align(0x1000) as usize;
        assert_eq!(cs.memory.len(), expected_memory_len);

        // Verify text data pattern (0xCC)
        let text_start = cs.segments[SEG_TEXT].offset as usize;
        assert_eq!(cs.memory[text_start], 0xCC);
        assert_eq!(cs.memory[text_start + 0xFF], 0xCC);
    }

    #[test]
    fn test_homebrew_detection() {
        let data = build_nro(0x100, 0x80, 0x40, 0, true, None, false);
        let cs = load_nro(&data).expect("load_nro should succeed");
        assert!(cs.is_homebrew);
    }

    #[test]
    fn test_non_homebrew_detection() {
        let data = build_nro(0x100, 0x80, 0x40, 0, false, None, false);
        let cs = load_nro(&data).expect("load_nro should succeed");
        assert!(!cs.is_homebrew);
    }

    #[test]
    fn test_mod0_bss_override() {
        // NRO header says bss=0x1000, but MOD0 says bss=0x2000..0x4000 (size=0x2000).
        let data = build_nro(0x100, 0x80, 0x40, 0x1000, false, Some((0x2000, 0x4000)), false);
        let cs = load_nro(&data).expect("load_nro should succeed");

        let mod0_bss_size = page_align(0x4000 - 0x2000);
        assert_eq!(cs.bss_size, mod0_bss_size);
        assert_eq!(
            cs.segments[SEG_DATA].size,
            page_align(0x40) + mod0_bss_size
        );
    }

    #[test]
    fn test_asset_header_parsed() {
        let data = build_nro(0x100, 0x80, 0x40, 0, false, None, true);
        let cs = load_nro(&data).expect("load_nro should succeed");

        let asset = cs.asset_header.as_ref().expect("should have asset header");
        assert_eq!(asset.magic, ASET_MAGIC);
        assert_eq!(asset.format_version, 0);
        assert_eq!(asset.icon.offset, ASSET_HEADER_SIZE as u64);
        assert_eq!(asset.icon.size, 4);
        assert_eq!(asset.nacp.size, 8);
        assert_eq!(asset.romfs.size, 0);
    }

    #[test]
    fn test_no_asset_header_when_absent() {
        let data = build_nro(0x100, 0x80, 0x40, 0, false, None, false);
        let cs = load_nro(&data).expect("load_nro should succeed");
        assert!(cs.asset_header.is_none());
    }

    #[test]
    fn test_data_too_small() {
        let data = vec![0u8; 0x10]; // Way too small
        let err = load_nro(&data).unwrap_err();
        assert!(matches!(err, NroError::DataTooSmall { .. }));
    }

    #[test]
    fn test_invalid_magic() {
        let mut data = build_nro(0x100, 0x80, 0x40, 0, false, None, false);
        // Corrupt the NRO0 magic at offset 0x10.
        data[0x10] = 0x00;
        data[0x11] = 0x00;
        data[0x12] = 0x00;
        data[0x13] = 0x00;
        let err = load_nro(&data).unwrap_err();
        assert!(matches!(err, NroError::InvalidMagic { .. }));
    }

    #[test]
    fn test_file_size_mismatch() {
        let mut data = build_nro(0x100, 0x80, 0x40, 0, false, None, false);
        // Set file_size to something much larger than actual data.
        let huge: u32 = (data.len() as u32) * 10;
        data[0x18..0x1C].copy_from_slice(&huge.to_le_bytes());
        let err = load_nro(&data).unwrap_err();
        assert!(matches!(err, NroError::FileSizeMismatch { .. }));
    }

    #[test]
    fn test_segment_out_of_bounds() {
        let mut data = build_nro(0x100, 0x80, 0x40, 0, false, None, false);
        // Make text segment size absurdly large.
        let huge_seg_size: u32 = 0xFFFF_0000;
        data[0x24..0x28].copy_from_slice(&huge_seg_size.to_le_bytes());
        let err = load_nro(&data).unwrap_err();
        assert!(matches!(err, NroError::SegmentOutOfBounds { .. }));
    }

    #[test]
    fn test_zero_bss() {
        let data = build_nro(0x100, 0x80, 0x40, 0, false, None, false);
        let cs = load_nro(&data).expect("load_nro should succeed");
        assert_eq!(cs.bss_size, 0);
    }

    #[test]
    fn test_bss_region_is_zeroed() {
        let data = build_nro(0x1000, 0x1000, 0x1000, 0x2000, false, None, false);
        let cs = load_nro(&data).expect("load_nro should succeed");

        // The BSS region should be all zeros. It starts right after the
        // page-aligned file data.
        let file_aligned = page_align(NRO_HEADER_SIZE as u32 + 0x1000 + 0x1000 + 0x1000) as usize;
        let bss_region = &cs.memory[file_aligned..];
        assert!(bss_region.iter().all(|&b| b == 0));
    }

    #[test]
    fn test_segment_data_integrity() {
        let data = build_nro(0x100, 0x80, 0x40, 0, false, None, false);
        let cs = load_nro(&data).expect("load_nro should succeed");

        // Text bytes should be 0xCC.
        let t = &cs.segments[SEG_TEXT];
        for &b in &cs.memory[t.offset as usize..(t.offset + 0x100) as usize] {
            assert_eq!(b, 0xCC, "text segment should contain 0xCC pattern");
        }

        // Rodata bytes should be 0xAA.
        let r = &cs.segments[SEG_RODATA];
        for &b in &cs.memory[r.offset as usize..(r.offset + 0x80) as usize] {
            assert_eq!(b, 0xAA, "rodata segment should contain 0xAA pattern");
        }

        // Data bytes should be 0xDD.
        let d = &cs.segments[SEG_DATA];
        for &b in &cs.memory[d.offset as usize..(d.offset + 0x40) as usize] {
            assert_eq!(b, 0xDD, "data segment should contain 0xDD pattern");
        }
    }

    #[test]
    fn test_page_alignment() {
        // Use sizes that are not page-aligned to verify rounding.
        let data = build_nro(0x123, 0x456, 0x789, 0xABC, false, None, false);
        let cs = load_nro(&data).expect("load_nro should succeed");

        assert_eq!(cs.segments[SEG_TEXT].size, page_align(0x123));
        assert_eq!(cs.segments[SEG_RODATA].size, page_align(0x456));
        assert_eq!(
            cs.segments[SEG_DATA].size,
            page_align(0x789) + page_align(0xABC)
        );

        // Verify page alignment on each segment size.
        for seg in &cs.segments {
            assert_eq!(
                seg.size % (PAGE_SIZE_U64 as u32),
                0,
                "segment size 0x{:X} is not page-aligned",
                seg.size
            );
        }
    }

    #[test]
    fn test_homebrew_with_asset() {
        let data = build_nro(0x100, 0x80, 0x40, 0, true, None, true);
        let cs = load_nro(&data).expect("load_nro should succeed");
        assert!(cs.is_homebrew);
        assert!(cs.asset_header.is_some());
    }
}
