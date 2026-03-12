// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/decode_bc.h and decode_bc.cpp
//!
//! Software BCn block-compression decompression.  The upstream
//! implementation calls into `bc_decoder.h`; this port stubs the decode
//! calls and will be completed once a Rust BCn library is integrated.

use super::format_lookup_table::PixelFormat;
use super::types::BufferImageCopy;

// ── Constants ──────────────────────────────────────────────────────────

const BLOCK_SIZE: u32 = 4;

// ── Public API ─────────────────────────────────────────────────────────

/// Bytes per output texel after decompression for a given BCn format.
///
/// Port of `VideoCommon::ConvertedBytesPerBlock`.
pub fn converted_bytes_per_block(pixel_format: PixelFormat) -> u32 {
    match pixel_format {
        PixelFormat::Bc4Snorm | PixelFormat::Bc4Unorm => 1,
        PixelFormat::Bc5Snorm | PixelFormat::Bc5Unorm => 2,
        PixelFormat::Bc6hSfloat | PixelFormat::Bc6hUfloat => 8,
        _ => 4,
    }
}

/// Decompress a BCn-compressed image into `output`.
///
/// Port of `VideoCommon::DecompressBCn`.
///
/// NOTE: actual decompression calls are stubbed — requires a BCn decoder
/// crate (equivalent of `bc_decoder.h`).
pub fn decompress_bcn(
    _input: &[u8],
    _output: &mut [u8],
    _copy: &mut BufferImageCopy,
    pixel_format: PixelFormat,
) {
    match pixel_format {
        PixelFormat::Bc1RgbaUnorm | PixelFormat::Bc1RgbaSrgb => {
            todo!("DecompressBCn: BC1")
        }
        PixelFormat::Bc2Unorm | PixelFormat::Bc2Srgb => {
            todo!("DecompressBCn: BC2")
        }
        PixelFormat::Bc3Unorm | PixelFormat::Bc3Srgb => {
            todo!("DecompressBCn: BC3")
        }
        PixelFormat::Bc4Snorm | PixelFormat::Bc4Unorm => {
            todo!("DecompressBCn: BC4")
        }
        PixelFormat::Bc5Snorm | PixelFormat::Bc5Unorm => {
            todo!("DecompressBCn: BC5")
        }
        PixelFormat::Bc6hSfloat | PixelFormat::Bc6hUfloat => {
            todo!("DecompressBCn: BC6")
        }
        PixelFormat::Bc7Srgb | PixelFormat::Bc7Unorm => {
            todo!("DecompressBCn: BC7")
        }
        _ => {
            log::warn!("Unimplemented BCn decompression {:?}", pixel_format);
        }
    }
}

// ── Private helpers ────────────────────────────────────────────────────

/// Whether the format uses a signed decode path.
const fn is_signed(pf: PixelFormat) -> bool {
    matches!(
        pf,
        PixelFormat::Bc4Snorm
            | PixelFormat::Bc4Unorm
            | PixelFormat::Bc5Snorm
            | PixelFormat::Bc5Unorm
            | PixelFormat::Bc6hSfloat
            | PixelFormat::Bc6hUfloat
    )
}

/// Compressed block size in bytes.
const fn block_byte_size(pf: PixelFormat) -> u32 {
    match pf {
        PixelFormat::Bc1RgbaSrgb
        | PixelFormat::Bc1RgbaUnorm
        | PixelFormat::Bc4Snorm
        | PixelFormat::Bc4Unorm => 8,
        _ => 16,
    }
}
