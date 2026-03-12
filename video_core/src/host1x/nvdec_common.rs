// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/nvdec_common.h`.
//!
//! Common NVDEC types: video codec enum, register offset structures, and the
//! NvdecRegisters union.

/// Video codec identifiers used by the NVDEC hardware.
///
/// Port of `Tegra::Host1x::NvdecCommon::VideoCodec`.
#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VideoCodec {
    None = 0x0,
    H264 = 0x3,
    VP8 = 0x5,
    H265 = 0x7,
    VP9 = 0x9,
}

impl From<u64> for VideoCodec {
    fn from(value: u64) -> Self {
        match value {
            0x0 => VideoCodec::None,
            0x3 => VideoCodec::H264,
            0x5 => VideoCodec::VP8,
            0x7 => VideoCodec::H265,
            0x9 => VideoCodec::VP9,
            _ => VideoCodec::None,
        }
    }
}

/// 64-bit offset that stores a shifted address.
///
/// Port of `Tegra::Host1x::NvdecCommon::Offset`.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Offset {
    offset: u64,
}

impl Offset {
    pub fn address(&self) -> u64 {
        self.offset << 8
    }
}

const _: () = assert!(std::mem::size_of::<Offset>() == 0x8);

/// NVDEC register file.
///
/// Port of `Tegra::Host1x::NvdecCommon::NvdecRegisters`.
///
/// NVDEC uses a 32-bit address space mapped to 64-bit, so all register slots
/// are 64-bit wide. The struct is 0xBC0 bytes (NUM_REGS * 8).
///
/// We represent it as a raw array and provide accessor methods matching the
/// upstream named fields at their byte offsets.
pub const NUM_REGS: usize = 0x178;

#[repr(C)]
#[derive(Clone)]
pub struct NvdecRegisters {
    pub reg_array: [u64; NUM_REGS],
}

impl Default for NvdecRegisters {
    fn default() -> Self {
        Self {
            reg_array: [0u64; NUM_REGS],
        }
    }
}

const _: () = assert!(std::mem::size_of::<NvdecRegisters>() == 0xBC0);

/// Macro to compute the register index from a byte offset, matching upstream
/// `NVDEC_REG_INDEX(field_name) = offsetof(...) / sizeof(u64)`.
macro_rules! reg_offset {
    ($byte_offset:expr) => {
        $byte_offset / 8
    };
}

// Register indices matching upstream field offsets.
// set_codec_id is at byte 0x400 => reg index 0x80
// execute is at byte 0x600 => reg index 0xC0
// control_params is at byte 0x800 => reg index 0x100
// picture_info_offset is at byte 0x808 => reg index 0x101
// frame_bitstream_offset is at byte 0x810 => reg index 0x102
// surface_luma_offsets[0] is at byte 0x860 => reg index 0x10C (17 elements)
// surface_chroma_offsets[0] is at byte 0x8E8 => reg index 0x11D (17 elements)
// vp9_prob_tab_buffer_offset is at byte 0xB80 => reg index 0x170

pub const REG_SET_CODEC_ID: usize = reg_offset!(0x400);
pub const REG_EXECUTE: usize = reg_offset!(0x600);
pub const REG_CONTROL_PARAMS: usize = reg_offset!(0x800);
pub const REG_PICTURE_INFO_OFFSET: usize = reg_offset!(0x808);
pub const REG_FRAME_BITSTREAM_OFFSET: usize = reg_offset!(0x810);
pub const REG_FRAME_NUMBER: usize = reg_offset!(0x818);
pub const REG_H264_SLICE_DATA_OFFSETS: usize = reg_offset!(0x820);
pub const REG_FRAME_STATS_OFFSET: usize = reg_offset!(0x848);
pub const REG_SURFACE_LUMA_OFFSETS: usize = reg_offset!(0x860);
pub const REG_SURFACE_CHROMA_OFFSETS: usize = reg_offset!(0x8E8);
pub const REG_VP8_PROB_DATA_OFFSET: usize = reg_offset!(0xA80);
pub const REG_VP9_PROB_TAB_BUFFER_OFFSET: usize = reg_offset!(0xB80);
pub const REG_VP9_CTX_COUNTER_BUFFER_OFFSET: usize = reg_offset!(0xB88);
pub const REG_VP9_SEGMENT_READ_BUFFER_OFFSET: usize = reg_offset!(0xB90);
pub const REG_VP9_SEGMENT_WRITE_BUFFER_OFFSET: usize = reg_offset!(0xB98);
pub const REG_VP9_COL_MVWRITE_BUFFER_OFFSET: usize = reg_offset!(0xBA8);
pub const REG_VP9_COL_MVREAD_BUFFER_OFFSET: usize = reg_offset!(0xBB0);

impl NvdecRegisters {
    /// Get the set_codec_id register value.
    pub fn set_codec_id(&self) -> VideoCodec {
        VideoCodec::from(self.reg_array[REG_SET_CODEC_ID])
    }

    /// Get the execute register value.
    pub fn execute(&self) -> u64 {
        self.reg_array[REG_EXECUTE]
    }

    /// Get picture_info_offset as an address.
    pub fn picture_info_offset(&self) -> Offset {
        Offset {
            offset: self.reg_array[REG_PICTURE_INFO_OFFSET],
        }
    }

    /// Get frame_bitstream_offset as an address.
    pub fn frame_bitstream_offset(&self) -> Offset {
        Offset {
            offset: self.reg_array[REG_FRAME_BITSTREAM_OFFSET],
        }
    }

    /// Access surface_luma_offsets array (17 elements starting at REG_SURFACE_LUMA_OFFSETS).
    pub fn surface_luma_offset(&self, index: usize) -> Offset {
        assert!(index < 17);
        Offset {
            offset: self.reg_array[REG_SURFACE_LUMA_OFFSETS + index],
        }
    }

    /// Access surface_chroma_offsets array (17 elements starting at REG_SURFACE_CHROMA_OFFSETS).
    pub fn surface_chroma_offset(&self, index: usize) -> Offset {
        assert!(index < 17);
        Offset {
            offset: self.reg_array[REG_SURFACE_CHROMA_OFFSETS + index],
        }
    }

    /// Get vp9_prob_tab_buffer_offset.
    pub fn vp9_prob_tab_buffer_offset(&self) -> Offset {
        Offset {
            offset: self.reg_array[REG_VP9_PROB_TAB_BUFFER_OFFSET],
        }
    }

    /// Get vp9_ctx_counter_buffer_offset.
    pub fn vp9_ctx_counter_buffer_offset(&self) -> Offset {
        Offset {
            offset: self.reg_array[REG_VP9_CTX_COUNTER_BUFFER_OFFSET],
        }
    }
}
