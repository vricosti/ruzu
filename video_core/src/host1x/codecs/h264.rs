// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/codecs/h264.h` and `h264.cpp`.
//!
//! H.264 decoder implementation including the H264BitWriter for composing
//! SPS/PPS headers, and the H264 decoder struct.

use crate::host1x::codecs::decoder::{DecoderImpl, DecoderState};
use crate::host1x::nvdec_common::VideoCodec;

// --------------------------------------------------------------------------
// ZigZag LUTs from libavcodec (same as upstream).
// --------------------------------------------------------------------------

const ZIG_ZAG_DIRECT: [u8; 64] = [
    0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20,
    13, 6, 7, 14, 21, 28, 35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59,
    52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63,
];

const ZIG_ZAG_SCAN: [u8; 16] = [
    0 + 0 * 4,
    1 + 0 * 4,
    0 + 1 * 4,
    0 + 2 * 4,
    1 + 1 * 4,
    2 + 0 * 4,
    3 + 0 * 4,
    2 + 1 * 4,
    1 + 2 * 4,
    0 + 3 * 4,
    1 + 3 * 4,
    2 + 2 * 4,
    3 + 1 * 4,
    3 + 2 * 4,
    2 + 3 * 4,
    3 + 3 * 4,
];

// --------------------------------------------------------------------------
// H264 Offset (32-bit, shifted by 8)
// --------------------------------------------------------------------------

/// 32-bit offset that stores a shifted address, specific to H264 codec structures.
///
/// Port of `Tegra::Decoders::Offset` in `h264.h`.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Offset {
    offset: u32,
}

impl Offset {
    pub fn address(&self) -> u32 {
        self.offset << 8
    }
}

const _: () = assert!(std::mem::size_of::<Offset>() == 0x4);

// --------------------------------------------------------------------------
// H264ParameterSet — 0x60 bytes
// --------------------------------------------------------------------------

/// Port of `Tegra::Decoders::H264ParameterSet`.
#[repr(C)]
#[derive(Clone, Default)]
pub struct H264ParameterSet {
    pub log2_max_pic_order_cnt_lsb_minus4: i32,      // 0x00
    pub delta_pic_order_always_zero_flag: i32,       // 0x04
    pub frame_mbs_only_flag: i32,                    // 0x08
    pub pic_width_in_mbs: u32,                       // 0x0C
    pub frame_height_in_mbs: u32,                    // 0x10
    pub surface_format: u32, // 0x14 (bitfield: tile_format, gob_height, reserved)
    pub entropy_coding_mode_flag: u32, // 0x18
    pub pic_order_present_flag: i32, // 0x1C
    pub num_refidx_l0_default_active: i32, // 0x20
    pub num_refidx_l1_default_active: i32, // 0x24
    pub deblocking_filter_control_present_flag: i32, // 0x28
    pub redundant_pic_cnt_present_flag: i32, // 0x2C
    pub transform_8x8_mode_flag: u32, // 0x30
    pub pitch_luma: u32,     // 0x34
    pub pitch_chroma: u32,   // 0x38
    pub luma_top_offset: Offset, // 0x3C
    pub luma_bot_offset: Offset, // 0x40
    pub luma_frame_offset: Offset, // 0x44
    pub chroma_top_offset: Offset, // 0x48
    pub chroma_bot_offset: Offset, // 0x4C
    pub chroma_frame_offset: Offset, // 0x50
    pub hist_buffer_size: u32, // 0x54
    /// Bitfield union storage — stored as [u32; 2] to avoid forcing 8-byte
    /// alignment on the containing struct (upstream C++ uses u64 BitField
    /// inside a union, but sizeof(H264DecoderContext) == 0x2FC requires
    /// 4-byte struct alignment).
    pub flags_raw: [u32; 2], // 0x58 (bitfield union, logically u64)
}

const _: () = assert!(std::mem::size_of::<H264ParameterSet>() == 0x60);

impl H264ParameterSet {
    /// Reconstruct the logical u64 from the [u32; 2] storage.
    #[inline]
    fn flags(&self) -> u64 {
        self.flags_raw[0] as u64 | ((self.flags_raw[1] as u64) << 32)
    }

    // Bitfield accessors for flags_raw (at offset 0x58).
    pub fn mbaff_frame(&self) -> u64 {
        self.flags() & 1
    }
    pub fn direct_8x8_inference(&self) -> u64 {
        (self.flags() >> 1) & 1
    }
    pub fn weighted_pred(&self) -> u64 {
        (self.flags() >> 2) & 1
    }
    pub fn constrained_intra_pred(&self) -> u64 {
        (self.flags() >> 3) & 1
    }
    pub fn log2_max_frame_num_minus4(&self) -> u64 {
        (self.flags() >> 8) & 0xF
    }
    pub fn chroma_format_idc(&self) -> u64 {
        (self.flags() >> 12) & 0x3
    }
    pub fn pic_order_cnt_type(&self) -> u64 {
        (self.flags() >> 14) & 0x3
    }
    pub fn pic_init_qp_minus26(&self) -> i64 {
        // 6-bit signed field at bit 16
        let raw = ((self.flags() >> 16) & 0x3F) as i64;
        if raw & 0x20 != 0 {
            raw | !0x3F
        } else {
            raw
        }
    }
    pub fn chroma_qp_index_offset(&self) -> i64 {
        // 5-bit signed field at bit 22
        let raw = ((self.flags() >> 22) & 0x1F) as i64;
        if raw & 0x10 != 0 {
            raw | !0x1F
        } else {
            raw
        }
    }
    pub fn second_chroma_qp_index_offset(&self) -> i64 {
        // 5-bit signed field at bit 27
        let raw = ((self.flags() >> 27) & 0x1F) as i64;
        if raw & 0x10 != 0 {
            raw | !0x1F
        } else {
            raw
        }
    }
    pub fn weighted_bipred_idc(&self) -> u64 {
        (self.flags() >> 32) & 0x3
    }
    pub fn curr_pic_idx(&self) -> u64 {
        (self.flags() >> 34) & 0x7F
    }
    pub fn frame_number(&self) -> u64 {
        (self.flags() >> 46) & 0xFFFF
    }
}

// --------------------------------------------------------------------------
// DpbEntry — 0x10 bytes
// --------------------------------------------------------------------------

/// Port of `Tegra::Decoders::DpbEntry`.
#[repr(C)]
#[derive(Clone, Default)]
pub struct DpbEntry {
    pub flags: u32,
    pub field_order_cnt: [u32; 2],
    pub frame_idx: u32,
}

const _: () = assert!(std::mem::size_of::<DpbEntry>() == 0x10);

// --------------------------------------------------------------------------
// DisplayParam — 0x1C bytes
// --------------------------------------------------------------------------

/// Port of `Tegra::Decoders::DisplayParam`.
#[repr(C)]
#[derive(Clone, Default)]
pub struct DisplayParam {
    pub flags0: u32,
    pub output_top: [i32; 2],
    pub output_bottom: [i32; 2],
    pub histogram_flags1: u32,
    pub histogram_flags2: u32,
}

const _: () = assert!(std::mem::size_of::<DisplayParam>() == 0x1C);

// --------------------------------------------------------------------------
// H264DecoderContext — 0x2FC bytes
// --------------------------------------------------------------------------

/// Port of `Tegra::Decoders::H264DecoderContext`.
#[repr(C)]
#[derive(Clone)]
pub struct H264DecoderContext {
    pub reserved0: [u32; 13],                 // 0x0000
    pub eos: [u8; 16],                        // 0x0034
    pub explicit_eos_present_flag: u8,        // 0x0044
    pub hint_dump_en: u8,                     // 0x0045
    pub _pad0: [u8; 2],                       // 0x0046
    pub stream_len: u32,                      // 0x0048
    pub slice_count: u32,                     // 0x004C
    pub mbhist_buffer_size: u32,              // 0x0050
    pub gptimer_timeout_value: u32,           // 0x0054
    pub h264_parameter_set: H264ParameterSet, // 0x0058
    pub curr_field_order_cnt: [i32; 2],       // 0x00B8
    pub dpb: [DpbEntry; 16],                  // 0x00C0
    pub weight_scale_4x4: [u8; 0x60],         // 0x01C0
    pub weight_scale_8x8: [u8; 0x80],         // 0x0220
    pub num_inter_view_refs_lx: [u8; 2],      // 0x02A0
    pub reserved2: [u8; 14],                  // 0x02A2
    pub inter_view_refidx_lx: [[i8; 16]; 2],  // 0x02B0
    pub lossless_flags: u32,                  // 0x02D0 (bitfield)
    pub display_param: DisplayParam,          // 0x02D4
    pub reserved4: [u32; 3],                  // 0x02F0
}

const _: () = assert!(std::mem::size_of::<H264DecoderContext>() == 0x2FC);

impl Default for H264DecoderContext {
    fn default() -> Self {
        // Safety: zeroed representation is valid for this C-layout struct.
        unsafe { std::mem::zeroed() }
    }
}

impl H264DecoderContext {
    pub fn qpprime_y_zero_transform_bypass_flag(&self) -> u32 {
        (self.lossless_flags >> 1) & 1
    }
}

// --------------------------------------------------------------------------
// H264BitWriter
// --------------------------------------------------------------------------

/// Bitstream writer for composing H.264 NAL units.
///
/// Port of `Tegra::Decoders::H264BitWriter`.
pub struct H264BitWriter {
    buffer_size: i32,
    buffer: i32,
    buffer_pos: i32,
    byte_array: Vec<u8>,
}

impl H264BitWriter {
    pub fn new() -> Self {
        Self {
            buffer_size: 8,
            buffer: 0,
            buffer_pos: 0,
            byte_array: Vec::new(),
        }
    }

    /// Writes value_sz bits from value into the stream.
    pub fn write_u(&mut self, value: i32, value_sz: i32) {
        self.write_bits(value, value_sz);
    }

    /// Writes a signed Exp-Golomb coded integer.
    pub fn write_se(&mut self, value: i32) {
        self.write_exp_golomb_coded_int(value);
    }

    /// Writes an unsigned Exp-Golomb coded integer.
    pub fn write_ue(&mut self, value: u32) {
        self.write_exp_golomb_coded_uint(value);
    }

    /// Finalize the bitstream.
    pub fn end(&mut self) {
        self.write_bit(true);
        self.flush();
    }

    /// Append a single bit to the stream.
    pub fn write_bit(&mut self, state: bool) {
        self.write_bits(if state { 1 } else { 0 }, 1);
    }

    /// Write scaling list per H.264 spec section 7.3.2.1.1.1.
    pub fn write_scaling_list(&mut self, list: &[u8], start: usize, count: usize) {
        let scan: Vec<u8> = if count == 16 {
            ZIG_ZAG_SCAN[..count].to_vec()
        } else {
            ZIG_ZAG_DIRECT[..count].to_vec()
        };

        let mut last_scale: u8 = 8;
        for index in 0..count {
            let value = list[start + scan[index] as usize];
            let delta_scale = value as i32 - last_scale as i32;
            self.write_se(delta_scale);
            last_scale = value;
        }
    }

    /// Return the composed byte array.
    pub fn get_byte_array(&self) -> &Vec<u8> {
        &self.byte_array
    }

    // --- Private helpers ---

    fn write_bits(&mut self, value: i32, bit_count: i32) {
        let mut value_pos = 0i32;
        let mut remaining = bit_count;

        while remaining > 0 {
            let free_bits = self.get_free_buffer_bits();
            let copy_size = remaining.min(free_bits);

            let mask = (1 << copy_size) - 1;
            let src_shift = (bit_count - value_pos) - copy_size;
            let dst_shift = (self.buffer_size - self.buffer_pos) - copy_size;

            self.buffer |= ((value >> src_shift) & mask) << dst_shift;

            value_pos += copy_size;
            self.buffer_pos += copy_size;
            remaining -= copy_size;
        }
    }

    fn write_exp_golomb_coded_int(&mut self, mut value: i32) {
        let sign = if value <= 0 { 0 } else { 1 };
        if value < 0 {
            value = -value;
        }
        value = (value << 1) - sign;
        self.write_exp_golomb_coded_uint(value as u32);
    }

    fn write_exp_golomb_coded_uint(&mut self, value: u32) {
        let size = 32 - (value + 1).leading_zeros() as i32;
        self.write_bits(1, size);
        let adjusted = value - ((1u32 << (size - 1)) - 1);
        self.write_bits(adjusted as i32, size - 1);
    }

    fn get_free_buffer_bits(&mut self) -> i32 {
        if self.buffer_pos == self.buffer_size {
            self.flush();
        }
        self.buffer_size - self.buffer_pos
    }

    fn flush(&mut self) {
        if self.buffer_pos == 0 {
            return;
        }
        self.byte_array.push(self.buffer as u8);
        self.buffer = 0;
        self.buffer_pos = 0;
    }
}

impl Default for H264BitWriter {
    fn default() -> Self {
        Self::new()
    }
}

// --------------------------------------------------------------------------
// H264 Decoder
// --------------------------------------------------------------------------

/// H.264 video decoder.
///
/// Port of `Tegra::Decoders::H264`.
pub struct H264 {
    pub state: DecoderState,
    is_first_frame: bool,
    frame_scratch: Vec<u8>,
    current_context: H264DecoderContext,
}

impl H264 {
    pub fn new(id: i32) -> Self {
        let mut state = DecoderState::new(id);
        state.codec = VideoCodec::H264;
        state.initialized = state.decode_api.initialize(VideoCodec::H264);
        Self {
            state,
            is_first_frame: true,
            frame_scratch: Vec::new(),
            current_context: H264DecoderContext::default(),
        }
    }
}

impl DecoderImpl for H264 {
    fn compose_frame(&mut self) -> Vec<u8> {
        // Stubbed — requires memory manager integration to read H264DecoderContext from memory
        // at picture_info_offset, compose SPS/PPS headers via H264BitWriter, and append
        // the raw bitstream from frame_bitstream_offset.
        // Upstream: H264::ComposeFrame() in video_core/host1x/codecs/h264.cpp
        log::warn!(
            "H264::compose_frame: not yet implemented (requires memory manager integration)"
        );
        Vec::new()
    }

    fn get_progressive_offsets(&self) -> (u64, u64) {
        // Upstream: surface_luma_offsets[curr_pic_idx].Address() + luma_frame_offset.Address()
        //           surface_chroma_offsets[curr_pic_idx].Address() + chroma_frame_offset.Address()
        // Stubbed until NvdecRegisters are wired into the decoder.
        let pic_idx = self.current_context.h264_parameter_set.curr_pic_idx() as usize;
        let _ = pic_idx;
        log::warn!("H264::get_progressive_offsets: not yet implemented (requires NvdecRegisters)");
        (0, 0)
    }

    fn get_interlaced_offsets(&self) -> (u64, u64, u64, u64) {
        // Upstream: surface_luma_offsets[curr_pic_idx].Address() + luma_top/bot_offset.Address()
        //           surface_chroma_offsets[curr_pic_idx].Address() + chroma_top/bot_offset.Address()
        // Stubbed until NvdecRegisters are wired into the decoder.
        log::warn!("H264::get_interlaced_offsets: not yet implemented (requires NvdecRegisters)");
        (0, 0, 0, 0)
    }

    fn is_interlaced(&self) -> bool {
        self.current_context
            .h264_parameter_set
            .luma_top_offset
            .address()
            != 0
            || self
                .current_context
                .h264_parameter_set
                .luma_bot_offset
                .address()
                != 0
    }

    fn get_current_codec_name(&self) -> &str {
        "H264"
    }

    fn get_current_codec(&self) -> VideoCodec {
        self.state.codec
    }

    fn state(&self) -> &DecoderState {
        &self.state
    }

    fn state_mut(&mut self) -> &mut DecoderState {
        &mut self.state
    }
}
