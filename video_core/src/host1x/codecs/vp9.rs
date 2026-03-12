// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/codecs/vp9.h` and `vp9.cpp`.
//!
//! VP9 video decoder implementation, including the VpxRangeEncoder and
//! VpxBitStreamWriter for composing VP9 compressed/uncompressed headers.

use crate::host1x::codecs::decoder::{DecoderImpl, DecoderState};
use crate::host1x::codecs::vp9_types::{
    PictureInfo, Segmentation, Vp9EntropyProbs, Vp9FrameContainer, Vp9PictureInfo,
};
use crate::host1x::nvdec_common::VideoCodec;

// --------------------------------------------------------------------------
// Constants from vp9.cpp
// --------------------------------------------------------------------------

/// Probability used for diff updates in the compressed header.
#[allow(dead_code)]
const DIFF_UPDATE_PROBABILITY: u32 = 252;

/// Frame sync code for VP9 uncompressed header.
#[allow(dead_code)]
const FRAME_SYNC_CODE: u32 = 0x498342;

// --------------------------------------------------------------------------
// VpxRangeEncoder
// --------------------------------------------------------------------------

/// Range encoder for VP9 compressed header bitstreams.
///
/// Port of `Tegra::Decoders::VpxRangeEncoder`.
pub struct VpxRangeEncoder {
    buffer: Vec<u8>,
    low_value: u32,
    range: u32,
    count: i32,
    #[allow(dead_code)]
    half_probability: i32,
}

impl VpxRangeEncoder {
    pub fn new() -> Self {
        Self {
            buffer: Vec::new(),
            low_value: 0,
            range: 0xff,
            count: -24,
            half_probability: 128,
        }
    }

    /// Writes the rightmost value_size bits from value into the stream.
    pub fn write(&mut self, value: i32, value_size: i32) {
        for i in (0..value_size).rev() {
            self.write_bit_half((value >> i) & 1 != 0);
        }
    }

    /// Writes a single bit with half probability.
    pub fn write_bit_half(&mut self, bit: bool) {
        self.write_bool(bit, 128);
    }

    /// Writes a bit encoded with the given probability.
    pub fn write_bool(&mut self, bit: bool, probability: i32) {
        let split = 1 + (((self.range - 1) * probability as u32) >> 8);

        if bit {
            self.low_value += split;
            self.range -= split;
        } else {
            self.range = split;
        }

        let mut shift = self.range.leading_zeros() as i32 - 24;
        if shift < 0 {
            shift = 0;
        }
        self.range <<= shift;
        self.count += shift;
        self.low_value <<= shift;

        if self.count >= 0 {
            let offset = self.count >> 3;
            let _ = offset;
            self.buffer.push(((self.low_value >> 24) & 0xFF) as u8);
            self.low_value &= (1 << 24) - 1;
            self.count -= 8;
        }
    }

    /// Signal the end of the bitstream.
    pub fn end(&mut self) {
        for _ in 0..32 {
            self.write_bit_half(false);
        }
    }

    pub fn get_buffer(&self) -> &Vec<u8> {
        &self.buffer
    }

    pub fn get_buffer_mut(&mut self) -> &mut Vec<u8> {
        &mut self.buffer
    }
}

impl Default for VpxRangeEncoder {
    fn default() -> Self {
        Self::new()
    }
}

// --------------------------------------------------------------------------
// VpxBitStreamWriter
// --------------------------------------------------------------------------

/// Bitstream writer for VP9 uncompressed headers.
///
/// Port of `Tegra::Decoders::VpxBitStreamWriter`.
pub struct VpxBitStreamWriter {
    buffer_size: i32,
    buffer: i32,
    buffer_pos: i32,
    byte_array: Vec<u8>,
}

impl VpxBitStreamWriter {
    pub fn new() -> Self {
        Self {
            buffer_size: 8,
            buffer: 0,
            buffer_pos: 0,
            byte_array: Vec::new(),
        }
    }

    /// Write an unsigned integer value.
    pub fn write_u(&mut self, value: u32, value_size: u32) {
        self.write_bits(value, value_size);
    }

    /// Write a signed integer value.
    pub fn write_s(&mut self, value: i32, value_size: u32) {
        let unsigned_value = if value < 0 {
            ((-value) as u32) | (1 << value_size)
        } else {
            value as u32
        };

        // Write magnitude, then sign bit.
        self.write_bits(unsigned_value & ((1 << value_size) - 1), value_size);
        if value_size > 0 {
            self.write_bit(value < 0);
        }
    }

    /// Write a delta coded value per VP9 spec section 6.2.10.
    pub fn write_delta_q(&mut self, value: u32) {
        if value == 0 {
            self.write_bit(false);
        } else {
            self.write_bit(true);
            self.write_bits(value, 4);
            self.write_bit(false); // sign = positive
        }
    }

    /// Write a single bit.
    pub fn write_bit(&mut self, state: bool) {
        self.write_bits(if state { 1 } else { 0 }, 1);
    }

    /// Pushes current buffer into byte_array, resets buffer.
    pub fn flush(&mut self) {
        if self.buffer_pos == 0 {
            return;
        }
        self.byte_array.push(self.buffer as u8);
        self.buffer = 0;
        self.buffer_pos = 0;
    }

    /// Returns the composed byte array.
    pub fn get_byte_array(&self) -> &Vec<u8> {
        &self.byte_array
    }

    /// Returns the composed byte array mutably.
    pub fn get_byte_array_mut(&mut self) -> &mut Vec<u8> {
        &mut self.byte_array
    }

    // --- Private ---

    fn write_bits(&mut self, value: u32, bit_count: u32) {
        let mut value_pos = 0u32;
        let mut remaining = bit_count as i32;

        while remaining > 0 {
            let free_bits = self.get_free_buffer_bits();
            let copy_size = remaining.min(free_bits);

            let mask = (1i32 << copy_size) - 1;
            let src_shift = (bit_count as i32 - value_pos as i32) - copy_size;
            let dst_shift = (self.buffer_size - self.buffer_pos) - copy_size;

            self.buffer |= (((value as i32) >> src_shift) & mask) << dst_shift;

            value_pos += copy_size as u32;
            self.buffer_pos += copy_size;
            remaining -= copy_size;
        }
    }

    fn get_free_buffer_bits(&mut self) -> i32 {
        if self.buffer_pos == self.buffer_size {
            self.flush();
        }
        self.buffer_size - self.buffer_pos
    }
}

impl Default for VpxBitStreamWriter {
    fn default() -> Self {
        Self::new()
    }
}

// --------------------------------------------------------------------------
// VP9 Decoder
// --------------------------------------------------------------------------

/// VP9 video decoder.
///
/// Port of `Tegra::Decoders::VP9`.
pub struct Vp9 {
    pub state: DecoderState,
    frame_scratch: Vec<u8>,

    loop_filter_ref_deltas: [i8; 4],
    loop_filter_mode_deltas: [i8; 2],

    next_frame: Vp9FrameContainer,
    frame_ctxs: [Vp9EntropyProbs; 4],
    #[allow(dead_code)]
    swap_ref_indices: bool,

    last_segmentation: Segmentation,
    #[allow(dead_code)]
    current_picture_info: PictureInfo,
    current_frame_info: Vp9PictureInfo,
    #[allow(dead_code)]
    prev_frame_probs: Vp9EntropyProbs,
}

impl Vp9 {
    pub fn new(id: i32) -> Self {
        let mut state = DecoderState::new(id);
        state.codec = VideoCodec::VP9;
        state.initialized = state.decode_api.initialize(VideoCodec::VP9);
        Self {
            state,
            frame_scratch: Vec::new(),
            loop_filter_ref_deltas: [0; 4],
            loop_filter_mode_deltas: [0; 2],
            next_frame: Vp9FrameContainer::default(),
            frame_ctxs: std::array::from_fn(|_| Vp9EntropyProbs::default()),
            swap_ref_indices: false,
            last_segmentation: Segmentation::default(),
            current_picture_info: PictureInfo::default(),
            current_frame_info: Vp9PictureInfo::default(),
            prev_frame_probs: Vp9EntropyProbs::default(),
        }
    }

    /// Returns true if the most recent frame was hidden.
    #[allow(dead_code)]
    fn was_frame_hidden(&self) -> bool {
        !self.current_frame_info.show_frame
    }
}

impl DecoderImpl for Vp9 {
    fn compose_frame(&mut self) -> Vec<u8> {
        // TODO: Read PictureInfo/EntropyProbs from memory, compose compressed
        // and uncompressed headers, append bitstream.
        // Stubbed — requires memory manager integration.
        todo!("VP9::compose_frame — requires memory manager integration")
    }

    fn get_progressive_offsets(&self) -> (u64, u64) {
        // Upstream uses surface_luma_offsets[Current] and surface_chroma_offsets[Current].
        todo!("VP9::get_progressive_offsets — requires NvdecRegisters access")
    }

    fn get_interlaced_offsets(&self) -> (u64, u64, u64, u64) {
        todo!("VP9::get_interlaced_offsets — requires NvdecRegisters access")
    }

    fn is_interlaced(&self) -> bool {
        false
    }

    fn get_current_codec_name(&self) -> &str {
        "VP9"
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
