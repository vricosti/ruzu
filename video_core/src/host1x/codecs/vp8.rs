// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/codecs/vp8.h` and `vp8.cpp`.
//!
//! VP8 video decoder implementation.

use crate::host1x::codecs::decoder::{DecoderImpl, DecoderState};
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::host1x::host1x::FrameQueue;
use crate::host1x::nvdec_common::{NvdecRegisters, VideoCodec};
use std::sync::Arc;

/// Surface indices used by the VP8 decoder.
///
/// Port of `Tegra::Decoders::Vp8SurfaceIndex`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vp8SurfaceIndex {
    Last = 0,
    Golden = 1,
    AltRef = 2,
    Current = 3,
}

/// VP8 picture info read from NVDEC registers.
///
/// Port of `Tegra::Decoders::VP8::VP8PictureInfo`.
#[repr(C)]
#[derive(Clone)]
pub struct Vp8PictureInfo {
    pub reserved0: [u32; 14],
    pub frame_width: u16,
    pub frame_height: u16,
    pub key_frame: u8,
    pub version: u8,
    pub surface_format: u8, // bitfield: tile_format(2), gob_height(3), reserved(3)
    pub error_conceal_on: u8,
    pub first_part_size: u32,
    pub hist_buffer_size: u32,
    pub vld_buffer_size: u32,
    pub frame_stride: [u32; 2],
    pub luma_top_offset: u32,
    pub luma_bot_offset: u32,
    pub luma_frame_offset: u32,
    pub chroma_top_offset: u32,
    pub chroma_bot_offset: u32,
    pub chroma_frame_offset: u32,
    pub display_params: [u8; 0x1c],
    pub current_output_memory_layout: i8,
    pub output_memory_layout: [i8; 3],
    pub segmentation_feature_data_update: u8,
    pub _pad: [u8; 3],
    pub result_value: u32,
    pub partition_offset: [u32; 8],
    pub reserved1: [u32; 3],
}

const _: () = assert!(std::mem::size_of::<Vp8PictureInfo>() == 0xc0);

impl Default for Vp8PictureInfo {
    fn default() -> Self {
        // Safety: zeroed representation is valid for this C-layout struct.
        unsafe { std::mem::zeroed() }
    }
}

/// VP8 video decoder.
///
/// Port of `Tegra::Decoders::VP8`.
pub struct Vp8 {
    pub state: DecoderState,
    frame_scratch: Vec<u8>,
    current_context: Vp8PictureInfo,
}

impl Vp8 {
    pub fn new(
        id: i32,
        memory_manager: Arc<MaxwellDeviceMemoryManager>,
        frame_queue: Arc<FrameQueue>,
    ) -> Self {
        let mut state = DecoderState::new(id, memory_manager, frame_queue);
        state.codec = VideoCodec::VP8;
        state.initialized = state.decode_api.initialize(VideoCodec::VP8);
        Self {
            state,
            frame_scratch: Vec::new(),
            current_context: Vp8PictureInfo::default(),
        }
    }
}

impl DecoderImpl for Vp8 {
    fn compose_frame(&mut self, _regs: &NvdecRegisters) -> Vec<u8> {
        // Stubbed — requires memory manager integration to read VP8PictureInfo from
        // memory at picture_info_offset and compose the VP8 frame header per RFC 6386 p.30.
        // Upstream: VP8::ComposeFrame() in video_core/host1x/codecs/vp8.cpp
        log::warn!("VP8::compose_frame: not yet implemented (requires memory manager integration)");
        Vec::new()
    }

    fn get_progressive_offsets(&self, regs: &NvdecRegisters) -> (u64, u64) {
        let current = Vp8SurfaceIndex::Current as usize;
        (
            regs.surface_luma_offset(current).address(),
            regs.surface_chroma_offset(current).address(),
        )
    }

    fn get_interlaced_offsets(&self, regs: &NvdecRegisters) -> (u64, u64, u64, u64) {
        let current = Vp8SurfaceIndex::Current as usize;
        let luma = regs.surface_luma_offset(current).address();
        let chroma = regs.surface_chroma_offset(current).address();
        (luma, luma, chroma, chroma)
    }

    fn is_interlaced(&self) -> bool {
        false
    }

    fn get_current_codec_name(&self) -> &str {
        "VP8"
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
