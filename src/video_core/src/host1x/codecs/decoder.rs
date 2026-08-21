// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/codecs/decoder.h` and `decoder.cpp`.
//!
//! Base decoder trait and common decode logic. In C++ this is an abstract class
//! `Tegra::Decoder`; in Rust we use a trait plus a shared helper struct.

use crate::host1x::ffmpeg::ffmpeg::{DecodeApi, FrameDimensions, FrameOffsets};
use std::sync::Arc;

use crate::host1x::host1x::FrameQueue;
use crate::host1x::nvdec_common::{NvdecRegisters, VideoCodec};
use crate::memory_manager::MemoryManager;

/// Trait matching the virtual methods of the upstream `Tegra::Decoder` class.
pub trait DecoderImpl: Send {
    /// Compose the frame bitstream for FFmpeg decoding.
    fn compose_frame(&mut self, regs: &NvdecRegisters) -> Vec<u8>;

    /// Get progressive luma/chroma offsets.
    fn get_progressive_offsets(&self, regs: &NvdecRegisters) -> (u64, u64);

    /// Get interlaced luma_top, luma_bottom, chroma_top, chroma_bottom offsets.
    fn get_interlaced_offsets(&self, regs: &NvdecRegisters) -> (u64, u64, u64, u64);

    /// Whether the current frame is interlaced.
    fn is_interlaced(&self) -> bool;

    /// Human-readable codec name.
    fn get_current_codec_name(&self) -> &str;

    /// The current video codec.
    fn get_current_codec(&self) -> VideoCodec;

    /// Access the shared decoder state.
    fn state(&self) -> &DecoderState;

    /// Access the shared decoder state mutably.
    fn state_mut(&mut self) -> &mut DecoderState;
}

/// Shared state for all decoder implementations.
///
/// Port of the non-virtual members of `Tegra::Decoder`.
pub struct DecoderState {
    pub codec: VideoCodec,
    pub decode_api: DecodeApi,
    pub initialized: bool,
    pub vp9_hidden_frame: bool,
    pub frame_dimensions: Option<FrameDimensions>,
    pub id: i32,
    pub memory_manager: Arc<parking_lot::Mutex<MemoryManager>>,
    pub frame_queue: Arc<FrameQueue>,
}

impl DecoderState {
    pub fn new(
        id: i32,
        memory_manager: Arc<parking_lot::Mutex<MemoryManager>>,
        frame_queue: Arc<FrameQueue>,
    ) -> Self {
        Self {
            codec: VideoCodec::None,
            decode_api: DecodeApi::new(),
            initialized: false,
            vp9_hidden_frame: false,
            frame_dimensions: None,
            id,
            memory_manager,
            frame_queue,
        }
    }

    pub fn set_frame_dimensions(&mut self, width: i32, height: i32) {
        if width <= 0 || height <= 0 {
            self.frame_dimensions = None;
            return;
        }
        self.frame_dimensions = Some(FrameDimensions { width, height });
    }

    pub fn get_frame_dimensions(&self) -> Option<FrameDimensions> {
        self.frame_dimensions
    }
}

/// Execute the decode pipeline: compose frame, send to FFmpeg, receive and
/// enqueue the result.
///
/// Port of `Tegra::Decoder::Decode`.
pub fn decode(decoder: &mut dyn DecoderImpl, regs: &NvdecRegisters) {
    let state = decoder.state();
    if !state.initialized {
        return;
    }
    let id = state.id;
    let frame_queue = Arc::clone(&state.frame_queue);

    let packet_data = decoder.compose_frame(regs);

    let interlaced = decoder.is_interlaced();
    let mut offsets = FrameOffsets {
        hidden: decoder.state().vp9_hidden_frame,
        interlaced,
        ..FrameOffsets::default()
    };
    if interlaced {
        let (luma, luma_bottom, _, _) = decoder.get_interlaced_offsets(regs);
        offsets.luma = luma;
        offsets.luma_bottom = luma_bottom;
    } else {
        let (luma, _) = decoder.get_progressive_offsets(regs);
        offsets.luma = luma;
    }
    let frame_dimensions = decoder.state().get_frame_dimensions();

    // Send assembled bitstream to decoder.
    if !decoder
        .state_mut()
        .decode_api
        .send_packet(&packet_data, offsets, frame_dimensions)
    {
        return;
    }

    let using_decode_order = decoder.state().decode_api.using_decode_order();
    while let Some(result) = decoder.state_mut().decode_api.receive_frame() {
        let frame = result.frame;
        let frame_offsets = result.offsets;
        let push = |luma, frame| {
            if using_decode_order {
                frame_queue.push_decode_order(id, luma, frame);
            } else {
                frame_queue.push_present_order(id, luma, frame);
            }
        };

        if frame_offsets.interlaced {
            let frame_copy = Arc::clone(&frame);
            push(frame_offsets.luma, frame);
            push(frame_offsets.luma_bottom, frame_copy);
        } else {
            push(frame_offsets.luma, frame);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DecoderState;
    use crate::host1x::ffmpeg::ffmpeg::FrameDimensions;
    use crate::host1x::host1x::FrameQueue;
    use crate::memory_manager::MemoryManager;
    use std::sync::Arc;

    #[test]
    fn decoder_state_owns_memory_manager_and_frame_queue_handles() {
        let memory_manager = Arc::new(parking_lot::Mutex::new(MemoryManager::new(0)));
        let frame_queue = Arc::new(FrameQueue::new());

        let state = DecoderState::new(3, Arc::clone(&memory_manager), Arc::clone(&frame_queue));

        assert_eq!(state.id, 3);
        assert!(Arc::ptr_eq(&state.memory_manager, &memory_manager));
        assert!(Arc::ptr_eq(&state.frame_queue, &frame_queue));
    }

    #[test]
    fn frame_dimensions_follow_upstream_validation() {
        let memory_manager = Arc::new(parking_lot::Mutex::new(MemoryManager::new(0)));
        let frame_queue = Arc::new(FrameQueue::new());
        let mut state = DecoderState::new(3, memory_manager, frame_queue);

        state.set_frame_dimensions(1920, 1080);
        assert_eq!(
            state.get_frame_dimensions(),
            Some(FrameDimensions {
                width: 1920,
                height: 1080,
            })
        );

        state.set_frame_dimensions(0, 1080);
        assert_eq!(state.get_frame_dimensions(), None);
        state.set_frame_dimensions(1920, -1);
        assert_eq!(state.get_frame_dimensions(), None);
    }
}
