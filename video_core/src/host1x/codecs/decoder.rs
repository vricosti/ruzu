// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/codecs/decoder.h` and `decoder.cpp`.
//!
//! Base decoder trait and common decode logic. In C++ this is an abstract class
//! `Tegra::Decoder`; in Rust we use a trait plus a shared helper struct.

use log::error;

use crate::host1x::ffmpeg::ffmpeg::DecodeApi;
use std::sync::Arc;

use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::host1x::host1x::FrameQueue;
use crate::host1x::nvdec_common::{NvdecRegisters, VideoCodec};

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
    pub id: i32,
    pub memory_manager: Arc<MaxwellDeviceMemoryManager>,
    pub frame_queue: Arc<FrameQueue>,
}

impl DecoderState {
    pub fn new(
        id: i32,
        memory_manager: Arc<MaxwellDeviceMemoryManager>,
        frame_queue: Arc<FrameQueue>,
    ) -> Self {
        Self {
            codec: VideoCodec::None,
            decode_api: DecodeApi::new(),
            initialized: false,
            vp9_hidden_frame: false,
            id,
            memory_manager,
            frame_queue,
        }
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

    // Send assembled bitstream to decoder.
    if !decoder.state_mut().decode_api.send_packet(&packet_data) {
        return;
    }

    // Only receive/store visible frames.
    if decoder.state().vp9_hidden_frame {
        return;
    }

    // Receive output frame from decoder.
    let frame = decoder.state_mut().decode_api.receive_frame();

    if decoder.is_interlaced() {
        let (luma_top, luma_bottom, _chroma_top, _chroma_bottom) =
            decoder.get_interlaced_offsets(regs);

        if frame.is_none() {
            error!(
                "Nvdec {} failed to decode interlaced frame for top 0x{:X} bottom 0x{:X}",
                id, luma_top, luma_bottom
            );
        }

        let frame_copy = frame.clone();
        if decoder.state().decode_api.using_decode_order() {
            if let Some(f) = frame {
                frame_queue.push_decode_order(id, luma_top, f);
            }
            if let Some(f) = frame_copy {
                frame_queue.push_decode_order(id, luma_bottom, f);
            }
        } else {
            if let Some(f) = frame {
                frame_queue.push_present_order(id, luma_top, f);
            }
            if let Some(f) = frame_copy {
                frame_queue.push_present_order(id, luma_bottom, f);
            }
        }
    } else {
        let (luma_offset, _chroma_offset) = decoder.get_progressive_offsets(regs);

        if frame.is_none() {
            error!(
                "Nvdec {} failed to decode progressive frame for luma 0x{:X}",
                id, luma_offset
            );
        }

        if decoder.state().decode_api.using_decode_order() {
            if let Some(f) = frame {
                frame_queue.push_decode_order(id, luma_offset, f);
            }
        } else {
            if let Some(f) = frame {
                frame_queue.push_present_order(id, luma_offset, f);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DecoderState;
    use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
    use crate::host1x::host1x::FrameQueue;
    use std::sync::Arc;

    #[test]
    fn decoder_state_owns_memory_manager_and_frame_queue_handles() {
        let memory_manager = Arc::new(MaxwellDeviceMemoryManager::default());
        let frame_queue = Arc::new(FrameQueue::new());

        let state = DecoderState::new(3, Arc::clone(&memory_manager), Arc::clone(&frame_queue));

        assert_eq!(state.id, 3);
        assert!(Arc::ptr_eq(&state.memory_manager, &memory_manager));
        assert!(Arc::ptr_eq(&state.frame_queue, &frame_queue));
    }
}
