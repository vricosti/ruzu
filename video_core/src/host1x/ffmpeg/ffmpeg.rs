// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/ffmpeg/ffmpeg.h` and `ffmpeg.cpp`.
//!
//! Wraps FFmpeg types (AVPacket, AVFrame, AVCodec, AVCodecContext) for video
//! decoding. All bodies that depend on FFmpeg are stubbed with `log::warn!` and
//! safe default returns since actual FFmpeg integration requires external C bindings.

use std::sync::Arc;

use crate::host1x::nvdec_common::VideoCodec;

/// Wraps an AVPacket — a container for compressed bitstream data.
///
/// Port of `FFmpeg::Packet`.
pub struct Packet {
    _data: Vec<u8>,
}

impl Packet {
    pub fn new(data: &[u8]) -> Self {
        Self {
            _data: data.to_vec(),
        }
    }
}

/// Wraps an AVFrame — a container for decoded audio/video data.
///
/// Port of `FFmpeg::Frame`.
pub struct Frame {
    width: i32,
    height: i32,
    pixel_format: i32,
    strides: [i32; 4],
    interlaced: bool,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            width: 0,
            height: 0,
            pixel_format: 0,
            strides: [0; 4],
            interlaced: false,
        }
    }

    pub fn get_width(&self) -> i32 {
        self.width
    }

    pub fn get_height(&self) -> i32 {
        self.height
    }

    pub fn get_pixel_format(&self) -> i32 {
        self.pixel_format
    }

    pub fn get_stride(&self, plane: usize) -> i32 {
        self.strides[plane]
    }

    pub fn is_interlaced(&self) -> bool {
        self.interlaced
    }

    pub fn is_hardware_decoded(&self) -> bool {
        false
    }
}

impl Default for Frame {
    fn default() -> Self {
        Self::new()
    }
}

/// Wraps an AVCodec — codec information.
///
/// Port of `FFmpeg::Decoder`.
pub struct Decoder {
    _codec: VideoCodec,
}

impl Decoder {
    pub fn new(codec: VideoCodec) -> Self {
        Self { _codec: codec }
    }

    pub fn supports_decoding_on_device(&self) -> bool {
        // Stubbed — requires FFmpeg C bindings to query AVCodec hardware device support.
        // Upstream: FFmpeg::Decoder::SupportDecodingOnDevice() in ffmpeg.cpp
        log::warn!("FFmpeg::Decoder::supports_decoding_on_device: FFmpeg bindings not available");
        false
    }
}

/// Wraps AVBufferRef for hardware-accelerated decoding.
///
/// Port of `FFmpeg::HardwareContext`.
pub struct HardwareContext;

impl HardwareContext {
    pub fn new() -> Self {
        Self
    }

    pub fn get_supported_device_types() -> Vec<u32> {
        // Stubbed — requires FFmpeg C bindings to enumerate AV_HWDEVICE_TYPE_* values.
        // Upstream: FFmpeg::HardwareContext::GetSupportedDeviceTypes() in ffmpeg.cpp
        log::warn!("FFmpeg::HardwareContext::get_supported_device_types: FFmpeg bindings not available");
        Vec::new()
    }

    pub fn initialize_for_decoder(
        &mut self,
        _decoder_context: &mut DecoderContext,
        _decoder: &Decoder,
    ) -> bool {
        // Stubbed — requires FFmpeg C bindings to create and attach an AVBufferRef
        // hardware device context to the AVCodecContext.
        // Upstream: FFmpeg::HardwareContext::InitializeForDecoder() in ffmpeg.cpp
        log::warn!("FFmpeg::HardwareContext::initialize_for_decoder: FFmpeg bindings not available");
        false
    }
}

impl Default for HardwareContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Wraps an AVCodecContext.
///
/// Port of `FFmpeg::DecoderContext`.
pub struct DecoderContext {
    _decode_order: bool,
}

impl DecoderContext {
    pub fn new(_decoder: &Decoder) -> Self {
        Self {
            _decode_order: false,
        }
    }

    pub fn initialize_hardware_decoder(&mut self, _context: &HardwareContext, _hw_pix_fmt: i32) {
        // Stubbed — requires FFmpeg C bindings to set AVCodecContext hw_device_ctx
        // and get_format callback.
        // Upstream: FFmpeg::DecoderContext::InitializeHardwareDecoder() in ffmpeg.cpp
        log::warn!("FFmpeg::DecoderContext::initialize_hardware_decoder: FFmpeg bindings not available");
    }

    pub fn open_context(&mut self, _decoder: &Decoder) -> bool {
        // Stubbed — requires FFmpeg C bindings to call avcodec_open2().
        // Upstream: FFmpeg::DecoderContext::OpenContext() in ffmpeg.cpp
        log::warn!("FFmpeg::DecoderContext::open_context: FFmpeg bindings not available");
        false
    }

    pub fn send_packet(&mut self, _packet: &Packet) -> bool {
        // Stubbed — requires FFmpeg C bindings to call avcodec_send_packet().
        // Upstream: FFmpeg::DecoderContext::SendPacket() in ffmpeg.cpp
        log::warn!("FFmpeg::DecoderContext::send_packet: FFmpeg bindings not available");
        false
    }

    pub fn receive_frame(&mut self) -> Option<Arc<Frame>> {
        // Stubbed — requires FFmpeg C bindings to call avcodec_receive_frame().
        // Upstream: FFmpeg::DecoderContext::ReceiveFrame() in ffmpeg.cpp
        log::warn!("FFmpeg::DecoderContext::receive_frame: FFmpeg bindings not available");
        None
    }

    pub fn using_decode_order(&self) -> bool {
        self._decode_order
    }
}

/// High-level decode API that manages codec, context, and optional hardware
/// acceleration.
///
/// Port of `FFmpeg::DecodeApi`.
pub struct DecodeApi {
    decoder: Option<Decoder>,
    decoder_context: Option<DecoderContext>,
    hardware_context: Option<HardwareContext>,
}

impl DecodeApi {
    pub fn new() -> Self {
        Self {
            decoder: None,
            decoder_context: None,
            hardware_context: None,
        }
    }

    pub fn initialize(&mut self, codec: VideoCodec) -> bool {
        self.reset();
        let decoder = Decoder::new(codec);
        let decoder_context = DecoderContext::new(&decoder);
        self.decoder = Some(decoder);
        self.decoder_context = Some(decoder_context);
        // TODO: Initialize hardware context and open decoder when FFmpeg bindings are available.
        // For now, return false to indicate initialization is stubbed.
        false
    }

    pub fn reset(&mut self) {
        self.hardware_context = None;
        self.decoder_context = None;
        self.decoder = None;
    }

    pub fn using_decode_order(&self) -> bool {
        self.decoder_context
            .as_ref()
            .map_or(false, |ctx| ctx.using_decode_order())
    }

    pub fn send_packet(&mut self, _packet_data: &[u8]) -> bool {
        // Stubbed — requires FFmpeg C bindings to wrap data in AVPacket and call
        // DecoderContext::send_packet().
        // Upstream: FFmpeg::DecodeApi::SendPacket() in ffmpeg.cpp
        log::warn!("FFmpeg::DecodeApi::send_packet: FFmpeg bindings not available");
        false
    }

    pub fn receive_frame(&mut self) -> Option<Arc<Frame>> {
        // Stubbed — requires FFmpeg C bindings to call DecoderContext::receive_frame().
        // Upstream: FFmpeg::DecodeApi::ReceiveFrame() in ffmpeg.cpp
        log::warn!("FFmpeg::DecodeApi::receive_frame: FFmpeg bindings not available");
        None
    }
}

impl Default for DecodeApi {
    fn default() -> Self {
        Self::new()
    }
}
