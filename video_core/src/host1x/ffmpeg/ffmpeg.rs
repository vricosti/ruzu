// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/ffmpeg/ffmpeg.h` and `ffmpeg.cpp`.
//!
//! Wraps FFmpeg types (AVPacket, AVFrame, AVCodec, AVCodecContext) for video
//! decoding.

use std::ffi::CStr;
use std::sync::Arc;

use crate::host1x::nvdec_common::VideoCodec;

mod ffi {
    use libc::{c_int, c_uchar, c_void, uintptr_t};

    pub type RuzuFfmpegDecoder = c_void;
    pub type AVFrame = c_void;

    extern "C" {
        pub fn ruzu_ffmpeg_decoder_create(codec: u64) -> *mut RuzuFfmpegDecoder;
        pub fn ruzu_ffmpeg_decoder_destroy(decoder: *mut RuzuFfmpegDecoder);
        pub fn ruzu_ffmpeg_decoder_send_packet(
            decoder: *mut RuzuFfmpegDecoder,
            data: *const c_uchar,
            size: uintptr_t,
        ) -> c_int;
        pub fn ruzu_ffmpeg_decoder_receive_frame(decoder: *mut RuzuFfmpegDecoder) -> *mut AVFrame;
        pub fn ruzu_ffmpeg_decoder_last_error(decoder: *const RuzuFfmpegDecoder) -> c_int;
        pub fn ruzu_ffmpeg_error_string(errnum: c_int, out: *mut i8, out_size: uintptr_t);
        pub fn ruzu_ffmpeg_frame_destroy(frame: *mut AVFrame);
        pub fn ruzu_ffmpeg_frame_width(frame: *const AVFrame) -> c_int;
        pub fn ruzu_ffmpeg_frame_height(frame: *const AVFrame) -> c_int;
        pub fn ruzu_ffmpeg_frame_format(frame: *const AVFrame) -> c_int;
        pub fn ruzu_ffmpeg_frame_stride(frame: *const AVFrame, plane: c_int) -> c_int;
        pub fn ruzu_ffmpeg_frame_plane(frame: *const AVFrame, plane: c_int) -> *const c_uchar;
        pub fn ruzu_ffmpeg_frame_interlaced(frame: *const AVFrame) -> c_int;
    }
}

fn av_error(ret: i32) -> String {
    let mut buffer = [0i8; 128];
    unsafe {
        ffi::ruzu_ffmpeg_error_string(ret, buffer.as_mut_ptr(), buffer.len());
        CStr::from_ptr(buffer.as_ptr())
            .to_string_lossy()
            .into_owned()
    }
}

/// Wraps an AVPacket — a container for compressed bitstream data.
///
/// Port of `FFmpeg::Packet`.
pub struct Packet {
    data: Vec<u8>,
}

impl Packet {
    pub fn new(data: &[u8]) -> Self {
        Self {
            data: data.to_vec(),
        }
    }
}

/// Wraps an AVFrame — a container for decoded audio/video data.
///
/// Port of `FFmpeg::Frame`.
pub struct Frame {
    raw: *mut ffi::AVFrame,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            raw: std::ptr::null_mut(),
        }
    }

    fn from_raw(raw: *mut ffi::AVFrame) -> Self {
        Self { raw }
    }

    pub fn get_width(&self) -> i32 {
        unsafe { ffi::ruzu_ffmpeg_frame_width(self.raw.cast_const()) }
    }

    pub fn get_height(&self) -> i32 {
        unsafe { ffi::ruzu_ffmpeg_frame_height(self.raw.cast_const()) }
    }

    pub fn get_pixel_format(&self) -> i32 {
        unsafe { ffi::ruzu_ffmpeg_frame_format(self.raw.cast_const()) }
    }

    pub fn get_stride(&self, plane: usize) -> i32 {
        unsafe { ffi::ruzu_ffmpeg_frame_stride(self.raw.cast_const(), plane as i32) }
    }

    pub fn get_plane_ptr(&self, plane: usize) -> *const u8 {
        unsafe { ffi::ruzu_ffmpeg_frame_plane(self.raw.cast_const(), plane as i32) }
    }

    pub fn is_interlaced(&self) -> bool {
        unsafe { ffi::ruzu_ffmpeg_frame_interlaced(self.raw.cast_const()) != 0 }
    }

    pub fn is_hardware_decoded(&self) -> bool {
        false
    }
}

unsafe impl Send for Frame {}
unsafe impl Sync for Frame {}

impl Drop for Frame {
    fn drop(&mut self) {
        if !self.raw.is_null() {
            unsafe { ffi::ruzu_ffmpeg_frame_destroy(self.raw) };
            self.raw = std::ptr::null_mut();
        }
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
    codec: VideoCodec,
}

impl Decoder {
    pub fn new(codec: VideoCodec) -> Self {
        Self { codec }
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
        log::warn!(
            "FFmpeg::HardwareContext::get_supported_device_types: FFmpeg bindings not available"
        );
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
        log::warn!(
            "FFmpeg::HardwareContext::initialize_for_decoder: FFmpeg bindings not available"
        );
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
    raw: *mut ffi::RuzuFfmpegDecoder,
    codec: VideoCodec,
    decode_order: bool,
}

impl DecoderContext {
    pub fn new(decoder: &Decoder) -> Self {
        Self {
            raw: std::ptr::null_mut(),
            codec: decoder.codec,
            decode_order: false,
        }
    }

    pub fn initialize_hardware_decoder(&mut self, _context: &HardwareContext, _hw_pix_fmt: i32) {
        // Stubbed — requires FFmpeg C bindings to set AVCodecContext hw_device_ctx
        // and get_format callback.
        // Upstream: FFmpeg::DecoderContext::InitializeHardwareDecoder() in ffmpeg.cpp
        log::warn!(
            "FFmpeg::DecoderContext::initialize_hardware_decoder: FFmpeg bindings not available"
        );
    }

    pub fn open_context(&mut self, _decoder: &Decoder) -> bool {
        self.raw = unsafe { ffi::ruzu_ffmpeg_decoder_create(_decoder.codec as u64) };
        if self.raw.is_null() {
            log::error!(
                "FFmpeg::DecoderContext::open_context: failed to open codec {:?}",
                _decoder.codec
            );
            return false;
        }
        log::info!("Using FFmpeg software decoding");
        // Upstream forces H264 software decoding through the decode-order queue.
        // ruzu still uses avcodec_send_packet in the C shim, but VIC frame lookup
        // must match upstream's offset-keyed queueing semantics.
        self.decode_order = self.codec == VideoCodec::H264;
        true
    }

    pub fn send_packet(&mut self, _packet: &Packet) -> bool {
        if self.raw.is_null() {
            return false;
        }
        let ret = unsafe {
            ffi::ruzu_ffmpeg_decoder_send_packet(
                self.raw,
                _packet.data.as_ptr(),
                _packet.data.len(),
            )
        };
        if ret < 0 {
            log::error!(
                "FFmpeg::DecoderContext::send_packet: avcodec_send_packet error: {}",
                av_error(ret)
            );
            return false;
        }
        true
    }

    pub fn receive_frame(&mut self) -> Option<Arc<Frame>> {
        if self.raw.is_null() {
            return None;
        }
        let frame = unsafe { ffi::ruzu_ffmpeg_decoder_receive_frame(self.raw) };
        if frame.is_null() {
            let ret = unsafe { ffi::ruzu_ffmpeg_decoder_last_error(self.raw.cast_const()) };
            if ret < 0 {
                log::error!(
                    "FFmpeg::DecoderContext::receive_frame: avcodec_receive_frame error: {}",
                    av_error(ret)
                );
            }
            return None;
        }
        Some(Arc::new(Frame::from_raw(frame)))
    }

    pub fn using_decode_order(&self) -> bool {
        self.decode_order
    }
}

unsafe impl Send for DecoderContext {}

impl Drop for DecoderContext {
    fn drop(&mut self) {
        if !self.raw.is_null() {
            unsafe { ffi::ruzu_ffmpeg_decoder_destroy(self.raw) };
            self.raw = std::ptr::null_mut();
        }
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
        let mut decoder_context = DecoderContext::new(&decoder);
        let initialized = decoder_context.open_context(&decoder);
        let _ = common::trace::emit(
            common::trace::cat::HOST1X_VIDEO,
            &[4, 1, codec as u64, initialized as u64, 0],
        );
        if !initialized {
            return false;
        }
        self.decoder = Some(decoder);
        self.decoder_context = Some(decoder_context);
        true
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
        let _ = common::trace::emit(
            common::trace::cat::HOST1X_VIDEO,
            &[4, 2, 0, 0, _packet_data.len() as u64],
        );
        let Some(decoder_context) = self.decoder_context.as_mut() else {
            return false;
        };
        let packet = Packet::new(_packet_data);
        decoder_context.send_packet(&packet)
    }

    pub fn receive_frame(&mut self) -> Option<Arc<Frame>> {
        let _ = common::trace::emit(common::trace::cat::HOST1X_VIDEO, &[4, 3, 0, 0, 0]);
        self.decoder_context.as_mut()?.receive_frame()
    }
}

unsafe impl Send for DecodeApi {}

impl Default for DecodeApi {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_api_initializes_h264_software_decoder() {
        let mut api = DecodeApi::new();
        assert!(api.initialize(VideoCodec::H264));
        assert!(api.using_decode_order());
    }
}
