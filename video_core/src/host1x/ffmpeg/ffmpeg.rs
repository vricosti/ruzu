// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/ffmpeg/ffmpeg.h` and `ffmpeg.cpp`.
//!
//! Wraps FFmpeg types (AVPacket, AVFrame, AVCodec, AVCodecContext) for video
//! decoding.

use std::ffi::CStr;
use std::sync::Arc;

use crate::host1x::nvdec_common::VideoCodec;

const AV_NUM_DATA_POINTERS: usize = 8;

mod ffi {
    use libc::{c_int, c_uchar, c_void, uintptr_t};

    pub type RuzuFfmpegDecoder = c_void;
    pub type RuzuFfmpegHardwareContext = c_void;
    pub type AVFrame = c_void;

    extern "C" {
        pub fn ruzu_ffmpeg_decoder_create(codec: u64) -> *mut RuzuFfmpegDecoder;
        pub fn ruzu_ffmpeg_decoder_open(decoder: *mut RuzuFfmpegDecoder) -> c_int;
        pub fn ruzu_ffmpeg_decoder_destroy(decoder: *mut RuzuFfmpegDecoder);
        pub fn ruzu_ffmpeg_hardware_context_create() -> *mut RuzuFfmpegHardwareContext;
        pub fn ruzu_ffmpeg_hardware_context_destroy(hardware: *mut RuzuFfmpegHardwareContext);
        pub fn ruzu_ffmpeg_decoder_supports_decoding_on_device(
            codec: u64,
            device_type: c_int,
            out_pix_fmt: *mut c_int,
        ) -> c_int;
        pub fn ruzu_ffmpeg_supported_device_types(
            out: *mut c_int,
            out_capacity: uintptr_t,
        ) -> uintptr_t;
        pub fn ruzu_ffmpeg_decoder_send_packet(
            decoder: *mut RuzuFfmpegDecoder,
            data: *const c_uchar,
            size: uintptr_t,
        ) -> c_int;
        pub fn ruzu_ffmpeg_decoder_receive_frame(decoder: *mut RuzuFfmpegDecoder) -> *mut AVFrame;
        pub fn ruzu_ffmpeg_decoder_receive_frame_with_hw_transfer(
            decoder: *mut RuzuFfmpegDecoder,
        ) -> *mut AVFrame;
        pub fn ruzu_ffmpeg_hardware_initialize_for_decoder(
            hardware: *mut RuzuFfmpegHardwareContext,
            decoder: *mut RuzuFfmpegDecoder,
            codec: u64,
        ) -> c_int;
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

    pub fn get_strides(&self) -> [i32; AV_NUM_DATA_POINTERS] {
        let mut strides = [0; AV_NUM_DATA_POINTERS];
        for (plane, stride) in strides.iter_mut().enumerate() {
            *stride = self.get_stride(plane);
        }
        strides
    }

    pub fn get_data(&self, plane: usize) -> *mut u8 {
        self.get_plane_ptr(plane).cast_mut()
    }

    pub fn get_plane(&self, plane: usize) -> *const u8 {
        self.get_plane_ptr(plane)
    }

    pub fn get_plane_ptr(&self, plane: usize) -> *const u8 {
        unsafe { ffi::ruzu_ffmpeg_frame_plane(self.raw.cast_const(), plane as i32) }
    }

    pub fn get_planes(&self) -> [*mut u8; AV_NUM_DATA_POINTERS] {
        let mut planes = [std::ptr::null_mut(); AV_NUM_DATA_POINTERS];
        for (plane, data) in planes.iter_mut().enumerate() {
            *data = self.get_data(plane);
        }
        planes
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

    pub fn supports_decoding_on_device(&self, device_type: u32) -> Option<i32> {
        let mut pix_fmt = -1;
        let supported = unsafe {
            ffi::ruzu_ffmpeg_decoder_supports_decoding_on_device(
                self.codec as u64,
                device_type as i32,
                &mut pix_fmt,
            )
        };
        (supported != 0).then_some(pix_fmt)
    }
}

/// Wraps AVBufferRef for hardware-accelerated decoding.
///
/// Port of `FFmpeg::HardwareContext`.
pub struct HardwareContext {
    raw: *mut ffi::RuzuFfmpegHardwareContext,
}

impl HardwareContext {
    pub fn new() -> Self {
        Self {
            raw: unsafe { ffi::ruzu_ffmpeg_hardware_context_create() },
        }
    }

    pub fn get_supported_device_types() -> Vec<u32> {
        let count = unsafe { ffi::ruzu_ffmpeg_supported_device_types(std::ptr::null_mut(), 0) };
        if count == 0 {
            return Vec::new();
        }

        let mut types = vec![0i32; count as usize];
        let written =
            unsafe { ffi::ruzu_ffmpeg_supported_device_types(types.as_mut_ptr(), types.len()) };
        types.truncate(written.min(types.len()) as usize);
        types.into_iter().map(|value| value as u32).collect()
    }

    pub fn initialize_for_decoder(
        &mut self,
        decoder_context: &mut DecoderContext,
        decoder: &Decoder,
    ) -> bool {
        if self.raw.is_null() || decoder_context.raw.is_null() {
            return false;
        }
        let initialized = unsafe {
            ffi::ruzu_ffmpeg_hardware_initialize_for_decoder(
                self.raw,
                decoder_context.raw,
                decoder.codec as u64,
            )
        } != 0;
        if initialized {
            log::info!("Using FFmpeg GPU decoding");
        } else {
            log::info!("Hardware decoding is disabled due to implementation issues, using CPU.");
        }
        initialized
    }
}

impl Default for HardwareContext {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for HardwareContext {
    fn drop(&mut self) {
        if !self.raw.is_null() {
            unsafe { ffi::ruzu_ffmpeg_hardware_context_destroy(self.raw) };
            self.raw = std::ptr::null_mut();
        }
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
        // The Rust shim applies the upstream hw_device_ctx/get_format/pix_fmt
        // side effects inside HardwareContext::initialize_for_decoder.
    }

    fn prepare_context(&mut self, decoder: &Decoder) -> bool {
        if !self.raw.is_null() {
            return true;
        }
        self.raw = unsafe { ffi::ruzu_ffmpeg_decoder_create(decoder.codec as u64) };
        if self.raw.is_null() {
            log::error!(
                "FFmpeg::DecoderContext::prepare_context: failed to allocate codec {:?}",
                decoder.codec
            );
            return false;
        }
        true
    }

    pub fn open_context(&mut self, decoder: &Decoder) -> bool {
        if !self.prepare_context(decoder) {
            return false;
        }
        let ret = unsafe { ffi::ruzu_ffmpeg_decoder_open(self.raw) };
        if ret < 0 {
            log::error!(
                "FFmpeg::DecoderContext::open_context: avcodec_open2 error: {}",
                av_error(ret)
            );
            return false;
        }
        log::info!("Using FFmpeg software decoding");
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
        let frame = unsafe { ffi::ruzu_ffmpeg_decoder_receive_frame_with_hw_transfer(self.raw) };
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
        if !decoder_context.prepare_context(&decoder) {
            return false;
        }
        if *common::settings::values().nvdec_emulation.get_value()
            == common::settings_enums::NvdecEmulation::Gpu
        {
            let mut hardware_context = HardwareContext::new();
            if hardware_context.initialize_for_decoder(&mut decoder_context, &decoder) {
                self.hardware_context = Some(hardware_context);
            }
        }
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
    fn default_frame_exposes_empty_planes_and_strides() {
        let frame = Frame::new();
        assert_eq!(frame.get_strides(), [0; AV_NUM_DATA_POINTERS]);
        assert!(frame.get_planes().iter().all(|ptr| ptr.is_null()));
        assert!(frame.get_plane(0).is_null());
        assert!(frame.get_data(0).is_null());
    }

    #[test]
    fn decode_api_initializes_h264_software_decoder() {
        let mut api = DecodeApi::new();
        assert!(api.initialize(VideoCodec::H264));
        assert!(api.using_decode_order());
    }

    #[test]
    fn ffmpeg_hardware_capability_queries_are_wired() {
        let decoder = Decoder::new(VideoCodec::H264);
        for device_type in HardwareContext::get_supported_device_types() {
            let _ = decoder.supports_decoding_on_device(device_type);
        }
    }
}
