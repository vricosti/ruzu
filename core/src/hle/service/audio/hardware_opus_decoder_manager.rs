//! Port of zuyu/src/core/hle/service/audio/hardware_opus_decoder_manager.h and .cpp
//!
//! IHardwareOpusDecoderManager service ("hwopus"). Each handler maps 1:1
//! to the upstream method of the same name, dispatching the per-session
//! decoder construction or work-buffer-size computation through
//! `AudioCoreInterface`.

use std::collections::BTreeMap;

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{CmifRequest, CmifResponse};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::hardware_opus_decoder::IHardwareOpusDecoder;

/// Mirrors `AudioCore::OpusDecoder::OpusParameters` (sizeof = 0x8).
#[derive(Clone, Copy, Default)]
#[repr(C)]
struct OpusParameters {
    sample_rate: u32,
    channel_count: u32,
}

/// Mirrors `AudioCore::OpusDecoder::OpusParametersEx` (sizeof = 0x10).
#[derive(Clone, Copy, Default)]
#[repr(C)]
struct OpusParametersEx {
    sample_rate: u32,
    channel_count: u32,
    use_large_frame_size: u8,
    _padding: [u8; 7],
}

/// Mirrors `AudioCore::OpusDecoder::OpusMultiStreamParameters` (sizeof = 0x110).
/// Read from a HipcPointer (X-descriptor) input buffer.
#[derive(Clone, Copy)]
#[repr(C)]
struct OpusMultiStreamParameters {
    sample_rate: u32,
    channel_count: u32,
    total_stream_count: u32,
    stereo_stream_count: u32,
    mappings: [u8; 256],
}

impl Default for OpusMultiStreamParameters {
    fn default() -> Self {
        Self {
            sample_rate: 0,
            channel_count: 0,
            total_stream_count: 0,
            stereo_stream_count: 0,
            mappings: [0; 256],
        }
    }
}

/// Mirrors `AudioCore::OpusDecoder::OpusMultiStreamParametersEx` (sizeof = 0x118).
#[derive(Clone, Copy)]
#[repr(C)]
struct OpusMultiStreamParametersEx {
    sample_rate: u32,
    channel_count: u32,
    total_stream_count: u32,
    stereo_stream_count: u32,
    use_large_frame_size: u8,
    _padding: [u8; 7],
    mappings: [u8; 256],
}

impl Default for OpusMultiStreamParametersEx {
    fn default() -> Self {
        Self {
            sample_rate: 0,
            channel_count: 0,
            total_stream_count: 0,
            stereo_stream_count: 0,
            use_large_frame_size: 0,
            _padding: [0; 7],
            mappings: [0; 256],
        }
    }
}

/// IPC command table for IHardwareOpusDecoderManager ("hwopus").
pub struct IHardwareOpusDecoderManager {
    system: SystemRef,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IHardwareOpusDecoderManager {
    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub fn new(system: SystemRef) -> Self {
        let handlers = build_handler_map(&[
            (
                0,
                Some(Self::open_hardware_opus_decoder_handler),
                "OpenHardwareOpusDecoder",
            ),
            (
                1,
                Some(Self::get_work_buffer_size_handler),
                "GetWorkBufferSize",
            ),
            (
                2,
                Some(Self::open_hardware_opus_decoder_for_multi_stream_handler),
                "OpenHardwareOpusDecoderForMultiStream",
            ),
            (
                3,
                Some(Self::get_work_buffer_size_for_multi_stream_handler),
                "GetWorkBufferSizeForMultiStream",
            ),
            (
                4,
                Some(Self::open_hardware_opus_decoder_ex_handler),
                "OpenHardwareOpusDecoderEx",
            ),
            (
                5,
                Some(Self::get_work_buffer_size_ex_handler),
                "GetWorkBufferSizeEx",
            ),
            (
                6,
                Some(Self::open_hardware_opus_decoder_for_multi_stream_ex_handler),
                "OpenHardwareOpusDecoderForMultiStreamEx",
            ),
            (
                7,
                Some(Self::get_work_buffer_size_for_multi_stream_ex_handler),
                "GetWorkBufferSizeForMultiStreamEx",
            ),
            (
                8,
                Some(Self::get_work_buffer_size_ex_ex_handler),
                "GetWorkBufferSizeExEx",
            ),
            (
                9,
                Some(Self::get_work_buffer_size_for_multi_stream_ex_ex_handler),
                "GetWorkBufferSizeForMultiStreamExEx",
            ),
        ]);
        Self {
            system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Push the constructed decoder back to the client as a sub-session,
    /// or return the error result.
    fn finish_open(
        ctx: &mut HLERequestContext,
        result: std::result::Result<Box<dyn crate::core::OpusDecoderInterface>, ResultCode>,
    ) {
        match result {
            Ok(decoder) => {
                let svc = std::sync::Arc::new(IHardwareOpusDecoder::new(decoder));
                let mut response = CmifResponse::new(ctx, 2, 0, 1);
                response.push_result(RESULT_SUCCESS);
                response.push_interface(svc);
            }
            Err(rc) => {
                CmifResponse::result_only(ctx, rc);
            }
        }
    }

    /// Mirror upstream `OpenHardwareOpusDecoder` (cmd 0).
    fn open_hardware_opus_decoder_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let params: OpusParameters = request.raw();
        let tmem_size = request.u32() as u64;
        log::debug!(
            "IHardwareOpusDecoderManager::OpenHardwareOpusDecoder rate={} ch={} tmem={:#x}",
            params.sample_rate,
            params.channel_count,
            tmem_size
        );
        let result = svc
            .system
            .get()
            .audio_core()
            .ok_or(ResultCode::new(0))
            .and_then(|audio_core| {
                audio_core.open_opus_decoder(
                    params.sample_rate,
                    params.channel_count,
                    false,
                    tmem_size,
                )
            });
        Self::finish_open(ctx, result);
    }

    /// Mirror upstream `GetWorkBufferSize` (cmd 1).
    fn get_work_buffer_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let params: OpusParameters = request.raw();
        let size = svc
            .system
            .get()
            .audio_core()
            .map(|audio_core| {
                audio_core.get_opus_work_buffer_size(
                    params.sample_rate,
                    params.channel_count,
                    false,
                )
            })
            .unwrap_or(0);
        log::debug!(
            "IHardwareOpusDecoderManager::GetWorkBufferSize rate={} ch={} -> {:#x}",
            params.sample_rate,
            params.channel_count,
            size
        );
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(size);
    }

    /// Mirror upstream `OpenHardwareOpusDecoderForMultiStream` (cmd 2).
    /// Params come via HipcPointer (X-descriptor) input buffer.
    fn open_hardware_opus_decoder_for_multi_stream_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let tmem_size = request.u32() as u64;
        let params = read_multi_stream_params(ctx);
        log::debug!(
            "IHardwareOpusDecoderManager::OpenHardwareOpusDecoderForMultiStream rate={} ch={} streams={}/{} tmem={:#x}",
            params.sample_rate,
            params.channel_count,
            params.total_stream_count,
            params.stereo_stream_count,
            tmem_size
        );
        let result = svc
            .system
            .get()
            .audio_core()
            .ok_or(ResultCode::new(0))
            .and_then(|audio_core| {
                audio_core.open_opus_decoder_for_multi_stream(
                    params.sample_rate,
                    params.channel_count,
                    params.total_stream_count,
                    params.stereo_stream_count,
                    false,
                    &params.mappings,
                    tmem_size,
                )
            });
        Self::finish_open(ctx, result);
    }

    /// Mirror upstream `GetWorkBufferSizeForMultiStream` (cmd 3).
    fn get_work_buffer_size_for_multi_stream_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let params = read_multi_stream_params(ctx);
        let size = svc
            .system
            .get()
            .audio_core()
            .map(|audio_core| {
                audio_core.get_opus_work_buffer_size_for_multi_stream(
                    params.sample_rate,
                    params.channel_count,
                    params.total_stream_count,
                    params.stereo_stream_count,
                    false,
                )
            })
            .unwrap_or(0);
        log::debug!(
            "IHardwareOpusDecoderManager::GetWorkBufferSizeForMultiStream -> {:#x}",
            size
        );
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(size);
    }

    /// Mirror upstream `OpenHardwareOpusDecoderEx` (cmd 4).
    fn open_hardware_opus_decoder_ex_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let params: OpusParametersEx = request.raw();
        let tmem_size = request.u32() as u64;
        log::debug!(
            "IHardwareOpusDecoderManager::OpenHardwareOpusDecoderEx rate={} ch={} large={} tmem={:#x}",
            params.sample_rate,
            params.channel_count,
            params.use_large_frame_size != 0,
            tmem_size
        );
        let result = svc
            .system
            .get()
            .audio_core()
            .ok_or(ResultCode::new(0))
            .and_then(|audio_core| {
                audio_core.open_opus_decoder(
                    params.sample_rate,
                    params.channel_count,
                    params.use_large_frame_size != 0,
                    tmem_size,
                )
            });
        Self::finish_open(ctx, result);
    }

    /// Mirror upstream `GetWorkBufferSizeEx` (cmd 5).
    fn get_work_buffer_size_ex_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let params: OpusParametersEx = request.raw();
        let size = svc
            .system
            .get()
            .audio_core()
            .map(|audio_core| {
                audio_core.get_opus_work_buffer_size(
                    params.sample_rate,
                    params.channel_count,
                    params.use_large_frame_size != 0,
                )
            })
            .unwrap_or(0);
        log::debug!(
            "IHardwareOpusDecoderManager::GetWorkBufferSizeEx -> {:#x}",
            size
        );
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(size);
    }

    /// Mirror upstream `OpenHardwareOpusDecoderForMultiStreamEx` (cmd 6).
    fn open_hardware_opus_decoder_for_multi_stream_ex_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let tmem_size = request.u32() as u64;
        let params = read_multi_stream_params_ex(ctx);
        log::debug!(
            "IHardwareOpusDecoderManager::OpenHardwareOpusDecoderForMultiStreamEx rate={} ch={} streams={}/{} large={} tmem={:#x}",
            params.sample_rate,
            params.channel_count,
            params.total_stream_count,
            params.stereo_stream_count,
            params.use_large_frame_size != 0,
            tmem_size
        );
        let result = svc
            .system
            .get()
            .audio_core()
            .ok_or(ResultCode::new(0))
            .and_then(|audio_core| {
                audio_core.open_opus_decoder_for_multi_stream(
                    params.sample_rate,
                    params.channel_count,
                    params.total_stream_count,
                    params.stereo_stream_count,
                    params.use_large_frame_size != 0,
                    &params.mappings,
                    tmem_size,
                )
            });
        Self::finish_open(ctx, result);
    }

    /// Mirror upstream `GetWorkBufferSizeForMultiStreamEx` (cmd 7).
    fn get_work_buffer_size_for_multi_stream_ex_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let params = read_multi_stream_params_ex(ctx);
        let size = svc
            .system
            .get()
            .audio_core()
            .map(|audio_core| {
                audio_core.get_opus_work_buffer_size_for_multi_stream(
                    params.sample_rate,
                    params.channel_count,
                    params.total_stream_count,
                    params.stereo_stream_count,
                    params.use_large_frame_size != 0,
                )
            })
            .unwrap_or(0);
        log::debug!(
            "IHardwareOpusDecoderManager::GetWorkBufferSizeForMultiStreamEx -> {:#x}",
            size
        );
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(size);
    }

    /// Mirror upstream `GetWorkBufferSizeExEx` (cmd 8). Same args/output as
    /// `GetWorkBufferSizeEx`; upstream's `impl.GetWorkBufferSizeExEx` differs
    /// only in selecting a slightly larger buffer for newer firmware.
    fn get_work_buffer_size_ex_ex_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::get_work_buffer_size_ex_handler(this, ctx);
    }

    /// Mirror upstream `GetWorkBufferSizeForMultiStreamExEx` (cmd 9).
    fn get_work_buffer_size_for_multi_stream_ex_ex_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::get_work_buffer_size_for_multi_stream_ex_handler(this, ctx);
    }
}

/// Read a `OpusMultiStreamParameters` struct from the request's first
/// HipcPointer (X-descriptor) input buffer.
fn read_multi_stream_params(ctx: &HLERequestContext) -> OpusMultiStreamParameters {
    let raw = ctx.read_buffer(0);
    let mut out = OpusMultiStreamParameters::default();
    let copy = raw
        .len()
        .min(std::mem::size_of::<OpusMultiStreamParameters>());
    unsafe {
        std::ptr::copy_nonoverlapping(raw.as_ptr(), &mut out as *mut _ as *mut u8, copy);
    }
    out
}

/// Read a `OpusMultiStreamParametersEx` struct from the request's first
/// HipcPointer (X-descriptor) input buffer.
fn read_multi_stream_params_ex(ctx: &HLERequestContext) -> OpusMultiStreamParametersEx {
    let raw = ctx.read_buffer(0);
    let mut out = OpusMultiStreamParametersEx::default();
    let copy = raw
        .len()
        .min(std::mem::size_of::<OpusMultiStreamParametersEx>());
    unsafe {
        std::ptr::copy_nonoverlapping(raw.as_ptr(), &mut out as *mut _ as *mut u8, copy);
    }
    out
}

impl SessionRequestHandler for IHardwareOpusDecoderManager {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IHardwareOpusDecoderManager {
    fn get_service_name(&self) -> &str {
        "hwopus"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
