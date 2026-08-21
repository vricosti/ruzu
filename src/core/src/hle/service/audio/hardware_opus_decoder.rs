//! Port of zuyu/src/core/hle/service/audio/hardware_opus_decoder.h and .cpp
//!
//! IHardwareOpusDecoder service. Each IPC handler corresponds 1:1 to the
//! upstream method of the same name and dispatches to the per-session
//! `OpusDecoderInterface` (an audio_core-implemented trait).

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::core::OpusDecoderInterface;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{CmifRequest, CmifResponse};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IHardwareOpusDecoder:
///
/// | Cmd | Name                                                      |
/// |-----|-----------------------------------------------------------|
/// | 0   | DecodeInterleavedOld                                      |
/// | 1   | SetContext                                                |
/// | 2   | DecodeInterleavedForMultiStreamOld                        |
/// | 3   | SetContextForMultiStream                                  |
/// | 4   | DecodeInterleavedWithPerfOld                              |
/// | 5   | DecodeInterleavedForMultiStreamWithPerfOld                |
/// | 6   | DecodeInterleavedWithPerfAndResetOld                      |
/// | 7   | DecodeInterleavedForMultiStreamWithPerfAndResetOld        |
/// | 8   | DecodeInterleaved                                         |
/// | 9   | DecodeInterleavedForMultiStream                           |
pub struct IHardwareOpusDecoder {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    decoder: Mutex<Box<dyn OpusDecoderInterface>>,
}

impl IHardwareOpusDecoder {
    /// Create a service wrapping a per-session decoder constructed by
    /// `IHardwareOpusDecoderManager::OpenHardwareOpusDecoder*`.
    pub fn new(decoder: Box<dyn OpusDecoderInterface>) -> Self {
        let handlers = build_handler_map(&[
            (
                0,
                Some(Self::decode_interleaved_old_handler),
                "DecodeInterleavedOld",
            ),
            (1, Some(Self::set_context_handler), "SetContext"),
            (
                2,
                Some(Self::decode_interleaved_for_multi_stream_old_handler),
                "DecodeInterleavedForMultiStreamOld",
            ),
            (
                3,
                Some(Self::set_context_for_multi_stream_handler),
                "SetContextForMultiStream",
            ),
            (
                4,
                Some(Self::decode_interleaved_with_perf_old_handler),
                "DecodeInterleavedWithPerfOld",
            ),
            (
                5,
                Some(Self::decode_interleaved_for_multi_stream_with_perf_old_handler),
                "DecodeInterleavedForMultiStreamWithPerfOld",
            ),
            (
                6,
                Some(Self::decode_interleaved_with_perf_and_reset_old_handler),
                "DecodeInterleavedWithPerfAndResetOld",
            ),
            (
                7,
                Some(Self::decode_interleaved_for_multi_stream_with_perf_and_reset_old_handler),
                "DecodeInterleavedForMultiStreamWithPerfAndResetOld",
            ),
            (
                8,
                Some(Self::decode_interleaved_handler),
                "DecodeInterleaved",
            ),
            (
                9,
                Some(Self::decode_interleaved_for_multi_stream_handler),
                "DecodeInterleavedForMultiStream",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            decoder: Mutex::new(decoder),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    /// Common decode-and-respond logic. `multi` selects the multi-stream
    /// trait method; `with_perf` adds the time_taken u64 to the response;
    /// `reset` is the value passed to the decoder (false for old variants
    /// without a reset flag). Mirror of upstream's `R_TRY(impl->Decode...(...))`
    /// + `R_SUCCEED()` boilerplate.
    fn dispatch_decode(
        &self,
        ctx: &mut HLERequestContext,
        multi: bool,
        with_perf: bool,
        reset: bool,
    ) {
        let opus_data = ctx.read_buffer(0);
        let out_cap = ctx.get_write_buffer_size(0);
        let mut out_pcm = vec![0u8; out_cap];
        let mut decoder = self.decoder.lock().unwrap();
        let result = if multi {
            decoder.decode_interleaved_for_multi_stream(&opus_data, &mut out_pcm, reset)
        } else {
            decoder.decode_interleaved(&opus_data, &mut out_pcm, reset)
        };
        match result {
            Ok((data_size, sample_count, time_taken)) => {
                ctx.write_buffer(&out_pcm, 0);
                let data_words = if with_perf { 5 } else { 4 };
                let mut rb = ResponseBuilder::new(ctx, data_words, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u32(data_size);
                rb.push_u32(sample_count);
                if with_perf {
                    rb.push_u64(time_taken);
                }
            }
            Err(rc) => {
                CmifResponse::result_only(ctx, rc);
            }
        }
    }

    fn decode_interleaved_old_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::as_self(this).dispatch_decode(ctx, false, false, false);
    }

    fn set_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let context = ctx.read_buffer(0);
        let rc = svc.decoder.lock().unwrap().set_context(&context);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn decode_interleaved_for_multi_stream_old_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::as_self(this).dispatch_decode(ctx, true, false, false);
    }

    fn set_context_for_multi_stream_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        // Upstream forwards both SetContext variants to the same impl.SetContext.
        Self::set_context_handler(this, ctx);
    }

    fn decode_interleaved_with_perf_old_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::as_self(this).dispatch_decode(ctx, false, true, false);
    }

    fn decode_interleaved_for_multi_stream_with_perf_old_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::as_self(this).dispatch_decode(ctx, true, true, false);
    }

    fn decode_interleaved_with_perf_and_reset_old_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let mut request = CmifRequest::new(ctx);
        let reset = request.u32() != 0;
        Self::as_self(this).dispatch_decode(ctx, false, true, reset);
    }

    fn decode_interleaved_for_multi_stream_with_perf_and_reset_old_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let mut request = CmifRequest::new(ctx);
        let reset = request.u32() != 0;
        Self::as_self(this).dispatch_decode(ctx, true, true, reset);
    }

    fn decode_interleaved_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut request = CmifRequest::new(ctx);
        let reset = request.u32() != 0;
        Self::as_self(this).dispatch_decode(ctx, false, true, reset);
    }

    fn decode_interleaved_for_multi_stream_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let mut request = CmifRequest::new(ctx);
        let reset = request.u32() != 0;
        Self::as_self(this).dispatch_decode(ctx, true, true, reset);
    }
}

impl SessionRequestHandler for IHardwareOpusDecoder {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IHardwareOpusDecoder {
    fn get_service_name(&self) -> &str {
        "IHardwareOpusDecoder"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
