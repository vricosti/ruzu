//! Port of zuyu/src/core/hle/service/audio/hardware_opus_decoder.h and .cpp
//!
//! IHardwareOpusDecoder service.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
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
/// | 8   | DecodeInterleaved                                        |
/// | 9   | DecodeInterleavedForMultiStream                           |
pub struct IHardwareOpusDecoder {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IHardwareOpusDecoder {
    pub fn new() -> Self {
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
        }
    }

    /// Decode stub returning silent PCM (zeros) at a plausible sample count.
    ///
    /// The real decode is in `audio_core/src/opus/decoder.rs::decode_interleaved`
    /// but isn't yet wired through `AudioCoreInterface` (would require adding
    /// `OpusDecoderInterface` trait + AudioCore methods + per-session decoder
    /// state; ~700 lines across 4 files). Until then, this silent-decode stub
    /// reports:
    ///   consumed_size = entire input packet
    ///   sample_count  = 960 samples (one 20ms frame at 48 kHz)
    ///   output_buffer = zero-filled PCM (silence)
    ///
    /// Strictly better than the previous `consumed=0, samples=0` stub: games
    /// that gate progress on "decoder produced data" advance instead of
    /// looping forever on an empty stream.
    fn decode_stub(ctx: &mut HLERequestContext, name: &str, has_perf: bool) {
        log::debug!("IHardwareOpusDecoder::{} (silent-decode)", name);
        let opus_data = ctx.read_buffer(0);
        let consumed = opus_data.len() as u32;
        // 1 frame of stereo 16-bit silence at 48 kHz: 960 samples × 4 bytes.
        // Cap at the output buffer's actual capacity.
        const STEREO_FRAME_SAMPLES: u32 = 960;
        const STEREO_FRAME_BYTES: usize = STEREO_FRAME_SAMPLES as usize * 4;
        let out_cap = ctx.get_write_buffer_size(0);
        let out_size = out_cap.min(STEREO_FRAME_BYTES);
        let zeros = vec![0u8; out_size];
        ctx.write_buffer(&zeros, 0);
        let data_words = if has_perf { 5 } else { 4 };
        let mut rb = ResponseBuilder::new(ctx, data_words, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(consumed); // consumed size
        rb.push_u32(STEREO_FRAME_SAMPLES); // sample count
        if has_perf {
            rb.push_u64(0); // time taken (ns)
        }
    }

    fn decode_interleaved_old_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::decode_stub(ctx, "DecodeInterleavedOld", false);
    }

    fn set_context_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IHardwareOpusDecoder::SetContext (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn decode_interleaved_for_multi_stream_old_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::decode_stub(ctx, "DecodeInterleavedForMultiStreamOld", false);
    }

    fn set_context_for_multi_stream_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IHardwareOpusDecoder::SetContextForMultiStream (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn decode_interleaved_with_perf_old_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::decode_stub(ctx, "DecodeInterleavedWithPerfOld", true);
    }

    fn decode_interleaved_for_multi_stream_with_perf_old_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::decode_stub(ctx, "DecodeInterleavedForMultiStreamWithPerfOld", true);
    }

    fn decode_interleaved_with_perf_and_reset_old_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::decode_stub(ctx, "DecodeInterleavedWithPerfAndResetOld", true);
    }

    fn decode_interleaved_for_multi_stream_with_perf_and_reset_old_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::decode_stub(
            ctx,
            "DecodeInterleavedForMultiStreamWithPerfAndResetOld",
            true,
        );
    }

    fn decode_interleaved_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::decode_stub(ctx, "DecodeInterleaved", true);
    }

    fn decode_interleaved_for_multi_stream_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::decode_stub(ctx, "DecodeInterleavedForMultiStream", true);
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
