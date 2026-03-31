//! Port of zuyu/src/core/hle/service/audio/hardware_opus_decoder_manager.h and .cpp
//!
//! IHardwareOpusDecoderManager service ("hwopus").

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IHardwareOpusDecoderManager ("hwopus"):
///
/// | Cmd | Name                                         |
/// |-----|----------------------------------------------|
/// | 0   | OpenHardwareOpusDecoder                      |
/// | 1   | GetWorkBufferSize                            |
/// | 2   | OpenOpusDecoderForMultiStream                |
/// | 3   | GetWorkBufferSizeForMultiStream              |
/// | 4   | OpenHardwareOpusDecoderEx                    |
/// | 5   | GetWorkBufferSizeEx                          |
/// | 6   | OpenHardwareOpusDecoderForMultiStreamEx       |
/// | 7   | GetWorkBufferSizeForMultiStreamEx            |
/// | 8   | GetWorkBufferSizeExEx                        |
/// | 9   | GetWorkBufferSizeForMultiStreamExEx          |
pub struct IHardwareOpusDecoderManager {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IHardwareOpusDecoderManager {
    pub fn new() -> Self {
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
                Some(Self::open_opus_decoder_for_multi_stream_handler),
                "OpenOpusDecoderForMultiStream",
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
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn open_hardware_opus_decoder_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IHardwareOpusDecoderManager::OpenHardwareOpusDecoder (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_work_buffer_size_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IHardwareOpusDecoderManager::GetWorkBufferSize (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0x4000); // reasonable default
    }

    fn open_opus_decoder_for_multi_stream_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IHardwareOpusDecoderManager::OpenOpusDecoderForMultiStream (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_work_buffer_size_for_multi_stream_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IHardwareOpusDecoderManager::GetWorkBufferSizeForMultiStream (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0x4000);
    }

    fn open_hardware_opus_decoder_ex_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IHardwareOpusDecoderManager::OpenHardwareOpusDecoderEx (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_work_buffer_size_ex_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IHardwareOpusDecoderManager::GetWorkBufferSizeEx (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0x4000);
    }

    fn open_hardware_opus_decoder_for_multi_stream_ex_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!(
            "IHardwareOpusDecoderManager::OpenHardwareOpusDecoderForMultiStreamEx (STUBBED)"
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_work_buffer_size_for_multi_stream_ex_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IHardwareOpusDecoderManager::GetWorkBufferSizeForMultiStreamEx (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0x4000);
    }

    fn get_work_buffer_size_ex_ex_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IHardwareOpusDecoderManager::GetWorkBufferSizeExEx (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0x4000);
    }

    fn get_work_buffer_size_for_multi_stream_ex_ex_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IHardwareOpusDecoderManager::GetWorkBufferSizeForMultiStreamExEx (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0x4000);
    }
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
