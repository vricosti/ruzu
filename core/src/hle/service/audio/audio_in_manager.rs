//! Port of zuyu/src/core/hle/service/audio/audio_in_manager.h and audio_in_manager.cpp
//!
//! IAudioInManager service ("audin:u").

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioInManager ("audin:u"):
///
/// | Cmd | Name                          |
/// |-----|-------------------------------|
/// | 0   | ListAudioIns                  |
/// | 1   | OpenAudioIn                   |
/// | 2   | ListAudioInsAuto              |
/// | 3   | OpenAudioInAuto               |
/// | 4   | ListAudioInsAutoFiltered      |
/// | 5   | OpenAudioInProtocolSpecified   |
pub struct IAudioInManager {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAudioInManager {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::list_audio_ins_handler), "ListAudioIns"),
            (1, Some(Self::open_audio_in_handler), "OpenAudioIn"),
            (
                2,
                Some(Self::list_audio_ins_auto_handler),
                "ListAudioInsAuto",
            ),
            (3, Some(Self::open_audio_in_auto_handler), "OpenAudioInAuto"),
            (
                4,
                Some(Self::list_audio_ins_auto_filtered_handler),
                "ListAudioInsAutoFiltered",
            ),
            (
                5,
                Some(Self::open_audio_in_protocol_specified_handler),
                "OpenAudioInProtocolSpecified",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Write the default audio input device name to the output buffer.
    fn write_audio_in_name(ctx: &mut HLERequestContext) {
        let name = b"AudioBuiltInU\0";
        ctx.write_buffer(name, 0);
    }

    fn list_audio_ins_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioInManager::ListAudioIns (STUBBED)");
        Self::write_audio_in_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // one audio in device
    }

    fn open_audio_in_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioInManager::OpenAudioIn (STUBBED)");
        Self::write_audio_in_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(48000); // sample rate
        rb.push_u32(2); // channel count
        rb.push_u32(2); // pcm format (Int16)
        rb.push_u32(0); // state (Stopped)
    }

    fn list_audio_ins_auto_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioInManager::ListAudioInsAuto (STUBBED)");
        Self::write_audio_in_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // one audio in device
    }

    fn open_audio_in_auto_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioInManager::OpenAudioInAuto (STUBBED)");
        Self::write_audio_in_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(48000); // sample rate
        rb.push_u32(2); // channel count
        rb.push_u32(2); // pcm format (Int16)
        rb.push_u32(0); // state (Stopped)
    }

    fn list_audio_ins_auto_filtered_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioInManager::ListAudioInsAutoFiltered (STUBBED)");
        Self::write_audio_in_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // one audio in device
    }

    fn open_audio_in_protocol_specified_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IAudioInManager::OpenAudioInProtocolSpecified (STUBBED)");
        Self::write_audio_in_name(ctx);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(48000); // sample rate
        rb.push_u32(2); // channel count
        rb.push_u32(2); // pcm format (Int16)
        rb.push_u32(0); // state (Stopped)
    }
}

impl SessionRequestHandler for IAudioInManager {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioInManager {
    fn get_service_name(&self) -> &str {
        "audin:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
