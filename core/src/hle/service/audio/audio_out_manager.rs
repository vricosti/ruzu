//! Port of zuyu/src/core/hle/service/audio/audio_out_manager.h and audio_out_manager.cpp
//!
//! IAudioOutManager service ("audout:u").

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{write_out_array_bytes, CmifResponse};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioOutManager ("audout:u"):
///
/// | Cmd | Name              |
/// |-----|-------------------|
/// | 0   | ListAudioOuts     |
/// | 1   | OpenAudioOut      |
/// | 2   | ListAudioOutsAuto |
/// | 3   | OpenAudioOutAuto  |
pub struct IAudioOutManager {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAudioOutManager {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::list_audio_outs_handler), "ListAudioOuts"),
            (1, Some(Self::open_audio_out_handler), "OpenAudioOut"),
            (
                2,
                Some(Self::list_audio_outs_auto_handler),
                "ListAudioOutsAuto",
            ),
            (
                3,
                Some(Self::open_audio_out_auto_handler),
                "OpenAudioOutAuto",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Write the default audio output device name to the output buffer.
    fn audio_out_name_bytes() -> &'static [u8] {
        // Upstream returns "AudioTvOutput" as the default device name.
        // The name is written as a null-terminated UTF-8 string.
        b"AudioTvOutput\0"
    }

    fn list_audio_outs_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioOutManager::ListAudioOuts (STUBBED)");
        write_out_array_bytes(ctx, 0, Self::audio_out_name_bytes());
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(1); // one audio out device
    }

    /// Port of upstream `IAudioOutManager::OpenAudioOut`.
    /// Creates an IAudioOut sub-session.
    fn open_audio_out_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioOutManager::OpenAudioOut");
        write_out_array_bytes(ctx, 0, Self::audio_out_name_bytes());
        let audio_out = Arc::new(super::audio_out::IAudioOut::new());
        let mut response = CmifResponse::new(ctx, 6, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(48000); // sample rate
        response.push_u32(2); // channel count
        response.push_u32(2); // pcm format (Int16)
        response.push_u32(0); // state (Stopped)
        response.push_interface(audio_out);
    }

    fn list_audio_outs_auto_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IAudioOutManager::ListAudioOutsAuto (STUBBED)");
        write_out_array_bytes(ctx, 0, Self::audio_out_name_bytes());
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(1); // one audio out device
    }

    /// Port of upstream `IAudioOutManager::OpenAudioOutAuto`.
    /// Creates an IAudioOut sub-session.
    fn open_audio_out_auto_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioOutManager::OpenAudioOutAuto");
        write_out_array_bytes(ctx, 0, Self::audio_out_name_bytes());
        let audio_out = Arc::new(super::audio_out::IAudioOut::new());
        let mut response = CmifResponse::new(ctx, 6, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(48000); // sample rate
        response.push_u32(2); // channel count
        response.push_u32(2); // pcm format (Int16)
        response.push_u32(0); // state (Stopped)
        response.push_interface(audio_out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn audio_out_manager_registers_upstream_command_ids() {
        let service = IAudioOutManager::new();

        for cmd in [0_u32, 1, 2, 3] {
            assert!(service.handlers.contains_key(&cmd));
            assert!(service.handlers[&cmd].handler_callback.is_some());
        }
    }
}

impl SessionRequestHandler for IAudioOutManager {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioOutManager {
    fn get_service_name(&self) -> &str {
        "audout:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
