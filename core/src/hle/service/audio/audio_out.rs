//! Port of zuyu/src/core/hle/service/audio/audio_out.h and audio_out.cpp
//!
//! IAudioOut service.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, Weak};

use crate::core::{AudioOutBufferWire, AudioOutSession};
use crate::hle::kernel::k_event::KEvent;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_process::ProcessLock;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::kernel::KernelCore;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{
    CmifInArrayBuffer, CmifOutArrayBuffer, CmifRequest, CmifResponse,
};
use crate::hle::service::cmif_types::{buffer_attr, InArray, OutArray};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioOut:
///
/// | Cmd | Name                            |
/// |-----|---------------------------------|
/// | 0   | GetAudioOutState                |
/// | 1   | Start                           |
/// | 2   | Stop                            |
/// | 3   | AppendAudioOutBuffer            |
/// | 4   | RegisterBufferEvent             |
/// | 5   | GetReleasedAudioOutBuffers      |
/// | 6   | ContainsAudioOutBuffer          |
/// | 7   | AppendAudioOutBufferAuto        |
/// | 8   | GetReleasedAudioOutBuffersAuto  |
/// | 9   | GetAudioOutBufferCount          |
/// | 10  | GetAudioOutPlayedSampleCount    |
/// | 11  | FlushAudioOutBuffers            |
/// | 12  | SetAudioOutVolume               |
/// | 13  | GetAudioOutVolume               |
pub struct IAudioOut {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    session: Mutex<AudioOutSession>,
    owner_process: Weak<ProcessLock>,
    buffer_event_object_id: u64,
    buffer_readable_event_object_id: u64,
    buffer_event: Arc<Mutex<KEvent>>,
    buffer_readable_event: Arc<Mutex<KReadableEvent>>,
}

impl IAudioOut {
    pub fn new(
        session: AudioOutSession,
        owner_process: Arc<ProcessLock>,
        buffer_event_object_id: u64,
        buffer_readable_event_object_id: u64,
        buffer_event: Arc<Mutex<KEvent>>,
        buffer_readable_event: Arc<Mutex<KReadableEvent>>,
    ) -> Self {
        session.set_buffer_readable_event(Arc::clone(&buffer_readable_event));
        session.set_process_arc(Arc::clone(&owner_process));

        let handlers = build_handler_map(&[
            (
                0,
                Some(Self::get_audio_out_state_handler),
                "GetAudioOutState",
            ),
            (1, Some(Self::start_handler), "Start"),
            (2, Some(Self::stop_handler), "Stop"),
            (
                3,
                Some(Self::append_audio_out_buffer_handler),
                "AppendAudioOutBuffer",
            ),
            (
                4,
                Some(Self::register_buffer_event_handler),
                "RegisterBufferEvent",
            ),
            (
                5,
                Some(Self::get_released_audio_out_buffers_handler),
                "GetReleasedAudioOutBuffers",
            ),
            (
                6,
                Some(Self::contains_audio_out_buffer_handler),
                "ContainsAudioOutBuffer",
            ),
            (
                7,
                Some(Self::append_audio_out_buffer_auto_handler),
                "AppendAudioOutBufferAuto",
            ),
            (
                8,
                Some(Self::get_released_audio_out_buffers_auto_handler),
                "GetReleasedAudioOutBuffersAuto",
            ),
            (
                9,
                Some(Self::get_audio_out_buffer_count_handler),
                "GetAudioOutBufferCount",
            ),
            (
                10,
                Some(Self::get_audio_out_played_sample_count_handler),
                "GetAudioOutPlayedSampleCount",
            ),
            (
                11,
                Some(Self::flush_audio_out_buffers_handler),
                "FlushAudioOutBuffers",
            ),
            (
                12,
                Some(Self::set_audio_out_volume_handler),
                "SetAudioOutVolume",
            ),
            (
                13,
                Some(Self::get_audio_out_volume_handler),
                "GetAudioOutVolume",
            ),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            session: Mutex::new(session),
            owner_process: Arc::downgrade(&owner_process),
            buffer_event_object_id,
            buffer_readable_event_object_id,
            buffer_event,
            buffer_readable_event,
        }
    }

    pub(crate) fn create_buffer_event(
        kernel: &KernelCore,
        owner_process: &Arc<ProcessLock>,
    ) -> (u64, u64, Arc<Mutex<KEvent>>, Arc<Mutex<KReadableEvent>>) {
        let buffer_event_object_id = kernel.create_new_object_id() as u64;
        let buffer_readable_event_object_id = kernel.create_new_object_id() as u64;

        let mut event = KEvent::new();
        let mut readable_event = KReadableEvent::new();

        let owner_process_id = owner_process.lock().unwrap().get_process_id();
        event.initialize(owner_process_id, buffer_readable_event_object_id);
        readable_event.initialize(buffer_event_object_id, buffer_readable_event_object_id);

        let event = Arc::new(Mutex::new(event));
        let readable_event = Arc::new(Mutex::new(readable_event));

        {
            let mut owner = owner_process.lock().unwrap();
            owner.register_event_object(buffer_event_object_id, Arc::clone(&event));
            owner.register_readable_event_object(
                buffer_readable_event_object_id,
                Arc::clone(&readable_event),
            );
        }

        (
            buffer_event_object_id,
            buffer_readable_event_object_id,
            event,
            readable_event,
        )
    }

    fn append_audio_out_buffer_auto<const A: i32>(
        &self,
        buffer: &InArray<AudioOutBufferWire, A>,
        buffer_client_ptr: u64,
    ) -> ResultCode {
        if buffer.is_empty() {
            return ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_INSUFFICIENT_BUFFER.1,
            );
        }

        self.session
            .lock()
            .unwrap()
            .append_buffer(buffer[0], buffer_client_ptr)
    }

    fn get_released_audio_out_buffers_auto<const A: i32>(
        &self,
        out_audio_buffer: &mut OutArray<u64, A>,
        out_count: &mut u32,
    ) -> ResultCode {
        if !out_audio_buffer.is_empty() {
            out_audio_buffer[0] = 0;
        }
        *out_count = self
            .session
            .lock()
            .unwrap()
            .get_released_buffers(out_audio_buffer) as u32;
        RESULT_SUCCESS
    }

    fn get_audio_out_state_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(svc.session.lock().unwrap().get_state());
    }

    fn start_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let result = svc.session.lock().unwrap().start();
        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn stop_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let result = svc.session.lock().unwrap().stop();
        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn append_audio_out_buffer_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let buffer_client_ptr = request.u64();
        let mut buffer_storage = CmifInArrayBuffer::<
            AudioOutBufferWire,
            { buffer_attr::BufferAttr_HipcMapAlias },
        >::from_ctx(ctx, 0);
        let buffer = buffer_storage.as_in_array();
        let result = svc.append_audio_out_buffer_auto(&buffer, buffer_client_ptr);
        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn register_buffer_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let _ = svc.buffer_event.lock().unwrap().is_initialized();
        let Some(handle) =
            ctx.copy_handle_for_readable_event(Arc::clone(&svc.buffer_readable_event))
        else {
            CmifResponse::result_only(
                ctx,
                ResultCode::from_module_description(
                    crate::hle::result::ErrorModule::Audio,
                    crate::hle::service::audio::errors::RESULT_OPERATION_FAILED.1,
                ),
            );
            return;
        };

        let mut response = CmifResponse::new(ctx, 2, 1, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_copy_objects(handle);
    }

    fn get_released_audio_out_buffers_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut out_count = 0u32;
        let mut out_storage =
            CmifOutArrayBuffer::<u64, { buffer_attr::BufferAttr_HipcMapAlias }>::from_ctx(ctx, 0);
        let mut out_audio_buffer = out_storage.as_out_array();
        let result = svc.get_released_audio_out_buffers_auto(&mut out_audio_buffer, &mut out_count);
        out_storage.write_back(ctx, 0, out_count as usize);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(result);
        response.push_u32(out_count);
    }

    fn contains_audio_out_buffer_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let buffer_client_ptr = request.u64();
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_bool(
            svc.session
                .lock()
                .unwrap()
                .contains_buffer(buffer_client_ptr),
        );
    }

    fn append_audio_out_buffer_auto_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let buffer_client_ptr = request.u64();
        let mut buffer_storage = CmifInArrayBuffer::<
            AudioOutBufferWire,
            { buffer_attr::BufferAttr_HipcAutoSelect },
        >::from_ctx(ctx, 0);
        let buffer = buffer_storage.as_in_array();
        let result = svc.append_audio_out_buffer_auto(&buffer, buffer_client_ptr);
        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn get_released_audio_out_buffers_auto_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut out_count = 0u32;
        let mut out_storage =
            CmifOutArrayBuffer::<u64, { buffer_attr::BufferAttr_HipcAutoSelect }>::from_ctx(ctx, 0);
        let mut out_audio_buffer = out_storage.as_out_array();
        let result = svc.get_released_audio_out_buffers_auto(&mut out_audio_buffer, &mut out_count);
        out_storage.write_back(ctx, 0, out_count as usize);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(result);
        response.push_u32(out_count);
    }

    fn get_audio_out_buffer_count_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u32(svc.session.lock().unwrap().get_buffer_count());
    }

    fn get_audio_out_played_sample_count_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut response = CmifResponse::new(ctx, 4, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u64(svc.session.lock().unwrap().get_played_sample_count());
    }

    fn flush_audio_out_buffers_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_bool(svc.session.lock().unwrap().flush_audio_out_buffers());
    }

    fn set_audio_out_volume_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let volume = request.f32();
        svc.session.lock().unwrap().set_volume(volume);
        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(RESULT_SUCCESS);
    }

    fn get_audio_out_volume_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_f32(svc.session.lock().unwrap().get_volume());
    }
}

impl Drop for IAudioOut {
    fn drop(&mut self) {
        if let Some(owner_process) = self.owner_process.upgrade() {
            let mut owner = owner_process.lock().unwrap();
            owner.unregister_readable_event_object_by_object_id(
                self.buffer_readable_event_object_id,
            );
            owner.unregister_event_object_by_object_id(self.buffer_event_object_id);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestAudioOutSession;

    impl crate::core::AudioOutSessionImpl for TestAudioOutSession {
        fn get_state(&self) -> u32 {
            1
        }
        fn start(&self) -> ResultCode {
            RESULT_SUCCESS
        }
        fn stop(&self) -> ResultCode {
            RESULT_SUCCESS
        }
        fn append_buffer(
            &self,
            _buffer: AudioOutBufferWire,
            _buffer_client_ptr: u64,
        ) -> ResultCode {
            RESULT_SUCCESS
        }
        fn get_released_buffers(&self, _out_tags: &mut [u64]) -> u32 {
            0
        }
        fn contains_buffer(&self, _buffer_client_ptr: u64) -> bool {
            false
        }
        fn get_buffer_count(&self) -> u32 {
            0
        }
        fn get_played_sample_count(&self) -> u64 {
            0
        }
        fn flush_audio_out_buffers(&self) -> bool {
            true
        }
        fn set_volume(&self, _volume: f32) {}
        fn get_volume(&self) -> f32 {
            1.0
        }
    }

    #[test]
    fn audio_out_registers_upstream_command_ids() {
        let owner_process = Arc::new(ProcessLock::from_value(KProcess::new()));
        let kernel = KernelCore::new();
        let (event_id, readable_event_id, event, readable_event) =
            IAudioOut::create_buffer_event(&kernel, &owner_process);
        let service = IAudioOut::new(
            AudioOutSession::from_arc(Arc::new(TestAudioOutSession)),
            owner_process,
            event_id,
            readable_event_id,
            event,
            readable_event,
        );

        for cmd in [0_u32, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13] {
            assert!(service.handlers.contains_key(&cmd));
            assert!(service.handlers[&cmd].handler_callback.is_some());
        }
    }

    #[test]
    fn audio_out_buffer_wire_matches_upstream_size() {
        assert_eq!(std::mem::size_of::<AudioOutBufferWire>(), 0x28);
    }
}

impl SessionRequestHandler for IAudioOut {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioOut {
    fn get_service_name(&self) -> &str {
        "IAudioOut"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
