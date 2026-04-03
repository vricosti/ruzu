//! Port of zuyu/src/core/hle/service/audio/audio_renderer_manager.h and audio_renderer_manager.cpp
//!
//! IAudioRendererManager service ("audren:u").

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::audio_core::AudioRendererParameterInternal;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::kernel::k_process::KProcess;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAudioRendererManager ("audren:u"):
///
/// | Cmd | Name                                       |
/// |-----|--------------------------------------------|
/// | 0   | OpenAudioRenderer                          |
/// | 1   | GetWorkBufferSize                          |
/// | 2   | GetAudioDeviceService                      |
/// | 3   | OpenAudioRendererForManualExecution         |
/// | 4   | GetAudioDeviceServiceWithRevisionInfo       |
pub struct IAudioRendererManager {
    _system: crate::core::SystemRef,
    audio_renderer_manager: Arc<dyn crate::audio_core::AudioRendererManagerHandle>,
    num_audio_devices: std::sync::atomic::AtomicU32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAudioRendererManager {
    fn resolve_process_handle(
        ctx: &HLERequestContext,
        process_handle: crate::hle::service::hle_ipc::Handle,
    ) -> *mut KProcess {
        if process_handle == 0 {
            return std::ptr::null_mut();
        }

        let Some(thread) = ctx.get_thread() else {
            return std::ptr::null_mut();
        };
        let process_arc = {
            let thread_guard = thread.lock().unwrap();
            thread_guard.parent.as_ref().and_then(|parent| parent.upgrade())
        };
        let Some(process_arc) = process_arc else {
            return std::ptr::null_mut();
        };

        let mut process = process_arc.lock().unwrap();
        &mut *process as *mut KProcess
    }

    pub fn new(system: crate::core::SystemRef) -> Self {
        let handlers = build_handler_map(&[
            (
                0,
                Some(Self::open_audio_renderer_handler),
                "OpenAudioRenderer",
            ),
            (
                1,
                Some(Self::get_work_buffer_size_handler),
                "GetWorkBufferSize",
            ),
            (
                2,
                Some(Self::get_audio_device_service_handler),
                "GetAudioDeviceService",
            ),
            (3, None, "OpenAudioRendererForManualExecution"),
            (
                4,
                Some(Self::get_audio_device_service_with_revision_info_handler),
                "GetAudioDeviceServiceWithRevisionInfo",
            ),
        ]);
        let audio_renderer_manager = system
            .get()
            .audio_core()
            .expect("AudioCore must be installed before audren:u starts")
            .create_audio_renderer_manager_handle();
        Self {
            _system: system,
            audio_renderer_manager,
            num_audio_devices: std::sync::atomic::AtomicU32::new(0),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of upstream `IAudioRendererManager::OpenAudioRenderer`.
    ///
    /// Upstream creates a new `IAudioRenderer` with a real renderer backend.
    /// We create a stub IAudioRenderer that provides event handles so the game
    /// doesn't block on missing kernel events.
    fn open_audio_renderer_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let params = rp.pop_raw::<AudioRendererParameterInternal>();
        rp.align_for::<u64>();
        let transfer_memory_size = rp.pop_u64();
        let _raw_applet_resource_user_id = rp.pop_u64();
        let applet_resource_user_id = ctx.get_pid();
        let transfer_memory_handle = ctx.get_copy_handle(0);
        let process_handle = ctx.get_copy_handle(1);

        log::info!(
            "IAudioRendererManager::OpenAudioRenderer sample_rate={} sample_count={} mixes={} tmem_size={:#x} tmem_handle={:#x} process_handle={:#x} aruid={:#x}",
            params.sample_rate,
            params.sample_count,
            params.mixes,
            transfer_memory_size,
            transfer_memory_handle,
            process_handle,
            applet_resource_user_id
        );

        let process_handle_ptr = Self::resolve_process_handle(ctx, process_handle);

        match service.audio_renderer_manager.open_audio_renderer(
            &params,
            transfer_memory_size,
            process_handle_ptr,
            applet_resource_user_id,
        ) {
            Ok(renderer_impl) => {
                let renderer = Arc::new(super::audio_renderer::IAudioRenderer::new(renderer_impl));
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
                rb.push_result(RESULT_SUCCESS);
                rb.push_ipc_interface(renderer);
            }
            Err(raw) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(crate::hle::result::ResultCode::new(raw));
            }
        }
    }

    fn get_work_buffer_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let params = rp.pop_raw::<AudioRendererParameterInternal>();
        match service.audio_renderer_manager.get_work_buffer_size(&params) {
            Ok(size) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(size);
            }
            Err(raw) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(crate::hle::result::ResultCode::new(raw));
            }
        }
    }

    /// Port of upstream `IAudioRendererManager::GetAudioDeviceService`.
    /// Upstream creates `IAudioDevice(system, aruid, REV1_magic, num_audio_devices++)`.
    fn get_audio_device_service_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioRendererManager::GetAudioDeviceService");
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let device_num = svc
            .num_audio_devices
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let device = Arc::new(super::audio_device::IAudioDevice::new(
            ctx.get_pid(),
            0x52455631,
            device_num,
        )); // 'REV1'
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(device);
    }

    /// Port of upstream `IAudioRendererManager::GetAudioDeviceServiceWithRevisionInfo`.
    /// Upstream creates `IAudioDevice(system, aruid, revision, num_audio_devices++)`.
    fn get_audio_device_service_with_revision_info_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::info!("IAudioRendererManager::GetAudioDeviceServiceWithRevisionInfo");
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let device_num = svc
            .num_audio_devices
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let revision = RequestParser::new(ctx).pop_u32();
        let device = Arc::new(super::audio_device::IAudioDevice::new(
            ctx.get_pid(),
            revision,
            device_num,
        ));
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(device);
    }
}

impl SessionRequestHandler for IAudioRendererManager {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAudioRendererManager {
    fn get_service_name(&self) -> &str {
        "audren:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
