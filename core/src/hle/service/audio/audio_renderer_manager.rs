//! Port of zuyu/src/core/hle/service/audio/audio_renderer_manager.h and audio_renderer_manager.cpp
//!
//! IAudioRendererManager service ("audren:u").

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::core::SystemRef;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_transfer_memory::KTransferMemory;
use crate::hle::kernel::svc_common::PseudoHandle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
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
    system: SystemRef,
    num_audio_devices: std::sync::atomic::AtomicU32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct AudioRendererParameterBlob {
    bytes: [u8; 0x34],
}

impl Default for AudioRendererParameterBlob {
    fn default() -> Self {
        Self { bytes: [0; 0x34] }
    }
}

impl IAudioRendererManager {
    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub fn new(system: SystemRef) -> Self {
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
        Self {
            system,
            num_audio_devices: std::sync::atomic::AtomicU32::new(0),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of upstream `IAudioRendererManager::OpenAudioRenderer`.
    fn open_audio_renderer_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::info!("IAudioRendererManager::OpenAudioRenderer");
        let mut rp = RequestParser::new(ctx);
        let params: AudioRendererParameterBlob = rp.pop_raw();
        let revision = u32::from_le_bytes([
            params.bytes[0x30],
            params.bytes[0x31],
            params.bytes[0x32],
            params.bytes[0x33],
        ]);
        log::info!(
            "IAudioRendererManager::OpenAudioRenderer params revision=0x{:08X} sample_rate={} sample_count={} mixes={} sub_mixes={} voices={} sinks={} effects={} perf_frames={} exec_mode={} splitter_infos={} splitter_destinations={} external_context_size=0x{:X}",
            revision,
            u32::from_le_bytes([params.bytes[0x00], params.bytes[0x01], params.bytes[0x02], params.bytes[0x03]]),
            u32::from_le_bytes([params.bytes[0x04], params.bytes[0x05], params.bytes[0x06], params.bytes[0x07]]),
            u32::from_le_bytes([params.bytes[0x08], params.bytes[0x09], params.bytes[0x0A], params.bytes[0x0B]]),
            u32::from_le_bytes([params.bytes[0x0C], params.bytes[0x0D], params.bytes[0x0E], params.bytes[0x0F]]),
            u32::from_le_bytes([params.bytes[0x10], params.bytes[0x11], params.bytes[0x12], params.bytes[0x13]]),
            u32::from_le_bytes([params.bytes[0x14], params.bytes[0x15], params.bytes[0x16], params.bytes[0x17]]),
            u32::from_le_bytes([params.bytes[0x18], params.bytes[0x19], params.bytes[0x1A], params.bytes[0x1B]]),
            u32::from_le_bytes([params.bytes[0x1C], params.bytes[0x1D], params.bytes[0x1E], params.bytes[0x1F]]),
            params.bytes[0x23],
            u32::from_le_bytes([params.bytes[0x24], params.bytes[0x25], params.bytes[0x26], params.bytes[0x27]]),
            i32::from_le_bytes([params.bytes[0x28], params.bytes[0x29], params.bytes[0x2A], params.bytes[0x2B]]),
            u32::from_le_bytes([params.bytes[0x2C], params.bytes[0x2D], params.bytes[0x2E], params.bytes[0x2F]])
        );
        rp.align_for::<u64>();
        let transfer_memory_size = rp.pop_u64();
        let applet_resource_user_id = rp.pop_u64();
        let Some(owner_process) = ctx.owner_process_arc() else {
            log::error!("IAudioRendererManager::OpenAudioRenderer missing owner process");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_INVALID_HANDLE.1,
            ));
            return;
        };
        let transfer_memory_handle = ctx.get_copy_handle(0);
        let process_handle = ctx.get_copy_handle(1);
        let transfer_memory =
            Self::resolve_transfer_memory_handle(&owner_process, transfer_memory_handle);
        let process = Self::resolve_process_handle(&owner_process, process_handle);

        let Some(transfer_memory) = transfer_memory else {
            log::error!(
                "IAudioRendererManager::OpenAudioRenderer invalid transfer memory handle {:#x}",
                transfer_memory_handle
            );
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_INVALID_HANDLE.1,
            ));
            return;
        };

        let Some(process) = process else {
            log::error!(
                "IAudioRendererManager::OpenAudioRenderer invalid process handle {:#x}",
                process_handle
            );
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_INVALID_HANDLE.1,
            ));
            return;
        };

        let kernel = svc
            .system
            .get()
            .kernel()
            .expect("OpenAudioRenderer requires KernelCore");
        let (
            rendered_event_object_id,
            rendered_readable_event_object_id,
            rendered_event,
            rendered_readable_event,
        ) = super::audio_renderer::IAudioRenderer::create_rendered_event(kernel, &owner_process);

        let result = svc
            .system
            .get()
            .audio_core()
            .ok_or_else(|| {
                log::error!("IAudioRendererManager::OpenAudioRenderer missing AudioCore owner");
                ResultCode::from_module_description(
                    crate::hle::result::ErrorModule::Audio,
                    crate::hle::service::audio::errors::RESULT_OPERATION_FAILED.1,
                )
            })
            .and_then(|audio_core| {
                audio_core.open_audio_renderer(
                    &params.bytes,
                    transfer_memory,
                    transfer_memory_size,
                    process,
                    applet_resource_user_id,
                    Arc::clone(&rendered_event),
                )
            });

        let Ok(renderer_impl) = result else {
            let err = result.err().unwrap_or_else(|| {
                ResultCode::from_module_description(
                    crate::hle::result::ErrorModule::Audio,
                    crate::hle::service::audio::errors::RESULT_OPERATION_FAILED.1,
                )
            });
            log::error!(
                "IAudioRendererManager::OpenAudioRenderer failed result=0x{:08X}",
                err.get_inner_value()
            );
            {
                let mut owner = owner_process.lock().unwrap();
                owner.unregister_readable_event_object_by_object_id(
                    rendered_readable_event_object_id,
                );
                owner.unregister_event_object_by_object_id(rendered_event_object_id);
            }
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(err);
            return;
        };

        log::info!("IAudioRendererManager::OpenAudioRenderer succeeded");

        let renderer = Arc::new(super::audio_renderer::IAudioRenderer::new(
            renderer_impl,
            owner_process,
            rendered_event_object_id,
            rendered_readable_event_object_id,
            rendered_event,
            rendered_readable_event,
        ));
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(renderer);
    }

    fn resolve_transfer_memory_handle(
        owner_process: &Arc<std::sync::Mutex<KProcess>>,
        handle: u32,
    ) -> Option<*mut KTransferMemory> {
        let process_guard = owner_process.lock().unwrap();
        let object_id = process_guard.handle_table.get_object(handle)?;
        let transfer_memory = process_guard.get_transfer_memory_by_object_id(object_id)?;
        let mut transfer_memory_guard = transfer_memory.lock().unwrap();
        Some(&mut *transfer_memory_guard as *mut KTransferMemory)
    }

    fn resolve_process_handle(
        owner_process: &Arc<std::sync::Mutex<KProcess>>,
        handle: u32,
    ) -> Option<*mut KProcess> {
        let mut process_guard = owner_process.lock().unwrap();
        if handle == PseudoHandle::CurrentProcess as u32 || handle == 0 {
            return Some(&mut *process_guard as *mut KProcess);
        }

        process_guard.handle_table.get_object(handle)?;
        Some(&mut *process_guard as *mut KProcess)
    }

    fn get_work_buffer_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        log::debug!("IAudioRendererManager::GetWorkBufferSize");
        let mut rp = RequestParser::new(ctx);
        let params: AudioRendererParameterBlob = rp.pop_raw();
        let size = svc
            .system
            .get()
            .audio_core()
            .and_then(|audio_core| audio_core.get_audio_renderer_work_buffer_size(&params.bytes))
            .unwrap_or(0x4000);
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(size);
    }

    /// Port of upstream `IAudioRendererManager::GetAudioDeviceService`.
    /// Upstream creates `IAudioDevice(system, aruid, REV1_magic, num_audio_devices++)`.
    fn get_audio_device_service_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IAudioRendererManager::GetAudioDeviceService");
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let applet_resource_user_id = rp.pop_u64();
        let device_num = svc
            .num_audio_devices
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        log::info!(
            "IAudioRendererManager::GetAudioDeviceService aruid={} revision=0x52455631 device_num={}",
            applet_resource_user_id,
            device_num
        );
        let device = Arc::new(super::audio_device::IAudioDevice::new(
            svc.system,
            applet_resource_user_id,
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
        let mut rp = RequestParser::new(ctx);
        let revision = rp.pop_u32();
        rp.align_for::<u64>();
        let applet_resource_user_id = rp.pop_u64();
        let device_num = svc
            .num_audio_devices
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        log::info!(
            "IAudioRendererManager::GetAudioDeviceServiceWithRevisionInfo aruid={} revision=0x{:08X} device_num={}",
            applet_resource_user_id,
            revision,
            device_num
        );
        let device = Arc::new(super::audio_device::IAudioDevice::new(
            svc.system,
            applet_resource_user_id,
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
