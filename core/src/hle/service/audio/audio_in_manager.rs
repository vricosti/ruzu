//! Port of zuyu/src/core/hle/service/audio/audio_in_manager.h and audio_in_manager.cpp
//!
//! IAudioInManager service ("audin:u").

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::core::{AudioInParameterWire, SystemRef};
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::svc_common::PseudoHandle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{
    CmifInArrayBuffer, CmifOutArrayBuffer, CmifRequest, CmifResponse,
};
use crate::hle::service::cmif_types::{buffer_attr, InArray, OutArray};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use crate::hle::kernel::k_process::ProcessLock;

/// IPC command table for IAudioInManager ("audin:u"):
///
/// | Cmd | Name                         |
/// |-----|------------------------------|
/// | 0   | ListAudioIns                 |
/// | 1   | OpenAudioIn                  |
/// | 2   | ListAudioInsAuto             |
/// | 3   | OpenAudioInAuto              |
/// | 4   | ListAudioInsAutoFiltered     |
/// | 5   | OpenAudioInProtocolSpecified |
pub struct IAudioInManager {
    system: SystemRef,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct AudioDeviceNameWire {
    name: [u8; 0x100],
}

impl Default for AudioDeviceNameWire {
    fn default() -> Self {
        Self { name: [0; 0x100] }
    }
}

type Protocol = [u32; 2];

#[derive(Clone, Copy, Default)]
#[repr(C)]
struct AudioInParameterInternalWire {
    sample_rate: u32,
    channel_count: u32,
    sample_format: u32,
    state: u32,
}

impl IAudioInManager {
    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub fn new(system: SystemRef) -> Self {
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
            system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn list_audio_ins_auto_filtered<const A: i32>(
        &self,
        out_audio_ins: &mut OutArray<AudioDeviceNameWire, A>,
        out_count: &mut u32,
    ) -> ResultCode {
        let mut names = vec![[0u8; 0x100]; out_audio_ins.len()];
        *out_count = self
            .system
            .get()
            .audio_core()
            .map(|audio_core| audio_core.list_audio_input_device_name(&mut names, true))
            .unwrap_or(0);
        for (dst, src) in out_audio_ins
            .iter_mut()
            .zip(names.iter())
            .take(*out_count as usize)
        {
            dst.name = *src;
        }
        RESULT_SUCCESS
    }

    fn resolve_process_handle(
        owner_process: &Arc<ProcessLock>,
        handle: u32,
    ) -> Option<*mut KProcess> {
        let mut process_guard = owner_process.lock().unwrap();
        if handle == PseudoHandle::CurrentProcess as u32 || handle == 0 {
            return Some(&mut *process_guard as *mut KProcess);
        }

        process_guard.handle_table.get_object(handle)?;
        Some(&mut *process_guard as *mut KProcess)
    }

    fn open_audio_in_protocol_specified<const AIN: i32, const AOUT: i32>(
        &self,
        ctx: &mut HLERequestContext,
        out_parameter_internal: &mut AudioInParameterInternalWire,
        out_name: &mut OutArray<AudioDeviceNameWire, AOUT>,
        name: &InArray<AudioDeviceNameWire, AIN>,
        protocol: Protocol,
        params: AudioInParameterWire,
    ) -> ResultCode {
        if name.is_empty() || out_name.is_empty() {
            return ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_OPERATION_FAILED.1,
            );
        }
        let mut request = CmifRequest::new(ctx);
        request.align_for::<u64>();
        let applet_resource_user_id = request.u64();

        let Some(owner_process) = ctx.owner_process_arc() else {
            return ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_INVALID_HANDLE.1,
            );
        };
        let process_handle = ctx.get_copy_handle(0);
        let process = Self::resolve_process_handle(&owner_process, process_handle);
        let Some(_process) = process else {
            return ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_INVALID_HANDLE.1,
            );
        };

        let result = self
            .system
            .get()
            .audio_core()
            .ok_or_else(|| {
                ResultCode::from_module_description(
                    crate::hle::result::ErrorModule::Audio,
                    crate::hle::service::audio::errors::RESULT_OPERATION_FAILED.1,
                )
            })
            .and_then(|audio_core| {
                audio_core.open_audio_input(
                    &name[0].name,
                    protocol,
                    params,
                    applet_resource_user_id,
                )
            });

        let Ok(opened) = result else {
            return result.err().unwrap_or_else(|| {
                ResultCode::from_module_description(
                    crate::hle::result::ErrorModule::Audio,
                    crate::hle::service::audio::errors::RESULT_OPERATION_FAILED.1,
                )
            });
        };

        let Some(kernel) = self.system.get().kernel() else {
            return ResultCode::from_module_description(
                crate::hle::result::ErrorModule::Audio,
                crate::hle::service::audio::errors::RESULT_OPERATION_FAILED.1,
            );
        };
        let (
            buffer_event_object_id,
            buffer_readable_event_object_id,
            buffer_event,
            buffer_readable_event,
        ) = super::audio_in::IAudioIn::create_buffer_event(kernel, &owner_process);

        let audio_in = Arc::new(super::audio_in::IAudioIn::new(
            opened.session,
            owner_process,
            buffer_event_object_id,
            buffer_readable_event_object_id,
            buffer_event,
            buffer_readable_event,
        ));

        *out_parameter_internal = AudioInParameterInternalWire {
            sample_rate: opened.sample_rate,
            channel_count: opened.channel_count,
            sample_format: opened.sample_format,
            state: opened.state,
        };
        out_name[0].name = opened.name;

        let mut response = CmifResponse::new(ctx, 6, 0, 1);
        response.push_result(RESULT_SUCCESS);
        response.push_raw(out_parameter_internal);
        response.push_interface(audio_in);
        RESULT_SUCCESS
    }

    fn list_audio_ins_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioInManager::ListAudioIns");
        let mut out_count = 0u32;
        let mut out_storage = CmifOutArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcMapAlias },
        >::from_ctx(ctx, 0);
        let mut out_audio_ins = out_storage.as_out_array();
        let result = svc.list_audio_ins_auto_filtered(&mut out_audio_ins, &mut out_count);
        out_storage.write_back(ctx, 0, out_count as usize);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(result);
        response.push_u32(out_count);
    }

    fn open_audio_in_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioInManager::OpenAudioIn");
        let mut request = CmifRequest::new(ctx);
        let params: AudioInParameterWire = request.raw();
        let mut out_name_storage = CmifOutArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcMapAlias },
        >::from_ctx(ctx, 0);
        let mut out_name = out_name_storage.as_out_array();
        let mut in_name_storage = CmifInArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcMapAlias },
        >::from_ctx(ctx, 0);
        let in_name = in_name_storage.as_in_array();
        let mut out_parameter_internal = AudioInParameterInternalWire::default();
        let result = svc.open_audio_in_protocol_specified(
            ctx,
            &mut out_parameter_internal,
            &mut out_name,
            &in_name,
            [0, 0],
            params,
        );
        out_name_storage.write_back(ctx, 0, 1);
        if result.is_error() {
            CmifResponse::result_only(ctx, result);
        }
    }

    fn list_audio_ins_auto_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioInManager::ListAudioInsAuto");
        let mut out_count = 0u32;
        let mut out_storage = CmifOutArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcAutoSelect },
        >::from_ctx(ctx, 0);
        let mut out_audio_ins = out_storage.as_out_array();
        let result = svc.list_audio_ins_auto_filtered(&mut out_audio_ins, &mut out_count);
        out_storage.write_back(ctx, 0, out_count as usize);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(result);
        response.push_u32(out_count);
    }

    fn open_audio_in_auto_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IAudioInManager::OpenAudioInAuto");
        let mut request = CmifRequest::new(ctx);
        let params: AudioInParameterWire = request.raw();
        let mut out_name_storage = CmifOutArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcAutoSelect },
        >::from_ctx(ctx, 0);
        let mut out_name = out_name_storage.as_out_array();
        let mut in_name_storage = CmifInArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcAutoSelect },
        >::from_ctx(ctx, 0);
        let in_name = in_name_storage.as_in_array();
        let mut out_parameter_internal = AudioInParameterInternalWire::default();
        let result = svc.open_audio_in_protocol_specified(
            ctx,
            &mut out_parameter_internal,
            &mut out_name,
            &in_name,
            [0, 0],
            params,
        );
        out_name_storage.write_back(ctx, 0, 1);
        if result.is_error() {
            CmifResponse::result_only(ctx, result);
        }
    }

    fn list_audio_ins_auto_filtered_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        log::debug!("IAudioInManager::ListAudioInsAutoFiltered");
        let mut out_count = 0u32;
        let mut out_storage = CmifOutArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcAutoSelect },
        >::from_ctx(ctx, 0);
        let mut out_audio_ins = out_storage.as_out_array();
        let result = svc.list_audio_ins_auto_filtered(&mut out_audio_ins, &mut out_count);
        out_storage.write_back(ctx, 0, out_count as usize);
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(result);
        response.push_u32(out_count);
    }

    fn open_audio_in_protocol_specified_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        log::debug!("IAudioInManager::OpenAudioInProtocolSpecified");
        let mut request = CmifRequest::new(ctx);
        let protocol = [request.u32(), request.u32()];
        let params: AudioInParameterWire = request.raw();
        let mut out_name_storage = CmifOutArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcAutoSelect },
        >::from_ctx(ctx, 0);
        let mut out_name = out_name_storage.as_out_array();
        let mut in_name_storage = CmifInArrayBuffer::<
            AudioDeviceNameWire,
            { buffer_attr::BufferAttr_HipcAutoSelect },
        >::from_ctx(ctx, 0);
        let in_name = in_name_storage.as_in_array();
        let mut out_parameter_internal = AudioInParameterInternalWire::default();
        let result = svc.open_audio_in_protocol_specified(
            ctx,
            &mut out_parameter_internal,
            &mut out_name,
            &in_name,
            protocol,
            params,
        );
        out_name_storage.write_back(ctx, 0, 1);
        if result.is_error() {
            CmifResponse::result_only(ctx, result);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn audio_in_manager_registers_upstream_command_ids() {
        let service = IAudioInManager::new(SystemRef::null());

        for cmd in [0_u32, 1, 2, 3, 4, 5] {
            assert!(service.handlers.contains_key(&cmd));
            assert!(service.handlers[&cmd].handler_callback.is_some());
        }
    }

    #[test]
    fn wire_layouts_match_upstream_sizes() {
        assert_eq!(std::mem::size_of::<AudioDeviceNameWire>(), 0x100);
        assert_eq!(std::mem::size_of::<AudioInParameterWire>(), 0x8);
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
