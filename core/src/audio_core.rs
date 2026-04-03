// SPDX-FileCopyrightText: 2026 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Opaque audio-core bridge used by `core` owners that must talk to the
//! frontend-provided audio implementation without depending on `audio_core`.

use std::any::Any;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum ExecutionMode {
    #[default]
    Auto = 0,
    Manual = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(C)]
pub struct AudioRendererParameterInternal {
    pub sample_rate: u32,
    pub sample_count: u32,
    pub mixes: u32,
    pub sub_mixes: u32,
    pub voices: u32,
    pub sinks: u32,
    pub effects: u32,
    pub perf_frames: u32,
    pub voice_drop_enabled: u8,
    pub unk_21: u8,
    pub rendering_device: u8,
    pub execution_mode: ExecutionMode,
    pub splitter_infos: u32,
    pub splitter_destinations: i32,
    pub external_context_size: u32,
    pub revision: u32,
}

const _: () = assert!(core::mem::size_of::<AudioRendererParameterInternal>() == 0x34);

pub trait AudioRendererSessionHandle: Any + Send + Sync {
    fn get_sample_rate(&self) -> u32;
    fn get_sample_count(&self) -> u32;
    fn get_mix_buffer_count(&self) -> u32;
    fn get_state(&self) -> u32;
    fn request_update(&self, input: &[u8], performance: &mut [u8], output: &mut [u8]) -> u32;
    fn start(&self);
    fn stop(&self);
    fn get_rendering_time_limit(&self) -> u32;
    fn set_rendering_time_limit(&self, limit: u32);
    fn get_voice_drop_parameter(&self) -> f32;
    fn set_voice_drop_parameter(&self, voice_drop_parameter: f32);
    fn execution_mode(&self) -> ExecutionMode;
}

pub trait AudioRendererManagerHandle: Any + Send + Sync {
    fn get_work_buffer_size(&self, params: &AudioRendererParameterInternal) -> Result<u64, u32>;

    fn open_audio_renderer(
        &self,
        params: &AudioRendererParameterInternal,
        transfer_memory_size: u64,
        process_handle: *mut crate::hle::kernel::k_process::KProcess,
        applet_resource_user_id: u64,
    ) -> Result<Arc<dyn AudioRendererSessionHandle>, u32>;
}

pub trait AudioCoreInterface: Any + Send {
    fn as_any(&self) -> &(dyn Any + Send);
    fn create_audio_renderer_manager_handle(&self) -> Arc<dyn AudioRendererManagerHandle>;
}
