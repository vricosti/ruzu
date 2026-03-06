use std::ffi::c_void;
use std::ptr::NonNull;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ExecutionMode {
    Auto = 0,
    Manual = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy)]
pub struct AudioRendererSystemContext {
    pub session_id: i32,
    pub channels: i8,
    pub mix_buffer_count: i16,
    pub behavior: Option<NonNull<c_void>>,
    pub depop_buffer: Option<NonNull<i32>>,
    pub upsampler_manager: Option<NonNull<c_void>>,
    pub memory_pool_info: Option<NonNull<c_void>>,
}
