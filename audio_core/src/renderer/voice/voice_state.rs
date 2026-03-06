use crate::common::common::{MAX_BIQUAD_FILTERS, MAX_MIX_BUFFERS, MAX_WAVE_BUFFERS};
use common::fixed_point::FixedPoint;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct BiquadFilterState {
    pub s0: i64,
    pub s1: i64,
    pub s2: i64,
    pub s3: i64,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AdpcmContext {
    pub header: u16,
    pub yn0: i16,
    pub yn1: i16,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct VoiceState {
    pub played_sample_count: u64,
    pub offset: u32,
    pub wave_buffer_index: u32,
    pub wave_buffer_valid: [bool; MAX_WAVE_BUFFERS as usize],
    pub wave_buffers_consumed: u32,
    pub sample_history: [i16; (MAX_WAVE_BUFFERS as usize) * 2],
    pub fraction: FixedPoint<49, 15>,
    pub adpcm_context: AdpcmContext,
    pub biquad_states:
        [[BiquadFilterState; MAX_BIQUAD_FILTERS as usize]; MAX_BIQUAD_FILTERS as usize],
    pub previous_samples: [i32; MAX_MIX_BUFFERS as usize],
    pub external_context_size: u32,
    pub external_context_enabled: bool,
    pub voice_dropped: bool,
    pub loop_count: i32,
}
