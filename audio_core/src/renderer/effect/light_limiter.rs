use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::behavior::ErrorInfo;
use crate::renderer::memory::PoolMapper;

use super::effect_info_base::{
    EffectInfoBase, InParameterVersion1, InParameterVersion2, ParameterState, UsageState,
};
use super::effect_result_state::EffectResultState;

#[derive(Debug, Clone, Copy, Default)]
#[repr(u8)]
pub enum ProcessingMode {
    #[default]
    Mode0 = 0,
    Mode1 = 1,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion1 {
    pub inputs: [i8; MAX_CHANNELS],
    pub outputs: [i8; MAX_CHANNELS],
    pub channel_count_max: u16,
    pub channel_count: u16,
    pub sample_rate: u32,
    pub look_ahead_time_max: i32,
    pub attack_time: i32,
    pub release_time: i32,
    pub look_ahead_time: i32,
    pub attack_coeff: f32,
    pub release_coeff: f32,
    pub threshold: f32,
    pub input_gain: f32,
    pub output_gain: f32,
    pub look_ahead_samples_min: i32,
    pub look_ahead_samples_max: i32,
    pub state: ParameterState,
    pub statistics_enabled: bool,
    pub statistics_reset_required: bool,
    pub processing_mode: ProcessingMode,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion2 {
    pub inputs: [i8; MAX_CHANNELS],
    pub outputs: [i8; MAX_CHANNELS],
    pub channel_count_max: u16,
    pub channel_count: u16,
    pub sample_rate: u32,
    pub look_ahead_time_max: i32,
    pub attack_time: i32,
    pub release_time: i32,
    pub look_ahead_time: i32,
    pub attack_coeff: f32,
    pub release_coeff: f32,
    pub threshold: f32,
    pub input_gain: f32,
    pub output_gain: f32,
    pub look_ahead_samples_min: i32,
    pub look_ahead_samples_max: i32,
    pub state: ParameterState,
    pub statistics_enabled: bool,
    pub statistics_reset_required: bool,
    pub processing_mode: ProcessingMode,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct StatisticsInternal {
    pub channel_max_sample: [f32; MAX_CHANNELS],
    pub channel_compression_gain_min: [f32; MAX_CHANNELS],
}

pub fn update_v1(
    effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    in_params: &InParameterVersion1,
    pool_mapper: &PoolMapper<'_>,
) {
    let specific = EffectInfoBase::read_specific::<ParameterVersion1>(&in_params.specific);
    effect.write_parameter(&specific);
    effect.apply_common_settings(
        in_params.is_new,
        in_params.enabled,
        in_params.mix_id as i32,
        in_params.process_order as i32,
    );
    if effect.buffer_unmapped || in_params.is_new {
        effect.set_usage(UsageState::New);
        effect.write_parameter_at(0x40, &ParameterState::Initialized);
        effect.buffer_unmapped = !pool_mapper.try_attach_buffer(
            error_info,
            &mut effect.workbuffers[0],
            in_params.workbuffer,
            in_params.workbuffer_size as u64,
        );
    } else {
        EffectInfoBase::set_success(error_info);
    }
}

pub fn update_v2(
    effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    in_params: &InParameterVersion2,
    pool_mapper: &PoolMapper<'_>,
) {
    let specific = EffectInfoBase::read_specific::<ParameterVersion2>(&in_params.specific);
    effect.write_parameter(&specific);
    effect.apply_common_settings(
        in_params.is_new,
        in_params.enabled,
        in_params.mix_id as i32,
        in_params.process_order as i32,
    );
    if effect.buffer_unmapped || in_params.is_new {
        effect.set_usage(UsageState::New);
        effect.write_parameter_at(0x40, &ParameterState::Initialized);
        effect.buffer_unmapped = !pool_mapper.try_attach_buffer(
            error_info,
            &mut effect.workbuffers[0],
            in_params.workbuffer,
            in_params.workbuffer_size as u64,
        );
    } else {
        EffectInfoBase::set_success(error_info);
    }
}

pub fn update_for_command_generation(effect: &mut EffectInfoBase) {
    effect.set_usage(if effect.is_enabled() {
        UsageState::Enabled
    } else {
        UsageState::Disabled
    });
    effect.write_parameter_at(0x40, &ParameterState::Updated);
    effect.write_parameter_at(0x42, &false);
}

pub fn initialize_result_state(result_state: &mut EffectResultState) {
    let stats = StatisticsInternal {
        channel_max_sample: [0.0; MAX_CHANNELS],
        channel_compression_gain_min: [1.0; MAX_CHANNELS],
    };
    result_state.buffer.fill(0);
    unsafe {
        std::ptr::copy_nonoverlapping(
            &stats as *const StatisticsInternal as *const u8,
            result_state.buffer.as_mut_ptr(),
            std::mem::size_of::<StatisticsInternal>(),
        );
    }
}

pub fn update_result_state(cpu_state: &mut EffectResultState, dsp_state: &EffectResultState) {
    cpu_state.buffer.fill(0);
    cpu_state.buffer[..std::mem::size_of::<StatisticsInternal>()]
        .copy_from_slice(&dsp_state.buffer[..std::mem::size_of::<StatisticsInternal>()]);
}

pub fn get_workbuffer(effect: &mut EffectInfoBase, index: i32) -> CpuAddr {
    effect.get_single_buffer(index)
}
