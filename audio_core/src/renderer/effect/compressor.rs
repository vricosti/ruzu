use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::behavior::ErrorInfo;
use crate::renderer::memory::PoolMapper;

use super::effect_info_base::{
    EffectInfoBase, InParameterVersion1, InParameterVersion2, ParameterState, UsageState,
};

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion1 {
    pub inputs: [i8; MAX_CHANNELS],
    pub outputs: [i8; MAX_CHANNELS],
    pub channel_count_max: i16,
    pub channel_count: i16,
    pub sample_rate: i32,
    pub threshold: f32,
    pub compressor_ratio: f32,
    pub attack_time: i32,
    pub release_time: i32,
    pub unk_24: f32,
    pub unk_28: f32,
    pub unk_2c: f32,
    pub out_gain: f32,
    pub state: ParameterState,
    pub makeup_gain_enabled: bool,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion2 {
    pub inputs: [i8; MAX_CHANNELS],
    pub outputs: [i8; MAX_CHANNELS],
    pub channel_count_max: i16,
    pub channel_count: i16,
    pub sample_rate: i32,
    pub threshold: f32,
    pub compressor_ratio: f32,
    pub attack_time: i32,
    pub release_time: i32,
    pub unk_24: f32,
    pub unk_28: f32,
    pub unk_2c: f32,
    pub out_gain: f32,
    pub state: ParameterState,
    pub makeup_gain_enabled: bool,
}

pub fn update_v1(
    _effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    _in_params: &InParameterVersion1,
    _pool_mapper: &PoolMapper<'_>,
) {
    EffectInfoBase::set_success(error_info);
}

pub fn update_v2(
    effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    in_params: &InParameterVersion2,
    _pool_mapper: &PoolMapper<'_>,
) {
    let specific = EffectInfoBase::read_specific::<ParameterVersion2>(&in_params.specific);
    effect.write_parameter(&specific);
    effect.apply_common_settings(
        in_params.is_new,
        in_params.enabled,
        in_params.mix_id as i32,
        in_params.process_order as i32,
    );
    EffectInfoBase::set_success(error_info);
}

pub fn update_for_command_generation(effect: &mut EffectInfoBase) {
    effect.set_usage(if effect.is_enabled() {
        UsageState::Enabled
    } else {
        UsageState::Disabled
    });
    effect.write_parameter_at(0x34, &ParameterState::Updated);
}

pub fn get_workbuffer(effect: &mut EffectInfoBase, index: i32) -> CpuAddr {
    effect.get_single_buffer(index)
}
