use crate::common::common::MAX_CHANNELS;
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
    pub b: [i16; 3],
    pub a: [i16; 2],
    pub channel_count: i8,
    pub state: ParameterState,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion2 {
    pub inputs: [i8; MAX_CHANNELS],
    pub outputs: [i8; MAX_CHANNELS],
    pub b: [i16; 3],
    pub a: [i16; 2],
    pub channel_count: i8,
    pub state: ParameterState,
}

pub fn update_v1(
    effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    in_params: &InParameterVersion1,
    _pool_mapper: &PoolMapper<'_>,
) {
    let specific = EffectInfoBase::read_specific::<ParameterVersion1>(&in_params.specific);
    effect.write_parameter(&specific);
    effect.apply_common_settings(
        in_params.is_new,
        in_params.enabled,
        in_params.mix_id as i32,
        in_params.process_order as i32,
    );
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
    effect.write_parameter_at(0x17, &ParameterState::Updated);
}
