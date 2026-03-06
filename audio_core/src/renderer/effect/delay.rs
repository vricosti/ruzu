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
    pub channel_count_max: u16,
    pub channel_count: u16,
    pub delay_time_max: u32,
    pub delay_time: u32,
    pub sample_rate: i32,
    pub in_gain: i32,
    pub feedback_gain: i32,
    pub wet_gain: i32,
    pub dry_gain: i32,
    pub channel_spread: i32,
    pub lowpass_amount: i32,
    pub state: ParameterState,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion2 {
    pub inputs: [i8; MAX_CHANNELS],
    pub outputs: [i8; MAX_CHANNELS],
    pub channel_count_max: i16,
    pub channel_count: i16,
    pub delay_time_max: i32,
    pub delay_time: i32,
    pub sample_rate: i32,
    pub in_gain: i32,
    pub feedback_gain: i32,
    pub wet_gain: i32,
    pub dry_gain: i32,
    pub channel_spread: i32,
    pub lowpass_amount: i32,
    pub state: ParameterState,
}

pub fn update_v1(
    effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    in_params: &InParameterVersion1,
    pool_mapper: &PoolMapper<'_>,
) {
    let specific = EffectInfoBase::read_specific::<ParameterVersion1>(&in_params.specific);
    if EffectInfoBase::is_channel_count_valid(specific.channel_count_max as i32) {
        let old_state = effect.read_parameter::<ParameterVersion1>().state;
        let mut params = specific;
        if !EffectInfoBase::is_channel_count_valid(specific.channel_count as i32) {
            params.channel_count = params.channel_count_max;
        }
        if !EffectInfoBase::is_channel_count_valid(specific.channel_count as i32)
            || old_state != ParameterState::Updated
        {
            params.state = old_state;
        }
        effect.write_parameter(&params);
        effect.apply_common_settings(
            in_params.is_new,
            in_params.enabled,
            in_params.mix_id as i32,
            in_params.process_order as i32,
        );
        if effect.buffer_unmapped || in_params.is_new {
            effect.set_usage(UsageState::New);
            effect.write_parameter_at(0x34, &ParameterState::Initialized);
            effect.buffer_unmapped = !pool_mapper.try_attach_buffer(
                error_info,
                &mut effect.workbuffers[0],
                in_params.workbuffer,
                in_params.workbuffer_size as u64,
            );
            return;
        }
    }
    EffectInfoBase::set_success(error_info);
}

pub fn update_v2(
    effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    in_params: &InParameterVersion2,
    pool_mapper: &PoolMapper<'_>,
) {
    let specific = EffectInfoBase::read_specific::<ParameterVersion2>(&in_params.specific);
    if EffectInfoBase::is_channel_count_valid(specific.channel_count_max as i32) {
        let old_state = effect.read_parameter::<ParameterVersion2>().state;
        let mut params = specific;
        if !EffectInfoBase::is_channel_count_valid(specific.channel_count as i32) {
            params.channel_count = params.channel_count_max;
        }
        if !EffectInfoBase::is_channel_count_valid(specific.channel_count as i32)
            || old_state != ParameterState::Updated
        {
            params.state = old_state;
        }
        effect.write_parameter(&params);
        effect.apply_common_settings(
            in_params.is_new,
            in_params.enabled,
            in_params.mix_id as i32,
            in_params.process_order as i32,
        );
        if effect.buffer_unmapped || in_params.is_new {
            effect.set_usage(UsageState::New);
            effect.write_parameter_at(0x34, &ParameterState::Initialized);
            effect.buffer_unmapped = !pool_mapper.try_attach_buffer(
                error_info,
                &mut effect.workbuffers[0],
                in_params.workbuffer,
                in_params.workbuffer_size as u64,
            );
            return;
        }
    }
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
