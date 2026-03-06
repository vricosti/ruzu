use crate::common::common::CpuAddr;
use crate::renderer::behavior::ErrorInfo;
use crate::renderer::memory::PoolMapper;

use super::aux_::{AuxBufferInfo, AuxInfoDsp, ParameterVersion1, ParameterVersion2};
use super::effect_info_base::{
    EffectInfoBase, InParameterVersion1, InParameterVersion2, UsageState,
};

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
        effect.buffer_unmapped = !pool_mapper.try_attach_buffer(
            error_info,
            &mut effect.workbuffers[0],
            specific.send_buffer_info_address,
            std::mem::size_of::<AuxBufferInfo>() as u64
                + specific.count_max as u64 * std::mem::size_of::<i32>() as u64,
        );
        if !effect.buffer_unmapped {
            let send = effect.workbuffers[0].get_reference(false);
            effect.send_buffer_info = send + std::mem::size_of::<AuxInfoDsp>();
            effect.send_buffer = send + std::mem::size_of::<AuxBufferInfo>();
            effect.return_buffer_info = 0;
            effect.return_buffer = 0;
            return;
        }
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
        effect.buffer_unmapped = !pool_mapper.try_attach_buffer(
            error_info,
            &mut effect.workbuffers[0],
            specific.send_buffer_info_address,
            std::mem::size_of::<AuxBufferInfo>() as u64
                + specific.count_max as u64 * std::mem::size_of::<i32>() as u64,
        );
        if !effect.buffer_unmapped {
            let send = effect.workbuffers[0].get_reference(false);
            effect.send_buffer_info = send + std::mem::size_of::<AuxInfoDsp>();
            effect.send_buffer = send + std::mem::size_of::<AuxBufferInfo>();
            effect.return_buffer_info = 0;
            effect.return_buffer = 0;
            return;
        }
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
}

pub fn get_workbuffer(effect: &mut EffectInfoBase, index: i32) -> CpuAddr {
    effect.workbuffers[index as usize].get_reference(true)
}
