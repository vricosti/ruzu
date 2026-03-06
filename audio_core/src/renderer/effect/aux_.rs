use crate::common::common::{CpuAddr, MAX_MIX_BUFFERS};
use crate::renderer::behavior::ErrorInfo;
use crate::renderer::memory::PoolMapper;

use super::effect_info_base::{
    EffectInfoBase, InParameterVersion1, InParameterVersion2, UsageState,
};

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion1 {
    pub inputs: [i8; MAX_MIX_BUFFERS as usize],
    pub outputs: [i8; MAX_MIX_BUFFERS as usize],
    pub mix_buffer_count: u32,
    pub sample_rate: u32,
    pub count_max: u32,
    pub mix_buffer_count_max: u32,
    pub send_buffer_info_address: CpuAddr,
    pub send_buffer_address: CpuAddr,
    pub return_buffer_info_address: CpuAddr,
    pub return_buffer_address: CpuAddr,
    pub mix_buffer_sample_size: u32,
    pub sample_count: u32,
    pub mix_buffer_sample_count: u32,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion2 {
    pub inputs: [i8; MAX_MIX_BUFFERS as usize],
    pub outputs: [i8; MAX_MIX_BUFFERS as usize],
    pub mix_buffer_count: u32,
    pub sample_rate: u32,
    pub count_max: u32,
    pub mix_buffer_count_max: u32,
    pub send_buffer_info_address: CpuAddr,
    pub send_buffer_address: CpuAddr,
    pub return_buffer_info_address: CpuAddr,
    pub return_buffer_address: CpuAddr,
    pub mix_buffer_sample_size: u32,
    pub sample_count: u32,
    pub mix_buffer_sample_count: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct AuxInfoDsp {
    pub read_offset: u32,
    pub write_offset: u32,
    pub lost_sample_count: u32,
    pub total_sample_count: u32,
    pub unk10: [u8; 0x30],
}

impl Default for AuxInfoDsp {
    fn default() -> Self {
        Self {
            read_offset: 0,
            write_offset: 0,
            lost_sample_count: 0,
            total_sample_count: 0,
            unk10: [0; 0x30],
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AuxBufferInfo {
    pub cpu_info: AuxInfoDsp,
    pub dsp_info: AuxInfoDsp,
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
        let send_unmapped = !pool_mapper.try_attach_buffer(
            error_info,
            &mut effect.workbuffers[0],
            specific.send_buffer_info_address,
            std::mem::size_of::<AuxBufferInfo>() as u64
                + specific.count_max as u64 * std::mem::size_of::<i32>() as u64,
        );
        let return_unmapped = !pool_mapper.try_attach_buffer(
            error_info,
            &mut effect.workbuffers[1],
            specific.return_buffer_info_address,
            std::mem::size_of::<AuxBufferInfo>() as u64
                + specific.count_max as u64 * std::mem::size_of::<i32>() as u64,
        );
        effect.buffer_unmapped = send_unmapped || return_unmapped;
        if !effect.buffer_unmapped {
            let send = effect.workbuffers[0].get_reference(false);
            effect.send_buffer_info = send + std::mem::size_of::<AuxInfoDsp>();
            effect.send_buffer = send + std::mem::size_of::<AuxBufferInfo>();

            let ret = effect.workbuffers[1].get_reference(false);
            effect.return_buffer_info = ret + std::mem::size_of::<AuxInfoDsp>();
            effect.return_buffer = ret + std::mem::size_of::<AuxBufferInfo>();
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
        let send_unmapped = !pool_mapper.try_attach_buffer(
            error_info,
            &mut effect.workbuffers[0],
            specific.send_buffer_info_address,
            std::mem::size_of::<AuxBufferInfo>() as u64
                + specific.count_max as u64 * std::mem::size_of::<i32>() as u64,
        );
        let return_unmapped = !pool_mapper.try_attach_buffer(
            error_info,
            &mut effect.workbuffers[1],
            specific.return_buffer_info_address,
            std::mem::size_of::<AuxBufferInfo>() as u64
                + specific.count_max as u64 * std::mem::size_of::<i32>() as u64,
        );
        effect.buffer_unmapped = send_unmapped || return_unmapped;
        if !effect.buffer_unmapped {
            let send = effect.workbuffers[0].get_reference(false);
            effect.send_buffer_info = send + std::mem::size_of::<AuxInfoDsp>();
            effect.send_buffer = send + std::mem::size_of::<AuxBufferInfo>();

            let ret = effect.workbuffers[1].get_reference(false);
            effect.return_buffer_info = ret + std::mem::size_of::<AuxInfoDsp>();
            effect.return_buffer = ret + std::mem::size_of::<AuxBufferInfo>();
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
