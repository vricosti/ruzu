use crate::common::common::{FINAL_MIX_ID, UNUSED_MIX_ID, UNUSED_SPLITTER_ID};
use crate::common::feature_support::check_valid_revision;
use crate::errors::RESULT_INVALID_UPDATE_INFO;
use crate::renderer::behavior::behavior_info::{
    BehaviorInfo, ErrorInfo, InParameter as BehaviorInParameter, OutStatus as BehaviorOutStatus,
};
use crate::renderer::effect::effect_info_base::{
    InParameterVersion1 as EffectInParameterVersion1,
    InParameterVersion2 as EffectInParameterVersion2, OutStatusVersion1 as EffectOutStatusVersion1,
    OutStatusVersion2 as EffectOutStatusVersion2,
};
use crate::renderer::effect::EffectContext;
use crate::renderer::memory::{
    MemoryPoolInParameter, MemoryPoolInfo, MemoryPoolOutStatus, PoolMapper,
};
use crate::renderer::mix::mix_info::{
    InDirtyParameter as MixInDirtyParameter, InParameter as MixInParameter,
};
use crate::renderer::mix::MixContext;
use crate::renderer::performance::performance_manager::{
    InParameter as PerfInParameter, OutStatus as PerfOutStatus,
};
use crate::renderer::performance::PerformanceManager;
use crate::renderer::sink::{SinkContext, SinkInParameter, SinkOutStatus};
use crate::renderer::splitter::SplitterContext;
use crate::renderer::upsampler::UpsamplerManager;
use crate::renderer::voice::voice_info::{
    InParameter as VoiceInParameter, OutStatus as VoiceOutStatus,
};
use crate::renderer::voice::VoiceContext;
use crate::Result;
use common::ResultCode;
use std::mem::size_of;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct UpdateDataHeader {
    pub revision: u32,
    pub behaviour_size: u32,
    pub memory_pool_size: u32,
    pub voices_size: u32,
    pub voice_resources_size: u32,
    pub effects_size: u32,
    pub mix_size: u32,
    pub sinks_size: u32,
    pub performance_buffer_size: u32,
    pub unk24: [u8; 4],
    pub render_info_size: u32,
    pub unk2c: [u8; 0x10],
    pub size: u32,
}

pub struct InfoUpdater<'a> {
    input_origin: &'a [u8],
    output_origin: &'a mut [u8],
    input_offset: usize,
    output_offset: usize,
    in_header: UpdateDataHeader,
    out_header: UpdateDataHeader,
    expected_input_size: u64,
    expected_output_size: u64,
    behavior: *mut BehaviorInfo,
}

impl<'a> InfoUpdater<'a> {
    pub fn new(input: &'a [u8], output: &'a mut [u8], behavior: &'a mut BehaviorInfo) -> Self {
        let header_size = size_of::<UpdateDataHeader>();
        let output_len = output.len();
        let in_header = read_pod::<UpdateDataHeader>(input, 0).unwrap_or_default();
        let out_header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            size: header_size as u32,
            ..Default::default()
        };
        if output.len() >= header_size {
            write_pod(&out_header, &mut output[..header_size]);
        }

        Self {
            input_origin: input,
            output_origin: output,
            input_offset: header_size.min(input.len()),
            output_offset: header_size.min(output_len),
            in_header,
            out_header,
            expected_input_size: input.len() as u64,
            expected_output_size: output_len as u64,
            behavior: behavior as *mut BehaviorInfo,
        }
    }

    pub fn update_behavior_info(&mut self) -> Result {
        let Some(in_params) = self.read_input::<BehaviorInParameter>() else {
            return RESULT_INVALID_UPDATE_INFO;
        };
        if !check_valid_revision(in_params.revision) {
            return RESULT_INVALID_UPDATE_INFO;
        }
        if in_params.revision != self.behavior().get_user_revision() {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.behavior_mut().clear_error();
        self.behavior_mut().update_flags(in_params.flags);
        if self.in_header.behaviour_size != size_of::<BehaviorInParameter>() as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.input_offset += size_of::<BehaviorInParameter>();
        ResultCode::SUCCESS
    }

    pub fn update_memory_pools(
        &mut self,
        memory_pools: &mut [MemoryPoolInfo],
        memory_pool_count: u32,
    ) -> Result {
        let input_size = memory_pool_count as usize * size_of::<MemoryPoolInParameter>();
        let output_size = memory_pool_count as usize * size_of::<MemoryPoolOutStatus>();
        if self.input_offset + input_size > self.input_origin.len()
            || self.output_offset + output_size > self.output_origin.len()
        {
            return RESULT_INVALID_UPDATE_INFO;
        }

        let in_params = unsafe {
            std::slice::from_raw_parts(
                self.input_origin[self.input_offset..].as_ptr() as *const MemoryPoolInParameter,
                memory_pool_count as usize,
            )
        };
        let mapper = PoolMapper::new(None, self.behavior().is_memory_force_mapping_enabled());
        for (index, in_param) in in_params.iter().enumerate() {
            let mut out_param = MemoryPoolOutStatus::default();
            let state = mapper.update(&mut memory_pools[index], in_param, &mut out_param);
            if !matches!(
                state,
                crate::renderer::memory::MemoryPoolResultState::Success
                    | crate::renderer::memory::MemoryPoolResultState::BadParam
                    | crate::renderer::memory::MemoryPoolResultState::MapFailed
                    | crate::renderer::memory::MemoryPoolResultState::InUse
            ) {
                return RESULT_INVALID_UPDATE_INFO;
            }
            if self
                .write_output_at(index * size_of::<MemoryPoolOutStatus>(), &out_param)
                .is_error()
            {
                return RESULT_INVALID_UPDATE_INFO;
            }
        }

        if self.in_header.memory_pool_size != input_size as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.out_header.memory_pool_size = output_size as u32;
        self.out_header.size = self.out_header.size.saturating_add(output_size as u32);
        self.input_offset += input_size;
        self.output_offset += output_size;
        ResultCode::SUCCESS
    }

    pub fn update_voice_channel_resources(&mut self, voice_context: &mut VoiceContext) -> Result {
        let voice_count = voice_context.get_count();
        let input_size = voice_count as usize
            * size_of::<crate::renderer::voice::voice_channel_resource::InParameter>();
        if self.input_offset + input_size > self.input_origin.len() {
            return RESULT_INVALID_UPDATE_INFO;
        }
        let in_params = unsafe {
            std::slice::from_raw_parts(
                self.input_origin[self.input_offset..].as_ptr()
                    as *const crate::renderer::voice::voice_channel_resource::InParameter,
                voice_count as usize,
            )
        };
        for (index, in_param) in in_params.iter().enumerate() {
            if let Some(resource) = voice_context.get_channel_resource(index as u32) {
                resource.in_use = in_param.in_use;
                if in_param.in_use {
                    resource.mix_volumes = in_param.mix_volumes;
                }
            }
        }
        if self.in_header.voice_resources_size != input_size as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.input_offset += input_size;
        ResultCode::SUCCESS
    }

    pub fn update_voices(
        &mut self,
        voice_context: &mut VoiceContext,
        memory_pools: &mut [MemoryPoolInfo],
        memory_pool_count: u32,
    ) -> Result {
        let voice_count = voice_context.get_count();
        let input_size = voice_count as usize * size_of::<VoiceInParameter>();
        let output_size = voice_count as usize * size_of::<VoiceOutStatus>();
        if self.input_offset + input_size > self.input_origin.len()
            || self.output_offset + output_size > self.output_origin.len()
        {
            return RESULT_INVALID_UPDATE_INFO;
        }
        let in_params = unsafe {
            std::slice::from_raw_parts(
                self.input_origin[self.input_offset..].as_ptr() as *const VoiceInParameter,
                voice_count as usize,
            )
        };
        for index in 0..voice_count {
            if let Some(info) = voice_context.get_info_mut(index) {
                info.in_use = false;
            }
        }

        let mapper = PoolMapper::with_pools(
            None,
            memory_pools,
            memory_pool_count,
            self.behavior().is_memory_force_mapping_enabled(),
        );
        let mut new_voice_count = 0u32;
        for (index, in_param) in in_params.iter().enumerate() {
            if !in_param.in_use {
                continue;
            }
            if in_param.id >= voice_count {
                return RESULT_INVALID_UPDATE_INFO;
            }
            let channel_count = in_param
                .channel_count
                .min(crate::common::common::MAX_CHANNELS as u32);
            let mut voice_states = Vec::with_capacity(channel_count as usize);
            for channel in 0..channel_count as usize {
                let state = voice_context
                    .get_state(in_param.channel_resource_ids[channel])
                    .copied();
                let Some(state) = state else {
                    return RESULT_INVALID_UPDATE_INFO;
                };
                voice_states.push(state);
            }
            let Some(voice_info) = voice_context.get_info_mut(in_param.id) else {
                return RESULT_INVALID_UPDATE_INFO;
            };
            if in_param.is_new {
                voice_info.initialize();
                for state in &mut voice_states {
                    *state = Default::default();
                }
            }

            let mut update_error = ErrorInfo::default();
            voice_info.update_parameters(&mut update_error, in_param, &mapper, self.behavior());
            if update_error.error_code.is_error() {
                self.behavior_mut().append_error(update_error);
            }

            let mut wavebuffer_errors =
                [[ErrorInfo::default(); 2]; crate::common::common::MAX_WAVE_BUFFERS as usize];
            voice_info.update_wave_buffers(
                &mut wavebuffer_errors,
                in_param,
                &mut voice_states,
                &mapper,
                self.behavior(),
            );
            for errors in wavebuffer_errors {
                for error in errors {
                    if error.error_code.is_error() {
                        self.behavior_mut().append_error(error);
                    }
                }
            }

            let mut out_status = VoiceOutStatus::default();
            voice_info.write_out_status(&mut out_status, in_param, &voice_states);
            if self
                .write_output_at(index * size_of::<VoiceOutStatus>(), &out_status)
                .is_error()
            {
                return RESULT_INVALID_UPDATE_INFO;
            }

            for (channel, state) in voice_states.into_iter().enumerate() {
                let Some(target) =
                    voice_context.get_state_mut(in_param.channel_resource_ids[channel])
                else {
                    return RESULT_INVALID_UPDATE_INFO;
                };
                *target = state;
            }
            new_voice_count = new_voice_count.saturating_add(channel_count);
        }

        if self.in_header.voices_size != input_size as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.out_header.voices_size = output_size as u32;
        self.out_header.size = self.out_header.size.saturating_add(output_size as u32);
        self.input_offset += input_size;
        self.output_offset += output_size;
        voice_context.set_active_count(new_voice_count);
        ResultCode::SUCCESS
    }

    pub fn update_effects(
        &mut self,
        effect_context: &mut EffectContext,
        renderer_active: bool,
        memory_pools: &mut [MemoryPoolInfo],
        memory_pool_count: u32,
    ) -> Result {
        if self.behavior().is_effect_info_version2_supported() {
            self.update_effects_version2(effect_context, renderer_active, memory_pools, memory_pool_count)
        } else {
            self.update_effects_version1(effect_context, renderer_active, memory_pools, memory_pool_count)
        }
    }

    fn update_effects_version1(
        &mut self,
        effect_context: &mut EffectContext,
        renderer_active: bool,
        memory_pools: &mut [MemoryPoolInfo],
        memory_pool_count: u32,
    ) -> Result {
        let effect_count = effect_context.get_count();
        let input_size = effect_count as usize * size_of::<EffectInParameterVersion1>();
        let output_size = effect_count as usize * size_of::<EffectOutStatusVersion1>();
        if self.input_offset + input_size > self.input_origin.len()
            || self.output_offset + output_size > self.output_origin.len()
        {
            return RESULT_INVALID_UPDATE_INFO;
        }
        let in_params = unsafe {
            std::slice::from_raw_parts(
                self.input_origin[self.input_offset..].as_ptr()
                    as *const EffectInParameterVersion1,
                effect_count as usize,
            )
        };
        let mapper = PoolMapper::with_pools(
            None,
            memory_pools,
            memory_pool_count,
            self.behavior().is_memory_force_mapping_enabled(),
        );
        for (index, in_param) in in_params.iter().enumerate() {
            let mut error_info = ErrorInfo::default();
            if let Some(effect) = effect_context.get_info_mut(index as u32) {
                effect.update_v1(&mut error_info, in_param, &mapper);
                if error_info.error_code.is_error() {
                    self.behavior_mut().append_error(error_info);
                }
            }
            let mut out = EffectOutStatusVersion1::default();
            if let Some(effect) = effect_context.get_info(index as u32) {
                effect.store_status_v1(&mut out, renderer_active);
            }
            if self
                .write_output_at(index * size_of::<EffectOutStatusVersion1>(), &out)
                .is_error()
            {
                return RESULT_INVALID_UPDATE_INFO;
            }
        }
        if self.in_header.effects_size != input_size as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.out_header.effects_size = output_size as u32;
        self.out_header.size = self.out_header.size.saturating_add(output_size as u32);
        self.input_offset += input_size;
        self.output_offset += output_size;
        ResultCode::SUCCESS
    }

    fn update_effects_version2(
        &mut self,
        effect_context: &mut EffectContext,
        renderer_active: bool,
        memory_pools: &mut [MemoryPoolInfo],
        memory_pool_count: u32,
    ) -> Result {
        let effect_count = effect_context.get_count();
        let input_size = effect_count as usize * size_of::<EffectInParameterVersion2>();
        let output_size = effect_count as usize * size_of::<EffectOutStatusVersion2>();
        if self.input_offset + input_size > self.input_origin.len()
            || self.output_offset + output_size > self.output_origin.len()
        {
            return RESULT_INVALID_UPDATE_INFO;
        }
        let in_params = unsafe {
            std::slice::from_raw_parts(
                self.input_origin[self.input_offset..].as_ptr()
                    as *const EffectInParameterVersion2,
                effect_count as usize,
            )
        };
        let mapper = PoolMapper::with_pools(
            None,
            memory_pools,
            memory_pool_count,
            self.behavior().is_memory_force_mapping_enabled(),
        );
        for (index, in_param) in in_params.iter().enumerate() {
            let mut error_info = ErrorInfo::default();
            let is_new = in_param.is_new;
            if let Some(effect) = effect_context.get_info_mut(index as u32) {
                effect.update_v2(&mut error_info, in_param, &mapper);
                if error_info.error_code.is_error() {
                    self.behavior_mut().append_error(error_info);
                }
            }
            let mut out = EffectOutStatusVersion2::default();
            if let Some(effect) = effect_context.get_info(index as u32) {
                effect.store_status_v2(&mut out, renderer_active, Default::default());
            }
            if is_new {
                let effect = effect_context.get_info(index as u32).cloned();
                if let Some(ref effect) = effect {
                    if let Some(result_state) =
                        effect_context.get_dsp_shared_result_state_mut(index as u32)
                    {
                        effect.initialize_result_state(result_state);
                    }
                    if let Some(result_state) =
                        effect_context.get_result_state_mut(index as u32)
                    {
                        effect.initialize_result_state(result_state);
                    }
                }
            }
            if let Some(effect) = effect_context.get_info(index as u32) {
                let default_result_state = Default::default();
                let source_result_state = effect_context
                    .get_result_state(index as u32)
                    .unwrap_or(&default_result_state);
                effect.update_result_state(&mut out.result_state, source_result_state);
            }
            if self
                .write_output_at(index * size_of::<EffectOutStatusVersion2>(), &out)
                .is_error()
            {
                return RESULT_INVALID_UPDATE_INFO;
            }
        }
        if self.in_header.effects_size != input_size as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.out_header.effects_size = output_size as u32;
        self.out_header.size = self.out_header.size.saturating_add(output_size as u32);
        self.input_offset += input_size;
        self.output_offset += output_size;
        ResultCode::SUCCESS
    }

    pub fn update_mixes(
        &mut self,
        mix_context: &mut MixContext,
        mix_buffer_count: u32,
        effect_context: &EffectContext,
        splitter_context: &mut SplitterContext,
    ) -> Result {
        let mut mix_count = mix_context.get_count();
        let consumed_input_size;
        if self
            .behavior()
            .is_mix_in_parameter_dirty_only_update_supported()
        {
            let Some(dirty) = self.read_input::<MixInDirtyParameter>() else {
                return RESULT_INVALID_UPDATE_INFO;
            };
            mix_count = dirty.count;
            if mix_count < 0 {
                return RESULT_INVALID_UPDATE_INFO;
            }
            self.input_offset += size_of::<MixInDirtyParameter>();
            consumed_input_size =
                size_of::<MixInDirtyParameter>() + mix_count as usize * size_of::<MixInParameter>();
        } else {
            if mix_count < 0 {
                return RESULT_INVALID_UPDATE_INFO;
            }
            consumed_input_size = mix_count as usize * size_of::<MixInParameter>();
        }
        if mix_buffer_count == 0 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        if self.input_offset + mix_count as usize * size_of::<MixInParameter>()
            > self.input_origin.len()
        {
            return RESULT_INVALID_UPDATE_INFO;
        }
        let in_params = unsafe {
            std::slice::from_raw_parts(
                self.input_origin[self.input_offset..].as_ptr() as *const MixInParameter,
                mix_count as usize,
            )
        };
        let mut total_buffer_count = 0u32;
        for params in in_params {
            if params.in_use {
                if params.buffer_count > i16::MAX as u32 {
                    return RESULT_INVALID_UPDATE_INFO;
                }
                total_buffer_count = total_buffer_count.saturating_add(params.buffer_count);
                if params.dest_mix_id > mix_context.get_count()
                    && params.dest_mix_id != UNUSED_MIX_ID
                    && params.mix_id != FINAL_MIX_ID
                {
                    return RESULT_INVALID_UPDATE_INFO;
                }
                if params.dest_mix_id == UNUSED_MIX_ID
                    && params.dest_splitter_id != UNUSED_SPLITTER_ID
                    && (params.dest_splitter_id < 0
                        || params.dest_splitter_id as u32 >= splitter_context.get_info_count())
                {
                    return RESULT_INVALID_UPDATE_INFO;
                }
            }
        }
        if total_buffer_count > mix_buffer_count {
            return RESULT_INVALID_UPDATE_INFO;
        }
        let mut mix_dirty = false;
        for (i, params) in in_params.iter().enumerate() {
            let mix_id = if self
                .behavior()
                .is_mix_in_parameter_dirty_only_update_supported()
            {
                params.mix_id
            } else {
                i as i32
            };
            if mix_id < 0 || mix_id >= mix_context.get_count() {
                return RESULT_INVALID_UPDATE_INFO;
            }
            if let Some(mix) = mix_context.get_info_mut(mix_id) {
                if mix.in_use != params.in_use {
                    mix.in_use = params.in_use;
                    if !params.in_use {
                        mix.clear_effect_processing_order();
                    }
                    mix_dirty = true;
                }
            }
            if params.in_use {
                mix_dirty |= mix_context.update_mix(
                    mix_id,
                    params,
                    effect_context,
                    splitter_context,
                    self.behavior(),
                );
            }
        }
        if mix_dirty {
            if self.behavior().is_splitter_supported() && splitter_context.using_splitter() {
                if !mix_context.tsort_info(splitter_context) {
                    return RESULT_INVALID_UPDATE_INFO;
                }
            } else {
                mix_context.sort_info();
            }
        }
        if self.in_header.mix_size != consumed_input_size as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.input_offset += mix_count as usize * size_of::<MixInParameter>();
        ResultCode::SUCCESS
    }

    pub fn update_sinks(
        &mut self,
        sink_context: &mut SinkContext,
        memory_pools: &mut [MemoryPoolInfo],
        memory_pool_count: u32,
        upsampler_manager: &mut UpsamplerManager,
    ) -> Result {
        let sink_count = sink_context.get_count();
        let input_size = sink_count as usize * size_of::<SinkInParameter>();
        let output_size = sink_count as usize * size_of::<SinkOutStatus>();
        if self.input_offset + input_size > self.input_origin.len()
            || self.output_offset + output_size > self.output_origin.len()
        {
            return RESULT_INVALID_UPDATE_INFO;
        }
        let in_params = unsafe {
            std::slice::from_raw_parts(
                self.input_origin[self.input_offset..].as_ptr() as *const SinkInParameter,
                sink_count as usize,
            )
        };
        let mapper = PoolMapper::with_pools(
            None,
            memory_pools,
            memory_pool_count,
            self.behavior().is_memory_force_mapping_enabled(),
        );
        for (index, in_param) in in_params.iter().enumerate() {
            let mut out_status = SinkOutStatus::default();
            let mut error_info = ErrorInfo::default();
            if let Some(sink) = sink_context.get_info(index as u32) {
                if sink.get_type() != in_param.sink_type {
                    sink.clean_up_with_upsampler(upsampler_manager);
                    sink.reset_type(in_param.sink_type);
                }
                sink.update(&mut error_info, &mut out_status, in_param, &mapper);
                if error_info.error_code.is_error() {
                    self.behavior_mut().append_error(error_info);
                }
            }
            if self
                .write_output_at(index * size_of::<SinkOutStatus>(), &out_status)
                .is_error()
            {
                return RESULT_INVALID_UPDATE_INFO;
            }
        }
        if self.in_header.sinks_size != input_size as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.out_header.sinks_size = output_size as u32;
        self.out_header.size = self.out_header.size.saturating_add(output_size as u32);
        self.input_offset += input_size;
        self.output_offset += output_size;
        ResultCode::SUCCESS
    }

    pub fn update_splitter_info(&mut self, splitter_context: &mut SplitterContext) -> Result {
        let mut consumed_size = 0u32;
        if !splitter_context.update(&self.input_origin[self.input_offset..], &mut consumed_size) {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.input_offset += consumed_size as usize;
        ResultCode::SUCCESS
    }

    pub fn update_performance_buffer(
        &mut self,
        performance_output: &mut [u8],
        performance_manager: Option<&mut PerformanceManager>,
    ) -> Result {
        let Some(in_params) = self.read_input::<PerfInParameter>() else {
            return RESULT_INVALID_UPDATE_INFO;
        };
        let mut out_params = PerfOutStatus::default();
        if let Some(manager) = performance_manager {
            out_params.history_size = manager.copy_histories(performance_output) as i32;
            manager.set_detail_target(in_params.target_node_id as u32);
        }
        let out_slice = match self.output_slice_mut(size_of::<PerfOutStatus>()) {
            Ok(slice) => slice,
            Err(error) => return error,
        };
        if write_pod_to_slice(&out_params, out_slice).is_error() {
            return RESULT_INVALID_UPDATE_INFO;
        }
        if self.in_header.performance_buffer_size != size_of::<PerfInParameter>() as u32 {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.out_header.performance_buffer_size = size_of::<PerfOutStatus>() as u32;
        self.out_header.size = self
            .out_header
            .size
            .saturating_add(size_of::<PerfOutStatus>() as u32);
        self.input_offset += size_of::<PerfInParameter>();
        self.output_offset += size_of::<PerfOutStatus>();
        ResultCode::SUCCESS
    }

    pub fn update_error_info(&mut self) -> Result {
        let mut out = BehaviorOutStatus::default();
        self.behavior()
            .copy_error_info(&mut out.errors, &mut out.error_count);
        let out_slice = match self.output_slice_mut(size_of::<BehaviorOutStatus>()) {
            Ok(slice) => slice,
            Err(error) => return error,
        };
        if write_pod_to_slice(&out, out_slice).is_error() {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.out_header.behaviour_size = size_of::<BehaviorOutStatus>() as u32;
        self.out_header.size = self
            .out_header
            .size
            .saturating_add(size_of::<BehaviorOutStatus>() as u32);
        self.output_offset += size_of::<BehaviorOutStatus>();
        ResultCode::SUCCESS
    }

    pub fn update_renderer_info(&mut self, elapsed_frames: u64) -> Result {
        #[derive(Clone, Copy)]
        #[repr(C)]
        struct RenderInfo {
            frames_elapsed: u64,
            unk08: [u8; 0x8],
        }
        let out = RenderInfo {
            frames_elapsed: elapsed_frames,
            unk08: [0; 0x8],
        };
        let out_slice = match self.output_slice_mut(size_of::<RenderInfo>()) {
            Ok(slice) => slice,
            Err(error) => return error,
        };
        if write_pod_to_slice(&out, out_slice).is_error() {
            return RESULT_INVALID_UPDATE_INFO;
        }
        self.out_header.render_info_size = size_of::<RenderInfo>() as u32;
        self.out_header.size = self
            .out_header
            .size
            .saturating_add(size_of::<RenderInfo>() as u32);
        self.output_offset += size_of::<RenderInfo>();
        ResultCode::SUCCESS
    }

    pub fn check_consumed_size(&self) -> Result {
        if self.input_offset as u64 != self.expected_input_size
            || self.output_offset as u64 != self.expected_output_size
        {
            return RESULT_INVALID_UPDATE_INFO;
        }
        ResultCode::SUCCESS
    }

    fn read_input<T: Copy>(&self) -> Option<T> {
        read_pod(self.input_origin, self.input_offset)
    }

    fn behavior(&self) -> &BehaviorInfo {
        unsafe { &*self.behavior }
    }

    fn behavior_mut(&mut self) -> &mut BehaviorInfo {
        unsafe { &mut *self.behavior }
    }

    fn output_slice_mut(&mut self, size: usize) -> std::result::Result<&mut [u8], Result> {
        let end = self.output_offset.saturating_add(size);
        self.output_origin
            .get_mut(self.output_offset..end)
            .ok_or(RESULT_INVALID_UPDATE_INFO)
    }

    fn write_output_at<T: Copy>(&mut self, relative_offset: usize, value: &T) -> Result {
        let start = self.output_offset.saturating_add(relative_offset);
        let end = start.saturating_add(size_of::<T>());
        let Some(slice) = self.output_origin.get_mut(start..end) else {
            return RESULT_INVALID_UPDATE_INFO;
        };
        write_pod_to_slice(value, slice)
    }
}

impl Drop for InfoUpdater<'_> {
    fn drop(&mut self) {
        if self.output_origin.len() >= size_of::<UpdateDataHeader>() {
            let _ = write_pod_to_slice(
                &self.out_header,
                &mut self.output_origin[..size_of::<UpdateDataHeader>()],
            );
        }
    }
}

fn read_pod<T: Copy>(input: &[u8], offset: usize) -> Option<T> {
    let end = offset.checked_add(size_of::<T>())?;
    let bytes = input.get(offset..end)?;
    Some(unsafe { (bytes.as_ptr() as *const T).read_unaligned() })
}

fn write_pod<T: Copy>(value: &T, output: &mut [u8]) {
    let _ = write_pod_to_slice(value, output);
}

fn write_pod_to_slice<T: Copy>(value: &T, output: &mut [u8]) -> Result {
    if output.len() < size_of::<T>() {
        return RESULT_INVALID_UPDATE_INFO;
    }
    unsafe {
        std::ptr::copy_nonoverlapping(
            value as *const T as *const u8,
            output.as_mut_ptr(),
            size_of::<T>(),
        );
    }
    ResultCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::audio_renderer_parameter::{AudioRendererParameterInternal, ExecutionMode};
    use crate::common::feature_support::CURRENT_REVISION;
    use crate::renderer::behavior::BehaviorInfo;
    use crate::renderer::effect::effect_info_base::{EffectType, OutStatusVersion2};
    use crate::renderer::effect::light_limiter;

    fn push_pod<T: Copy>(out: &mut Vec<u8>, value: &T) {
        let bytes =
            unsafe { std::slice::from_raw_parts(value as *const T as *const u8, size_of::<T>()) };
        out.extend_from_slice(bytes);
    }

    #[test]
    fn update_mixes_rejects_invalid_destination_mix() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(1);
        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            mix_size: (2 * size_of::<MixInParameter>()) as u32,
            size: (size_of::<UpdateDataHeader>() + 2 * size_of::<MixInParameter>()) as u32,
            ..Default::default()
        };
        let invalid_mix = MixInParameter {
            in_use: true,
            mix_id: 1,
            dest_mix_id: 99,
            buffer_count: 1,
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &MixInParameter::default());
        push_pod(&mut input, &invalid_mix);

        let mut mix_context = MixContext::new();
        mix_context.initialize(2, 0, &behavior);
        let mut splitter_context = SplitterContext::new();
        let _ = splitter_context.initialize(
            &behavior,
            &AudioRendererParameterInternal {
                sample_rate: 48_000,
                sample_count: 160,
                mixes: 2,
                sub_mixes: 0,
                voices: 0,
                sinks: 0,
                effects: 0,
                perf_frames: 0,
                voice_drop_enabled: 0,
                unk_21: 0,
                rendering_device: 0,
                execution_mode: ExecutionMode::Manual,
                splitter_infos: 0,
                splitter_destinations: 0,
                external_context_size: 0,
                revision: 1,
            },
        );
        let effect_context = EffectContext::new();
        let mut output = vec![0u8; size_of::<UpdateDataHeader>()];
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);

        assert_eq!(
            updater.update_mixes(&mut mix_context, 2, &effect_context, &mut splitter_context),
            RESULT_INVALID_UPDATE_INFO
        );
    }

    #[test]
    fn update_mixes_rejects_invalid_dirty_mix_id() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);

        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            mix_size: (size_of::<MixInDirtyParameter>() + size_of::<MixInParameter>()) as u32,
            size: (size_of::<UpdateDataHeader>()
                + size_of::<MixInDirtyParameter>()
                + size_of::<MixInParameter>()) as u32,
            ..Default::default()
        };
        let dirty = MixInDirtyParameter {
            count: 1,
            ..Default::default()
        };
        let params = MixInParameter {
            mix_id: -1,
            in_use: true,
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &dirty);
        push_pod(&mut input, &params);

        let mut mix_context = MixContext::new();
        mix_context.initialize(1, 0, &behavior);
        let effect_context = EffectContext::new();
        let mut splitter_context = SplitterContext::new();
        let mut output = vec![0u8; size_of::<UpdateDataHeader>()];
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);

        assert_eq!(
            updater.update_mixes(&mut mix_context, 4, &effect_context, &mut splitter_context),
            RESULT_INVALID_UPDATE_INFO
        );
    }

    #[test]
    fn update_voice_channel_resources_preserves_previous_mix_volumes() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(1);

        let params = crate::renderer::voice::voice_channel_resource::InParameter {
            id: 0,
            mix_volumes: [0.25; crate::common::common::MAX_MIX_BUFFERS as usize],
            in_use: true,
            ..Default::default()
        };
        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            voice_resources_size: size_of::<
                crate::renderer::voice::voice_channel_resource::InParameter,
            >() as u32,
            size: (size_of::<UpdateDataHeader>()
                + size_of::<crate::renderer::voice::voice_channel_resource::InParameter>())
                as u32,
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &params);

        let mut voice_context = VoiceContext::new();
        voice_context.initialize(1, 1, 0);
        if let Some(resource) = voice_context.get_channel_resource(0) {
            resource.mix_volumes = [0.5; crate::common::common::MAX_MIX_BUFFERS as usize];
            resource.prev_mix_volumes = [0.75; crate::common::common::MAX_MIX_BUFFERS as usize];
            resource.in_use = true;
        }

        let mut output = vec![0u8; size_of::<UpdateDataHeader>()];
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);

        assert_eq!(
            updater.update_voice_channel_resources(&mut voice_context),
            ResultCode::SUCCESS
        );

        let resource = voice_context.get_channel_resource_ref(0).unwrap();
        assert_eq!(
            resource.prev_mix_volumes,
            [0.75; crate::common::common::MAX_MIX_BUFFERS as usize]
        );
        assert_eq!(
            resource.mix_volumes,
            [0.25; crate::common::common::MAX_MIX_BUFFERS as usize]
        );
    }

    #[test]
    fn update_effects_v2_uses_effect_specific_result_state_translation() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);
        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            effects_size: size_of::<EffectInParameterVersion2>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<EffectInParameterVersion2>()) as u32,
            ..Default::default()
        };
        let in_params = EffectInParameterVersion2 {
            type_: EffectType::LightLimiter,
            enabled: true,
            is_new: false,
            specific: {
                let mut specific = [0u8; 0xA0];
                let parameter = light_limiter::ParameterVersion2 {
                    channel_count_max: 2,
                    channel_count: 2,
                    statistics_enabled: true,
                    ..Default::default()
                };
                let bytes = unsafe {
                    std::slice::from_raw_parts(
                        &parameter as *const light_limiter::ParameterVersion2 as *const u8,
                        size_of::<light_limiter::ParameterVersion2>(),
                    )
                };
                specific[..bytes.len()].copy_from_slice(bytes);
                specific
            },
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &in_params);

        let mut output =
            vec![0u8; size_of::<UpdateDataHeader>() + size_of::<EffectOutStatusVersion2>()];
        let mut effect_context = EffectContext::new();
        effect_context.initialize(1, 1);
        if let Some(effect) = effect_context.get_info_mut(0) {
            effect.set_type(EffectType::LightLimiter);
        }
        if let Some(result_state) = effect_context.get_result_state_mut(0) {
            result_state.buffer.fill(0xAB);
            let stats = light_limiter::StatisticsInternal {
                channel_max_sample: [1.0; crate::common::common::MAX_CHANNELS],
                channel_compression_gain_min: [0.5; crate::common::common::MAX_CHANNELS],
            };
            let stats_bytes = unsafe {
                std::slice::from_raw_parts(
                    &stats as *const light_limiter::StatisticsInternal as *const u8,
                    size_of::<light_limiter::StatisticsInternal>(),
                )
            };
            result_state.buffer[..stats_bytes.len()].copy_from_slice(stats_bytes);
        }

        {
            let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);
            assert_eq!(
                updater.update_effects(&mut effect_context, false, &mut [], 0),
                ResultCode::SUCCESS
            );
        }

        let out = read_pod::<OutStatusVersion2>(&output, size_of::<UpdateDataHeader>()).unwrap();
        let stats_size = size_of::<light_limiter::StatisticsInternal>();
        assert!(out.result_state.buffer[stats_size..]
            .iter()
            .all(|byte| *byte == 0));
    }

    #[test]
    fn update_effects_v2_initializes_new_effect_result_state_before_output_translation() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);
        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            effects_size: size_of::<EffectInParameterVersion2>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<EffectInParameterVersion2>()) as u32,
            ..Default::default()
        };
        let in_params = EffectInParameterVersion2 {
            type_: EffectType::LightLimiter,
            enabled: true,
            is_new: true,
            specific: {
                let mut specific = [0u8; 0xA0];
                let parameter = light_limiter::ParameterVersion2 {
                    channel_count_max: 2,
                    channel_count: 2,
                    statistics_enabled: true,
                    ..Default::default()
                };
                let bytes = unsafe {
                    std::slice::from_raw_parts(
                        &parameter as *const light_limiter::ParameterVersion2 as *const u8,
                        size_of::<light_limiter::ParameterVersion2>(),
                    )
                };
                specific[..bytes.len()].copy_from_slice(bytes);
                specific
            },
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &in_params);

        let mut output =
            vec![0u8; size_of::<UpdateDataHeader>() + size_of::<EffectOutStatusVersion2>()];
        let mut effect_context = EffectContext::new();
        effect_context.initialize(1, 1);
        if let Some(result_state) = effect_context.get_result_state_mut(0) {
            result_state.buffer.fill(0xCD);
        }

        {
            let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);
            assert_eq!(
                updater.update_effects(&mut effect_context, false, &mut [], 0),
                ResultCode::SUCCESS
            );
        }

        let out = read_pod::<OutStatusVersion2>(&output, size_of::<UpdateDataHeader>()).unwrap();
        let stats = unsafe {
            &*(out.result_state.buffer.as_ptr() as *const light_limiter::StatisticsInternal)
        };
        assert_eq!(
            stats.channel_compression_gain_min,
            [1.0; crate::common::common::MAX_CHANNELS]
        );
        assert!(
            out.result_state.buffer[size_of::<light_limiter::StatisticsInternal>()..]
                .iter()
                .all(|byte| *byte == 0)
        );
    }

    #[test]
    fn update_performance_buffer_preserves_negative_detail_target_bits() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(1);

        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            performance_buffer_size: size_of::<PerfInParameter>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<PerfInParameter>()) as u32,
            ..Default::default()
        };
        let perf_in = PerfInParameter {
            target_node_id: -1,
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &perf_in);

        let mut output = vec![0u8; size_of::<UpdateDataHeader>() + size_of::<PerfOutStatus>()];
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);
        let mut performance_manager = PerformanceManager::new();

        assert_eq!(
            updater.update_performance_buffer(&mut [], Some(&mut performance_manager)),
            ResultCode::SUCCESS
        );
        assert!(performance_manager.is_detail_target(u32::MAX));
    }

    #[test]
    fn update_sinks_preserves_new_sink_type_across_disabled_type_switch() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);

        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            sinks_size: size_of::<SinkInParameter>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<SinkInParameter>()) as u32,
            ..Default::default()
        };
        let sink_in = SinkInParameter {
            sink_type: crate::renderer::sink::SinkType::DeviceSink,
            in_use: false,
            node_id: 7,
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &sink_in);

        let mut output = vec![0u8; size_of::<UpdateDataHeader>() + size_of::<SinkOutStatus>()];
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);
        let mut sink_context = SinkContext::default();
        sink_context.initialize(vec![Default::default()], 1);
        let mut upsampler_manager = UpsamplerManager::new(1);

        assert_eq!(
            updater.update_sinks(&mut sink_context, &mut [], 0, &mut upsampler_manager),
            ResultCode::SUCCESS
        );

        let sink = sink_context.get_info(0).unwrap();
        assert_eq!(sink.get_type(), crate::renderer::sink::SinkType::DeviceSink);
        assert!(!sink.is_used());
    }

    #[test]
    fn update_mixes_rejects_negative_dirty_mix_count() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);

        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            mix_size: size_of::<MixInDirtyParameter>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<MixInDirtyParameter>()) as u32,
            ..Default::default()
        };
        let dirty = MixInDirtyParameter {
            magic: 0,
            count: -1,
            _unk08: [0; 0x18],
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &dirty);

        let mut output = vec![0u8; size_of::<UpdateDataHeader>()];
        let mut mix_context = MixContext::new();
        mix_context.initialize(1, 1, &behavior);
        let effect_context = EffectContext::new();
        let mut splitter_context = SplitterContext::new();
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);

        assert_eq!(
            updater.update_mixes(&mut mix_context, 1, &effect_context, &mut splitter_context),
            RESULT_INVALID_UPDATE_INFO
        );
    }

    #[test]
    fn update_mixes_rejects_invalid_destination_splitter() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);

        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            mix_size: size_of::<MixInParameter>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<MixInParameter>()) as u32,
            ..Default::default()
        };
        let mix = MixInParameter {
            in_use: true,
            mix_id: 0,
            buffer_count: 1,
            dest_mix_id: UNUSED_MIX_ID,
            dest_splitter_id: 3,
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &mix);

        let mut output = vec![0u8; size_of::<UpdateDataHeader>()];
        let mut mix_context = MixContext::new();
        mix_context.initialize(1, 0, &behavior);
        let effect_context = EffectContext::new();
        let mut splitter_context = SplitterContext::new();
        splitter_context.initialize(
            &behavior,
            &crate::common::audio_renderer_parameter::AudioRendererParameterInternal {
                sample_rate: 48_000,
                sample_count: 240,
                mixes: 1,
                sub_mixes: 0,
                voices: 0,
                sinks: 0,
                effects: 0,
                perf_frames: 0,
                voice_drop_enabled: 0,
                unk_21: 0,
                rendering_device: 0,
                execution_mode: crate::common::audio_renderer_parameter::ExecutionMode::Auto,
                splitter_infos: 1,
                splitter_destinations: 1,
                external_context_size: 0,
                revision: behavior.get_process_revision(),
            },
        );
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);

        assert_eq!(
            updater.update_mixes(&mut mix_context, 1, &effect_context, &mut splitter_context),
            RESULT_INVALID_UPDATE_INFO
        );
    }

    #[test]
    fn update_mixes_rejects_buffer_count_that_does_not_fit_internal_i16() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);

        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            mix_size: size_of::<MixInParameter>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<MixInParameter>()) as u32,
            ..Default::default()
        };
        let mix = MixInParameter {
            in_use: true,
            mix_id: 0,
            buffer_count: i16::MAX as u32 + 1,
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &mix);

        let mut output = vec![0u8; size_of::<UpdateDataHeader>()];
        let mut mix_context = MixContext::new();
        mix_context.initialize(1, 0, &behavior);
        let effect_context = EffectContext::new();
        let mut splitter_context = SplitterContext::new();
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);

        assert_eq!(
            updater.update_mixes(
                &mut mix_context,
                u32::MAX,
                &effect_context,
                &mut splitter_context
            ),
            RESULT_INVALID_UPDATE_INFO
        );
    }

    #[test]
    fn update_voices_rejects_invalid_voice_id() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);

        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            voices_size: size_of::<VoiceInParameter>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<VoiceInParameter>()) as u32,
            ..Default::default()
        };
        let voice = VoiceInParameter {
            in_use: true,
            id: 1,
            channel_count: 1,
            ..Default::default()
        };
        push_pod(&mut input, &header);
        push_pod(&mut input, &voice);

        let mut output = vec![0u8; size_of::<UpdateDataHeader>() + size_of::<VoiceOutStatus>()];
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);
        let mut voice_context = VoiceContext::new();
        voice_context.initialize(1, 1, 1);

        assert_eq!(
            updater.update_voices(&mut voice_context, &mut [], 0),
            RESULT_INVALID_UPDATE_INFO
        );
    }

    #[test]
    fn update_voices_rejects_invalid_channel_resource_id() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);

        let mut input = Vec::new();
        let header = UpdateDataHeader {
            revision: behavior.get_process_revision(),
            voices_size: size_of::<VoiceInParameter>() as u32,
            size: (size_of::<UpdateDataHeader>() + size_of::<VoiceInParameter>()) as u32,
            ..Default::default()
        };
        let mut voice = VoiceInParameter {
            in_use: true,
            id: 0,
            channel_count: 1,
            ..Default::default()
        };
        voice.channel_resource_ids[0] = 1;
        push_pod(&mut input, &header);
        push_pod(&mut input, &voice);

        let mut output = vec![0u8; size_of::<UpdateDataHeader>() + size_of::<VoiceOutStatus>()];
        let mut updater = InfoUpdater::new(&input, &mut output, &mut behavior);
        let mut voice_context = VoiceContext::new();
        voice_context.initialize(1, 1, 1);

        assert_eq!(
            updater.update_voices(&mut voice_context, &mut [], 0),
            RESULT_INVALID_UPDATE_INFO
        );
    }
}
