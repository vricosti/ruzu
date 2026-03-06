use crate::common::audio_renderer_parameter::AudioRendererParameterInternal;
use crate::common::common::{
    is_channel_count_valid, use_old_channel_mapping, CpuAddr, SampleFormat, FINAL_MIX_ID,
    HIGHEST_VOICE_PRIORITY, MAX_BIQUAD_FILTERS, MAX_CHANNELS, MAX_MIX_BUFFERS, TARGET_SAMPLE_RATE,
    UNUSED_MIX_ID, UNUSED_SPLITTER_ID,
};
use crate::renderer::behavior::BehaviorInfo;
use crate::renderer::effect::biquad_filter;
use crate::renderer::effect::effect_info_base::EffectType;
use crate::renderer::effect::EffectContext;
use crate::renderer::effect::{
    aux_, buffer_mixer, compressor, delay, i3dl2, light_limiter, reverb,
};
use crate::renderer::memory::MemoryPoolInfo;
use crate::renderer::mix::{MixContext, MixInfo};
use crate::renderer::performance::{
    DetailAspect, EntryAspect, PerformanceDetailType, PerformanceEntryAddresses,
    PerformanceEntryType, PerformanceManager, PerformanceState,
};
use crate::renderer::sink::{SinkContext, SinkType};
use crate::renderer::splitter::SplitterContext;
use crate::renderer::upsampler::UpsamplerManager;
use crate::renderer::voice::voice_state::BiquadFilterState;
use crate::renderer::voice::VoiceContext;
use common::fixed_point::FixedPoint;
use std::mem::size_of_val;

use super::command_buffer::CommandBuffer;
use super::command_list_header::CommandListHeader;
use super::commands::{
    AuxCommand, BiquadFilterCommand, CaptureCommand, CircularBufferSinkCommand, Command,
    CompressorCommand, CopyMixBufferCommand, DataSourceCommand, DelayCommand,
    DepopForMixBuffersCommand, DepopPrepareCommand, DeviceSinkCommand, DownMix6chTo2chCommand,
    I3dl2ReverbCommand, LightLimiterVersion1Command, LightLimiterVersion2Command, MixCommand,
    MixRampCommand, MixRampGroupedCommand, MultiTapBiquadFilterCommand, PerformanceCommand,
    ReverbCommand, UpsampleCommand, VolumeCommand, VolumeRampCommand,
};

pub struct CommandGenerator<'a> {
    command_buffer: &'a mut CommandBuffer,
    command_list_header: &'a mut CommandListHeader,
    behavior: &'a BehaviorInfo,
    voice_context: &'a mut VoiceContext,
    mix_context: &'a mut MixContext,
    effect_context: &'a mut EffectContext,
    sink_context: &'a mut SinkContext,
    splitter_context: &'a mut SplitterContext,
    performance_manager: Option<&'a mut PerformanceManager>,
    voice_state_pool: &'a MemoryPoolInfo,
    effect_state_pool: &'a MemoryPoolInfo,
    effect_result_state_pool: &'a MemoryPoolInfo,
    upsampler_manager: &'a mut UpsamplerManager,
    session_id: i32,
    render_channels: i8,
    depop_buffer: &'a [i32],
    depop_buffer_pool: &'a MemoryPoolInfo,
}

impl<'a> CommandGenerator<'a> {
    pub fn new(
        command_buffer: &'a mut CommandBuffer,
        command_list_header: &'a mut CommandListHeader,
        behavior: &'a BehaviorInfo,
        voice_context: &'a mut VoiceContext,
        mix_context: &'a mut MixContext,
        effect_context: &'a mut EffectContext,
        sink_context: &'a mut SinkContext,
        splitter_context: &'a mut SplitterContext,
        performance_manager: Option<&'a mut PerformanceManager>,
        voice_state_pool: &'a MemoryPoolInfo,
        effect_state_pool: &'a MemoryPoolInfo,
        effect_result_state_pool: &'a MemoryPoolInfo,
        upsampler_manager: &'a mut UpsamplerManager,
        session_id: i32,
        render_channels: i8,
        depop_buffer: &'a [i32],
        depop_buffer_pool: &'a MemoryPoolInfo,
    ) -> Self {
        Self {
            command_buffer,
            command_list_header,
            behavior,
            voice_context,
            mix_context,
            effect_context,
            sink_context,
            splitter_context,
            performance_manager,
            voice_state_pool,
            effect_state_pool,
            effect_result_state_pool,
            upsampler_manager,
            session_id,
            render_channels,
            depop_buffer,
            depop_buffer_pool,
        }
    }

    pub fn calculate_command_buffer_size(
        behavior: &BehaviorInfo,
        params: &AudioRendererParameterInternal,
    ) -> u64 {
        let effect_size = params.effects as u64 * 0x100;
        let voice_source_size = if behavior.is_wave_buffer_ver2_supported() {
            0x180u64
        } else {
            0x160u64
        };
        let voice_size = voice_source_size
            + MAX_BIQUAD_FILTERS as u64 * 0x40
            + 0x20
            + 0x20 * MAX_MIX_BUFFERS as u64;

        let submix_size = 0x40 + (0x20 * MAX_MIX_BUFFERS as u64 * MAX_MIX_BUFFERS as u64);
        let final_mix_size = 0x40 + 0x20 * MAX_MIX_BUFFERS as u64;
        let splitter_size =
            params.splitter_destinations.max(0) as u64 * 0x20 * MAX_MIX_BUFFERS as u64;
        let sink_size = params.sinks as u64
            * (std::mem::size_of::<crate::renderer::command::commands::DeviceSinkPayload>()
                + std::mem::size_of::<crate::renderer::command::commands::CircularBufferSinkPayload>(
                )
                + std::mem::size_of::<crate::renderer::command::commands::UpsamplePayload>()
                + std::mem::size_of::<crate::renderer::command::commands::DownMix6chTo2chPayload>())
                as u64;
        let perf_size =
            (params.effects + params.voices + params.sinks + params.sub_mixes + 1) as u64 * 0x20;

        effect_size
            + params.voices as u64 * voice_size
            + params.sub_mixes as u64 * submix_size
            + final_mix_size
            + splitter_size
            + sink_size
            + perf_size
    }

    pub fn generate(&mut self, mix_buffer_count: u32) {
        let _ = self
            .command_buffer
            .generate_clear_mix_command(u32::MAX, mix_buffer_count);
        self.generate_voice_commands();
        self.generate_submix_commands();
        self.generate_final_mix_commands();
        self.generate_sink_commands();
        self.command_list_header.command_count = self.command_buffer.count();
    }

    pub fn generate_voice_commands(&mut self) {
        self.voice_context.sort_info();
        let sorted_indices = self.voice_context.sorted_voice_indices().to_vec();

        for voice_index in sorted_indices {
            let Some(voice) = self
                .voice_context
                .update_info_for_command_generation(voice_index)
            else {
                continue;
            };

            if !voice.in_use || voice.should_skip() {
                continue;
            }

            let node_id = voice.node_id;
            let entry_aspect =
                self.begin_performance_entry(node_id as i32, PerformanceEntryType::Voice);
            let data_detail_type = match voice.sample_format {
                SampleFormat::PcmInt16 => PerformanceDetailType::Unk1,
                SampleFormat::PcmFloat => PerformanceDetailType::Unk10,
                SampleFormat::Adpcm => PerformanceDetailType::Unk2,
                SampleFormat::Invalid
                | SampleFormat::PcmInt8
                | SampleFormat::PcmInt24
                | SampleFormat::PcmInt32 => PerformanceDetailType::Invalid,
            };
            let data_detail = self.begin_performance_detail(
                node_id as i32,
                PerformanceEntryType::Voice,
                data_detail_type,
            );
            for channel in 0..voice.channel_count.max(0) as usize {
                let Some(resource_id) = voice.channel_resource_ids.get(channel).copied() else {
                    continue;
                };
                let Some((depop_commands, data_command, voice_state)) = self
                    .voice_context
                    .get_dsp_shared_state_ref(resource_id)
                    .and_then(|voice_state_ref| {
                        let data_command = self.build_data_source_command(
                            &voice,
                            voice_state_ref,
                            channel as i8,
                            self.command_list_header.buffer_count,
                        )?;
                        let depop_commands =
                            self.build_voice_depop_prepare_commands(&voice, voice_state_ref);
                        Some((depop_commands, data_command, *voice_state_ref))
                    })
                else {
                    continue;
                };
                for (command, enabled) in depop_commands {
                    let _ = self.command_buffer.push_with_enabled(
                        Command::DepopPrepare(command),
                        node_id,
                        enabled,
                    );
                }
                if !voice.was_playing {
                    let _ = self.command_buffer.push(data_command, node_id);
                }

                if voice.has_any_connection() {
                    if voice.was_playing {
                        continue;
                    }
                    let biquad_detail = self.begin_performance_detail(
                        node_id as i32,
                        PerformanceEntryType::Voice,
                        PerformanceDetailType::Unk4,
                    );
                    self.push_voice_biquad_commands(&voice, &voice_state, channel as i8, node_id);
                    if let Some(addresses) = biquad_detail {
                        self.end_performance(node_id as i32, addresses);
                    }

                    let volume_detail = self.begin_performance_detail(
                        node_id as i32,
                        PerformanceEntryType::Voice,
                        PerformanceDetailType::Unk3,
                    );
                    let precision = Self::mix_precision(self.behavior);
                    let input_output = self.command_list_header.buffer_count + channel as i16;
                    let _ = self.command_buffer.push(
                        Command::VolumeRamp(VolumeRampCommand {
                            precision,
                            input_index: input_output,
                            output_index: input_output,
                            volume: voice.volume,
                            prev_volume: voice.prev_volume,
                        }),
                        node_id,
                    );

                    if voice.mix_id == UNUSED_MIX_ID {
                        if voice.splitter_id != UNUSED_SPLITTER_ID {
                            let mut destination_id = channel as i32;
                            while let Some(destination) = self
                                .splitter_context
                                .get_destination_data(voice.splitter_id, destination_id)
                                .cloned()
                            {
                                if destination.is_configured() {
                                    let mix_id = destination.get_mix_id();
                                    if let Some(mix_info) =
                                        self.mix_context.get_info(mix_id).cloned()
                                    {
                                        self.push_voice_mix_commands(
                                            destination.get_mix_volume_slice(),
                                            destination.get_mix_volume_prev_slice(),
                                            &voice_state,
                                            mix_info.buffer_offset,
                                            mix_info.buffer_count,
                                            input_output,
                                            node_id,
                                        );
                                        if let Some(destination_mut) =
                                            self.splitter_context.get_destination_data_mut(
                                                voice.splitter_id,
                                                destination_id,
                                            )
                                        {
                                            destination_mut.mark_as_need_to_update_internal_state();
                                        }
                                    }
                                }
                                destination_id += voice.channel_count as i32;
                            }
                        }
                    } else if let Some(mix_info) = self.mix_context.get_info(voice.mix_id).cloned()
                    {
                        let Some(channel_resource) = self
                            .voice_context
                            .get_channel_resource_ref(resource_id)
                            .cloned()
                        else {
                            continue;
                        };
                        self.push_voice_mix_commands(
                            &channel_resource.mix_volumes,
                            &channel_resource.prev_mix_volumes,
                            &voice_state,
                            mix_info.buffer_offset,
                            mix_info.buffer_count,
                            input_output,
                            node_id,
                        );
                        if let Some(channel_resource_mut) =
                            self.voice_context.get_channel_resource(resource_id)
                        {
                            channel_resource_mut.prev_mix_volumes =
                                channel_resource_mut.mix_volumes;
                        }
                    }
                    if let Some(addresses) = volume_detail {
                        self.end_performance(node_id as i32, addresses);
                    }
                }
            }
            if let Some(addresses) = data_detail {
                self.end_performance(node_id as i32, addresses);
            }

            if let Some(stored_voice) = self.voice_context.get_info_mut(voice_index as u32) {
                stored_voice.prev_volume = if voice.was_playing { 0.0 } else { voice.volume };
                stored_voice.biquad_initialized = stored_voice.biquads.map(|biquad| biquad.enabled);
            }

            if let Some(addresses) = entry_aspect {
                self.end_performance(node_id as i32, addresses);
            }
        }
    }

    fn build_data_source_command(
        &self,
        voice: &crate::renderer::voice::VoiceInfo,
        voice_state: &crate::renderer::voice::VoiceState,
        channel: i8,
        buffer_count: i16,
    ) -> Option<Command> {
        let mut wave_buffers = [crate::common::wave_buffer::WaveBufferVersion2::default(); 4];
        for (index, wave_buffer) in voice.wavebuffers.iter().enumerate() {
            if let Some(target) = wave_buffers.get_mut(index) {
                let mut copy = *target;
                let mut source = wave_buffer.clone();
                source.copy_to_v2(&mut copy);
                *target = copy;
            }
        }
        let mut data_address = voice.data_address;

        let data = DataSourceCommand {
            src_quality: voice.src_quality,
            output_index: buffer_count + i16::from(channel),
            flags: voice.flags & 3,
            sample_rate: voice.sample_rate,
            pitch: voice.pitch,
            channel_index: channel,
            channel_count: voice.channel_count,
            wave_buffers,
            voice_state: self.voice_state_pool.translate(
                voice_state as *const _ as CpuAddr,
                size_of_val(voice_state) as u64,
            ),
            data_address: data_address.get_reference(true),
            data_size: voice.data_address.get_size(),
        };

        Some(match voice.sample_format {
            SampleFormat::PcmInt16 => {
                if self.behavior.is_wave_buffer_ver2_supported() {
                    Command::DataSourcePcmInt16Version2(data)
                } else {
                    Command::DataSourcePcmInt16Version1(data)
                }
            }
            SampleFormat::PcmFloat => {
                if self.behavior.is_wave_buffer_ver2_supported() {
                    Command::DataSourcePcmFloatVersion2(data)
                } else {
                    Command::DataSourcePcmFloatVersion1(data)
                }
            }
            SampleFormat::Adpcm => {
                if self.behavior.is_wave_buffer_ver2_supported() {
                    Command::DataSourceAdpcmVersion2(data)
                } else {
                    Command::DataSourceAdpcmVersion1(data)
                }
            }
            SampleFormat::Invalid
            | SampleFormat::PcmInt8
            | SampleFormat::PcmInt24
            | SampleFormat::PcmInt32 => return None,
        })
    }

    fn build_voice_depop_prepare_commands(
        &self,
        voice: &crate::renderer::voice::VoiceInfo,
        voice_state: &crate::renderer::voice::VoiceState,
    ) -> Vec<(DepopPrepareCommand, bool)> {
        let mut commands = Vec::new();
        if voice.mix_id == UNUSED_MIX_ID {
            if voice.splitter_id == UNUSED_SPLITTER_ID {
                return commands;
            }

            let mut destination_id = 0i32;
            while let Some(destination) = self
                .splitter_context
                .get_destination_data(voice.splitter_id, destination_id)
                .cloned()
            {
                if destination.is_configured() {
                    let mix_id = destination.get_mix_id();
                    if let Some(mix_info) = self.mix_context.get_info(mix_id).cloned() {
                        if let Some(command) = self.build_depop_prepare_command(
                            &voice_state.previous_samples,
                            mix_info.buffer_count,
                            mix_info.buffer_offset,
                            voice.was_playing,
                        ) {
                            commands.push(command);
                        }
                    }
                }
                destination_id += 1;
            }
            return commands;
        }

        if let Some(mix_info) = self.mix_context.get_info(voice.mix_id).cloned() {
            if let Some(command) = self.build_depop_prepare_command(
                &voice_state.previous_samples,
                mix_info.buffer_count,
                mix_info.buffer_offset,
                voice.was_playing,
            ) {
                commands.push(command);
            }
        }
        commands
    }

    fn build_depop_prepare_command(
        &self,
        previous_samples: &[i32; MAX_MIX_BUFFERS as usize],
        buffer_count: i16,
        buffer_offset: i16,
        enabled: bool,
    ) -> Option<(DepopPrepareCommand, bool)> {
        let buffer_count = u32::try_from(buffer_count).ok()?;
        let mut inputs = [0i16; MAX_MIX_BUFFERS as usize];
        for (index, input) in inputs.iter_mut().enumerate() {
            *input = buffer_offset + index as i16;
        }
        Some((
            DepopPrepareCommand {
                inputs,
                previous_samples: self.voice_state_pool.translate(
                    previous_samples.as_ptr() as CpuAddr,
                    size_of_val(previous_samples) as u64,
                ),
                buffer_count,
                depop_buffer: self.depop_buffer_pool.translate(
                    self.depop_buffer.as_ptr() as CpuAddr,
                    size_of_val(self.depop_buffer) as u64,
                ),
            },
            enabled,
        ))
    }

    pub fn generate_submix_commands(&mut self) {
        let submix_count = self.mix_context.get_count();
        for index in 0..submix_count {
            let Some(mix_info) = self.mix_context.get_sorted_info(index).cloned() else {
                continue;
            };
            if !mix_info.in_use || mix_info.mix_id == FINAL_MIX_ID {
                continue;
            }

            let entry_aspect =
                self.begin_performance_entry(mix_info.node_id, PerformanceEntryType::SubMix);
            self.generate_submix_command(&mix_info);
            if let Some(addresses) = entry_aspect {
                self.end_performance(mix_info.node_id, addresses);
            }
        }
    }

    pub fn generate_final_mix_commands(&mut self) {
        let Some(final_mix_info) = self.mix_context.get_final_mix_info().cloned() else {
            return;
        };
        if !final_mix_info.in_use {
            return;
        }

        let entry_aspect =
            self.begin_performance_entry(final_mix_info.node_id, PerformanceEntryType::FinalMix);
        self.generate_final_mix_command(&final_mix_info);
        if let Some(addresses) = entry_aspect {
            self.end_performance(final_mix_info.node_id, addresses);
        }
    }

    pub fn generate_sink_commands(&mut self) {
        let Some(final_mix) = self.mix_context.get_final_mix_info().cloned() else {
            return;
        };

        let device_indices = self
            .sink_context
            .infos()
            .iter()
            .enumerate()
            .filter_map(|(index, sink)| {
                (sink.is_used() && sink.get_type() == SinkType::DeviceSink).then_some(index)
            })
            .collect::<Vec<_>>();
        for index in device_indices {
            let node_id = {
                let Some(sink) = self.sink_context.get_info(index as u32) else {
                    continue;
                };
                if self.command_list_header.sample_rate != TARGET_SAMPLE_RATE
                    && sink.device_state.upsampler_info.is_none()
                {
                    sink.device_state.upsampler_info = self.upsampler_manager.allocate();
                }
                sink.get_node_id() as i32
            };
            let entry_aspect = self.begin_performance_entry(node_id, PerformanceEntryType::Sink);
            if let Some(mut sink) = self.sink_context.infos().get(index).cloned() {
                self.generate_sink_command(final_mix.buffer_offset, &mut sink);
            }
            if let Some(sink) = self.sink_context.get_info(index as u32) {
                sink.update_for_command_generation();
            }
            if let Some(addresses) = entry_aspect {
                self.end_performance(node_id, addresses);
            }
        }

        let circular_indices = self
            .sink_context
            .infos()
            .iter()
            .enumerate()
            .filter_map(|(index, sink)| {
                (sink.is_used() && sink.get_type() == SinkType::CircularBufferSink).then_some(index)
            })
            .collect::<Vec<_>>();
        for index in circular_indices {
            let Some(sink) = self.sink_context.infos().get(index).cloned() else {
                continue;
            };
            let node_id = sink.get_node_id() as i32;
            let entry_aspect = self.begin_performance_entry(node_id, PerformanceEntryType::Sink);
            let mut sink_snapshot = sink;
            self.generate_sink_command(final_mix.buffer_offset, &mut sink_snapshot);
            if let Some(sink) = self.sink_context.get_info(index as u32) {
                sink.update_for_command_generation();
            }
            if let Some(addresses) = entry_aspect {
                self.end_performance(node_id, addresses);
            }
        }
    }

    fn generate_sink_command(
        &mut self,
        buffer_offset: i16,
        sink_info: &mut crate::renderer::sink::SinkInfoBase,
    ) {
        if sink_info.should_skip() {
            return;
        }

        match sink_info.get_type() {
            SinkType::DeviceSink => self.generate_device_command(buffer_offset, sink_info),
            SinkType::CircularBufferSink => {
                let Some(pos) = u32::try_from(sink_info.circular_state.current_pos).ok() else {
                    return;
                };
                let _ = self.command_buffer.push(
                    Command::CircularBufferSink(CircularBufferSinkCommand {
                        input_count: sink_info.circular_parameter.input_count,
                        inputs: sink_info
                            .circular_parameter
                            .inputs
                            .map(|input| buffer_offset + i16::from(input)),
                        address: sink_info.circular_state.address_info.get_reference(true),
                        size: sink_info.circular_parameter.size,
                        pos,
                    }),
                    sink_info.get_node_id(),
                );
            }
            SinkType::Invalid => {}
        }
    }

    fn generate_device_command(
        &mut self,
        buffer_offset: i16,
        sink_info: &mut crate::renderer::sink::SinkInfoBase,
    ) {
        let node_id = sink_info.get_node_id();
        if self.render_channels == 2 && sink_info.device_parameter.downmix_enabled {
            let mut inputs = [0i16; MAX_CHANNELS];
            let mut outputs = [0i16; MAX_CHANNELS];
            for i in 0..MAX_CHANNELS {
                inputs[i] = buffer_offset + i16::from(sink_info.device_parameter.inputs[i]);
                outputs[i] = buffer_offset + i16::from(sink_info.device_parameter.inputs[i]);
            }
            let _ = self.command_buffer.push(
                Command::DownMix6chTo2ch(DownMix6chTo2chCommand {
                    inputs,
                    outputs,
                    down_mix_coeff: sink_info
                        .device_parameter
                        .downmix_coeff
                        .map(|coeff| FixedPoint::<48, 16>::from_f32(coeff).to_raw()),
                }),
                node_id,
            );
        }

        let sample_buffer = if let Some(upsampler_index) = sink_info.device_state.upsampler_info {
            let upsample_meta =
                if let Some(upsampler_info) = self.upsampler_manager.get_mut(upsampler_index) {
                    upsampler_info.input_count = sink_info.device_parameter.input_count;
                    for i in 0..sink_info
                        .device_parameter
                        .input_count
                        .min(MAX_CHANNELS as u32) as usize
                    {
                        upsampler_info.inputs[i] =
                            buffer_offset + i16::from(sink_info.device_parameter.inputs[i]);
                    }
                    let max_input = upsampler_info
                        .inputs
                        .iter()
                        .take(upsampler_info.input_count as usize)
                        .copied()
                        .max()
                        .and_then(|value| u64::try_from(value).ok())
                        .unwrap_or(0);
                    Some((
                        upsampler_info.input_count,
                        upsampler_info.sample_count as u64,
                        max_input,
                    ))
                } else {
                    None
                };
            if let Some((input_count, sample_count, max_input)) = upsample_meta {
                let translated_samples_buffer = self
                    .upsampler_manager
                    .translated_command_samples_address(upsampler_index);
                let device_sample_buffer = self
                    .upsampler_manager
                    .translated_device_samples_address(upsampler_index, input_count);
                let _ = self.command_buffer.push(
                    Command::Upsample(UpsampleCommand {
                        samples_buffer: translated_samples_buffer,
                        inputs: self
                            .upsampler_manager
                            .translated_inputs_address(upsampler_index),
                        buffer_count: self.command_list_header.buffer_count.max(0) as u32,
                        unk_20: 0,
                        source_sample_count: self.command_list_header.sample_count,
                        source_sample_rate: self.command_list_header.sample_rate,
                        upsampler_info: self
                            .upsampler_manager
                            .translated_info_address(upsampler_index),
                    }),
                    node_id,
                );

                (device_sample_buffer, (max_input + 1) * sample_count)
            } else {
                (
                    self.command_list_header.samples_buffer,
                    self.samples_workbuffer_size(),
                )
            }
        } else {
            (
                self.command_list_header.samples_buffer,
                self.samples_workbuffer_size(),
            )
        };

        let mut inputs = [0i16; MAX_CHANNELS];
        for i in 0..sink_info
            .device_parameter
            .input_count
            .min(MAX_CHANNELS as u32) as usize
        {
            inputs[i] = buffer_offset + i16::from(sink_info.device_parameter.inputs[i]);
        }

        let _ = self.command_buffer.push(
            Command::DeviceSink(DeviceSinkCommand {
                name: sink_info.device_parameter.name,
                session_id: self.session_id,
                sample_buffer: sample_buffer.0,
                sample_count: sample_buffer.1,
                input_count: sink_info.device_parameter.input_count,
                inputs,
            }),
            node_id,
        );
    }

    fn samples_workbuffer_size(&self) -> u64 {
        self.command_list_header.buffer_count.max(0) as u64
            * self.command_list_header.sample_count as u64
    }

    fn checked_mix_buffer_span(mix_info: &MixInfo) -> Option<(u32, u32)> {
        Some((
            u32::try_from(mix_info.buffer_offset).ok()?,
            u32::try_from(mix_info.buffer_count).ok()?,
        ))
    }

    fn generate_submix_command(&mut self, mix_info: &MixInfo) {
        let Some((input, count)) = Self::checked_mix_buffer_span(mix_info) else {
            return;
        };
        let _ = self.command_buffer.push(
            Command::DepopForMixBuffers(DepopForMixBuffersCommand {
                input,
                count,
                decay: Self::depop_decay(mix_info.sample_rate).to_raw(),
                depop_buffer: self.depop_buffer_pool.translate(
                    self.depop_buffer.as_ptr() as CpuAddr,
                    size_of_val(self.depop_buffer) as u64,
                ),
            }),
            mix_info.node_id as u32,
        );
        self.generate_effect_commands(mix_info);
        let mix_detail = self.begin_performance_detail(
            mix_info.node_id,
            PerformanceEntryType::SubMix,
            PerformanceDetailType::Unk5,
        );
        self.generate_mix_commands(mix_info);
        if let Some(addresses) = mix_detail {
            self.end_performance(mix_info.node_id, addresses);
        }
    }

    fn generate_final_mix_command(&mut self, mix_info: &MixInfo) {
        let Some((input, count)) = Self::checked_mix_buffer_span(mix_info) else {
            return;
        };
        let _ = self.command_buffer.push(
            Command::DepopForMixBuffers(DepopForMixBuffersCommand {
                input,
                count,
                decay: Self::depop_decay(mix_info.sample_rate).to_raw(),
                depop_buffer: self.depop_buffer_pool.translate(
                    self.depop_buffer.as_ptr() as CpuAddr,
                    size_of_val(self.depop_buffer) as u64,
                ),
            }),
            mix_info.node_id as u32,
        );
        self.generate_effect_commands(mix_info);

        let precision = if self
            .behavior
            .is_volume_mix_parameter_precision_q23_supported()
        {
            23
        } else {
            15
        };

        for mix_index in 0..count {
            let volume_detail = self.begin_performance_detail(
                mix_info.node_id,
                PerformanceEntryType::FinalMix,
                PerformanceDetailType::Unk3,
            );
            let _ = self.command_buffer.push(
                Command::Volume(VolumeCommand {
                    input_index: mix_info.buffer_offset + mix_index as i16,
                    output_index: mix_info.buffer_offset + mix_index as i16,
                    precision,
                    volume: mix_info.volume,
                }),
                mix_info.node_id as u32,
            );
            if let Some(addresses) = volume_detail {
                self.end_performance(mix_info.node_id, addresses);
            }
        }
    }

    fn generate_effect_commands(&mut self, mix_info: &MixInfo) {
        for effect_index in mix_info.effect_order_buffer.iter().copied() {
            if effect_index < 0 {
                break;
            }

            let Some(mut effect_info) = self
                .effect_context
                .update_info_for_command_generation(effect_index as usize)
            else {
                continue;
            };
            if effect_info.should_skip() {
                continue;
            }

            let detail_type = match effect_info.get_type() {
                EffectType::Invalid => PerformanceDetailType::Invalid,
                EffectType::Mix => PerformanceDetailType::Unk5,
                EffectType::Aux => PerformanceDetailType::Unk7,
                EffectType::Delay => PerformanceDetailType::Unk6,
                EffectType::Reverb => PerformanceDetailType::Unk8,
                EffectType::I3dl2Reverb => PerformanceDetailType::Unk9,
                EffectType::BiquadFilter => PerformanceDetailType::Unk4,
                EffectType::LightLimiter => PerformanceDetailType::Unk11,
                EffectType::Capture => PerformanceDetailType::Unk12,
                EffectType::Compressor => PerformanceDetailType::Unk13,
            };
            let entry_type = if mix_info.mix_id == FINAL_MIX_ID {
                PerformanceEntryType::FinalMix
            } else {
                PerformanceEntryType::SubMix
            };
            let detail_aspect =
                self.begin_performance_detail(mix_info.node_id, entry_type, detail_type);
            self.push_effect_commands(mix_info, effect_index as usize, &mut effect_info);
            if let Some(addresses) = detail_aspect {
                self.end_performance(mix_info.node_id, addresses);
            }
        }
    }

    fn push_effect_commands(
        &mut self,
        mix_info: &MixInfo,
        effect_index: usize,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        match effect_info.get_type() {
            EffectType::Invalid => {}
            EffectType::Mix => self.push_buffer_mixer_commands(mix_info, effect_info),
            EffectType::Aux => self.push_aux_commands(mix_info, effect_info),
            EffectType::Delay => self.push_delay_command(mix_info, effect_info),
            EffectType::Reverb => self.push_reverb_command(mix_info, effect_info),
            EffectType::I3dl2Reverb => self.push_i3dl2_reverb_command(mix_info, effect_info),
            EffectType::BiquadFilter => {
                self.push_biquad_filter_effect_commands(mix_info, effect_info)
            }
            EffectType::LightLimiter => {
                self.push_light_limiter_command(mix_info, effect_index, effect_info)
            }
            EffectType::Capture => self.push_capture_commands(mix_info, effect_info),
            EffectType::Compressor => self.push_compressor_command(mix_info, effect_info),
        }
    }

    fn push_buffer_mixer_commands(
        &mut self,
        mix_info: &MixInfo,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        let parameter = if self.behavior.is_effect_info_version2_supported() {
            effect_info.read_parameter::<buffer_mixer::ParameterVersion2>()
        } else {
            Self::convert_buffer_mixer_parameter(
                effect_info.read_parameter::<buffer_mixer::ParameterVersion1>(),
            )
        };
        if !effect_info.is_enabled() {
            return;
        }
        let precision = Self::mix_precision(self.behavior);

        for index in 0..parameter.mix_count.min(MAX_MIX_BUFFERS) as usize {
            if parameter.volumes[index] == 0.0 {
                continue;
            }
            let _ = self.command_buffer.push(
                Command::Mix(MixCommand {
                    precision,
                    input_index: mix_info.buffer_offset + i16::from(parameter.inputs[index]),
                    output_index: mix_info.buffer_offset + i16::from(parameter.outputs[index]),
                    volume: parameter.volumes[index],
                }),
                mix_info.node_id as u32,
            );
        }
    }

    fn push_voice_mix_commands(
        &mut self,
        mix_volumes: &[f32],
        prev_mix_volumes: &[f32],
        voice_state: &crate::renderer::voice::VoiceState,
        output_index: i16,
        buffer_count: i16,
        input_index: i16,
        node_id: u32,
    ) {
        let precision = Self::mix_precision(self.behavior);
        if buffer_count > 8 {
            let mut inputs = [0i16; MAX_MIX_BUFFERS as usize];
            let mut outputs = [0i16; MAX_MIX_BUFFERS as usize];
            let mut prev_volumes = [0.0f32; MAX_MIX_BUFFERS as usize];
            let mut volumes = [0.0f32; MAX_MIX_BUFFERS as usize];
            let Some(count) = usize::try_from(buffer_count).ok() else {
                return;
            };
            for i in 0..count {
                inputs[i] = input_index;
                outputs[i] = output_index + i as i16;
                prev_volumes[i] = prev_mix_volumes.get(i).copied().unwrap_or(0.0);
                volumes[i] = mix_volumes.get(i).copied().unwrap_or(0.0);
            }
            let _ = self.command_buffer.push(
                Command::MixRampGrouped(MixRampGroupedCommand {
                    buffer_count: count as u32,
                    precision,
                    inputs,
                    outputs,
                    prev_volumes,
                    volumes,
                    previous_samples: self.voice_state_pool.translate(
                        voice_state.previous_samples.as_ptr() as CpuAddr,
                        size_of_val(&voice_state.previous_samples) as u64,
                    ),
                }),
                node_id,
            );
            return;
        }

        let Some(count) = usize::try_from(buffer_count).ok() else {
            return;
        };
        for i in 0..count {
            let prev_volume = prev_mix_volumes.get(i).copied().unwrap_or(0.0);
            let volume = mix_volumes.get(i).copied().unwrap_or(0.0);
            if volume == 0.0 && prev_volume == 0.0 {
                continue;
            }
            let _ = self.command_buffer.push(
                Command::MixRamp(MixRampCommand {
                    precision,
                    input_index,
                    output_index: output_index + i as i16,
                    prev_volume,
                    volume,
                    previous_sample: self.voice_state_pool.translate(
                        &voice_state.previous_samples[i] as *const i32 as CpuAddr,
                        std::mem::size_of::<i32>() as u64,
                    ),
                }),
                node_id,
            );
        }
    }

    fn push_voice_biquad_commands(
        &mut self,
        voice: &crate::renderer::voice::VoiceInfo,
        voice_state: &crate::renderer::voice::VoiceState,
        channel: i8,
        node_id: u32,
    ) {
        let both_biquads_enabled = voice.biquads[0].enabled && voice.biquads[1].enabled;
        let use_float_processing = self.behavior.use_biquad_filter_float_processing();
        let input_output = self.command_list_header.buffer_count + i16::from(channel);

        if both_biquads_enabled
            && self.behavior.use_multi_tap_biquad_filter_processing()
            && use_float_processing
        {
            let _ = self.command_buffer.push(
                Command::MultiTapBiquadFilter(MultiTapBiquadFilterCommand {
                    input: input_output,
                    output: input_output,
                    biquads: voice.biquads,
                    states: [
                        self.voice_state_pool.translate(
                            voice_state.biquad_states[0].as_ptr() as CpuAddr,
                            size_of_val(&voice_state.biquad_states[0]) as u64,
                        ),
                        self.voice_state_pool.translate(
                            voice_state.biquad_states[1].as_ptr() as CpuAddr,
                            size_of_val(&voice_state.biquad_states[1]) as u64,
                        ),
                    ],
                    needs_init: [!voice.biquad_initialized[0], !voice.biquad_initialized[1]],
                    filter_tap_count: MAX_BIQUAD_FILTERS as u8,
                }),
                node_id,
            );
            return;
        }

        for biquad_index in 0..MAX_BIQUAD_FILTERS as usize {
            let biquad = voice.biquads[biquad_index];
            if !biquad.enabled {
                continue;
            }
            let _ = self.command_buffer.push(
                Command::BiquadFilter(BiquadFilterCommand {
                    input: input_output,
                    output: input_output,
                    biquad,
                    state: self.voice_state_pool.translate(
                        voice_state.biquad_states[biquad_index].as_ptr() as CpuAddr,
                        size_of_val(&voice_state.biquad_states[biquad_index]) as u64,
                    ),
                    needs_init: !voice.biquad_initialized[biquad_index],
                    use_float_processing,
                }),
                node_id,
            );
        }
    }

    fn push_aux_commands(
        &mut self,
        mix_info: &MixInfo,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        let parameter = if self.behavior.is_effect_info_version2_supported() {
            effect_info.read_parameter::<aux_::ParameterVersion2>()
        } else {
            Self::convert_aux_parameter(effect_info.read_parameter::<aux_::ParameterVersion1>())
        };

        if effect_info.is_enabled() {
            let _ = effect_info.get_workbuffer(0);
            let _ = effect_info.get_workbuffer(1);
        }

        if effect_info.get_send_buffer() == 0 || effect_info.get_return_buffer() == 0 {
            return;
        }

        let mut write_offset = 0u32;
        for index in 0..parameter.mix_buffer_count.min(MAX_MIX_BUFFERS) as usize {
            let new_update_count =
                self.command_list_header.sample_count.max(0) as u32 + write_offset;
            let channel_index = parameter.mix_buffer_count as usize - 1 - index;
            let update_count = if channel_index > 0 {
                0
            } else {
                new_update_count
            };
            let _ = self.command_buffer.push(
                Command::Aux(AuxCommand {
                    input: mix_info.buffer_offset as i16 + parameter.inputs[index] as i16,
                    output: mix_info.buffer_offset as i16 + parameter.outputs[index] as i16,
                    send_buffer_info: effect_info.get_send_buffer_info(),
                    return_buffer_info: effect_info.get_return_buffer_info(),
                    send_buffer: effect_info.get_send_buffer(),
                    return_buffer: effect_info.get_return_buffer(),
                    count_max: parameter.count_max,
                    write_offset,
                    update_count,
                    effect_enabled: effect_info.is_enabled(),
                }),
                mix_info.node_id as u32,
            );
            write_offset = new_update_count;
        }
    }

    fn push_capture_commands(
        &mut self,
        mix_info: &MixInfo,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        let parameter = if self.behavior.is_effect_info_version2_supported() {
            effect_info.read_parameter::<aux_::ParameterVersion2>()
        } else {
            Self::convert_capture_parameter(effect_info.read_parameter::<aux_::ParameterVersion1>())
        };

        if effect_info.is_enabled() {
            let _ = effect_info.get_workbuffer(0);
        }

        if effect_info.get_send_buffer() == 0 {
            return;
        }

        let mut write_offset = 0u32;
        for index in 0..parameter.mix_buffer_count.min(MAX_MIX_BUFFERS) as usize {
            let new_update_count =
                self.command_list_header.sample_count.max(0) as u32 + write_offset;
            let channel_index = parameter.mix_buffer_count as usize - 1 - index;
            let update_count = if channel_index > 0 {
                0
            } else {
                new_update_count
            };
            let _ = self.command_buffer.push(
                Command::Capture(CaptureCommand {
                    input: mix_info.buffer_offset as i16 + parameter.inputs[index] as i16,
                    output: mix_info.buffer_offset as i16 + parameter.outputs[index] as i16,
                    send_buffer_info: effect_info.get_send_buffer_info(),
                    send_buffer: effect_info.get_send_buffer(),
                    count_max: parameter.count_max,
                    write_offset,
                    update_count,
                    effect_enabled: effect_info.is_enabled(),
                }),
                mix_info.node_id as u32,
            );
            write_offset = new_update_count;
        }
    }

    fn push_delay_command(
        &mut self,
        mix_info: &MixInfo,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        let parameter = if self.behavior.is_effect_info_version2_supported() {
            effect_info.read_parameter::<delay::ParameterVersion2>()
        } else {
            Self::convert_delay_parameter(effect_info.read_parameter::<delay::ParameterVersion1>())
        };
        if !is_channel_count_valid(parameter.channel_count as u16) {
            return;
        }

        let (mut inputs, mut outputs) = Self::build_effect_channels(
            mix_info.buffer_offset.into(),
            &parameter.inputs,
            &parameter.outputs,
            parameter.channel_count as usize,
        );
        if !self.behavior.is_delay_channel_mapping_changed() && parameter.channel_count == 6 {
            use_old_channel_mapping(&mut inputs, &mut outputs);
        }

        let _ = self.command_buffer.push(
            Command::Delay(DelayCommand {
                inputs,
                outputs,
                parameter,
                state: self.translate_effect_state_address(effect_info, 0),
                workbuffer: effect_info.get_workbuffer(-1),
                effect_enabled: effect_info.is_enabled(),
            }),
            mix_info.node_id as u32,
        );
    }

    fn push_reverb_command(
        &mut self,
        mix_info: &MixInfo,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        let parameter = if self.behavior.is_effect_info_version2_supported() {
            effect_info.read_parameter::<reverb::ParameterVersion2>()
        } else {
            Self::convert_reverb_parameter(
                effect_info.read_parameter::<reverb::ParameterVersion1>(),
            )
        };
        if !is_channel_count_valid(parameter.channel_count) {
            return;
        }

        let (mut inputs, mut outputs) = Self::build_effect_channels(
            mix_info.buffer_offset.into(),
            &parameter.inputs,
            &parameter.outputs,
            parameter.channel_count as usize,
        );
        if !self.behavior.is_reverb_channel_mapping_changed() && parameter.channel_count == 6 {
            use_old_channel_mapping(&mut inputs, &mut outputs);
        }

        let _ = self.command_buffer.push(
            Command::Reverb(ReverbCommand {
                inputs,
                outputs,
                parameter,
                state: self.translate_effect_state_address(effect_info, 0),
                workbuffer: effect_info.get_workbuffer(-1),
                effect_enabled: effect_info.is_enabled(),
                long_size_pre_delay_supported: self.behavior.is_long_size_pre_delay_supported(),
            }),
            mix_info.node_id as u32,
        );
    }

    fn push_i3dl2_reverb_command(
        &mut self,
        mix_info: &MixInfo,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        let parameter = if self.behavior.is_effect_info_version2_supported() {
            effect_info.read_parameter::<i3dl2::ParameterVersion2>()
        } else {
            Self::convert_i3dl2_parameter(effect_info.read_parameter::<i3dl2::ParameterVersion1>())
        };
        if !is_channel_count_valid(parameter.channel_count as u16) {
            return;
        }

        let (mut inputs, mut outputs) = Self::build_effect_channels(
            mix_info.buffer_offset.into(),
            &parameter.inputs,
            &parameter.outputs,
            parameter.channel_count as usize,
        );
        if !self.behavior.is_i3dl2_reverb_channel_mapping_changed() && parameter.channel_count == 6
        {
            use_old_channel_mapping(&mut inputs, &mut outputs);
        }

        let _ = self.command_buffer.push(
            Command::I3dl2Reverb(I3dl2ReverbCommand {
                inputs,
                outputs,
                parameter,
                state: self.translate_effect_state_address(effect_info, 0),
                workbuffer: effect_info.get_workbuffer(-1),
                effect_enabled: effect_info.is_enabled(),
            }),
            mix_info.node_id as u32,
        );
    }

    fn push_light_limiter_command(
        &mut self,
        mix_info: &MixInfo,
        effect_index: usize,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        if self.behavior.is_effect_info_version2_supported() {
            let parameter = effect_info.read_parameter::<light_limiter::ParameterVersion2>();
            if !is_channel_count_valid(parameter.channel_count) {
                return;
            }
            let (inputs, outputs) = Self::build_effect_channels(
                mix_info.buffer_offset.into(),
                &parameter.inputs,
                &parameter.outputs,
                parameter.channel_count as usize,
            );
            let result_state = if parameter.statistics_enabled {
                self.effect_context
                    .get_dsp_shared_result_state(effect_index as u32)
                    .map(|state| {
                        self.effect_result_state_pool
                            .translate(state as *const _ as CpuAddr, size_of_val(state) as u64)
                    })
                    .unwrap_or(0)
            } else {
                0
            };
            let _ = self.command_buffer.push(
                Command::LightLimiterVersion2(LightLimiterVersion2Command {
                    inputs,
                    outputs,
                    parameter,
                    state: self.translate_effect_state_address(effect_info, 0),
                    workbuffer: effect_info.get_workbuffer(-1),
                    result_state,
                    effect_enabled: effect_info.is_enabled(),
                }),
                mix_info.node_id as u32,
            );
        } else {
            let parameter = effect_info.read_parameter::<light_limiter::ParameterVersion1>();
            if !is_channel_count_valid(parameter.channel_count) {
                return;
            }
            let (inputs, outputs) = Self::build_effect_channels(
                mix_info.buffer_offset.into(),
                &parameter.inputs,
                &parameter.outputs,
                parameter.channel_count as usize,
            );
            let _ = self.command_buffer.push(
                Command::LightLimiterVersion1(LightLimiterVersion1Command {
                    inputs,
                    outputs,
                    parameter,
                    state: self.translate_effect_state_address(effect_info, 0),
                    workbuffer: effect_info.get_workbuffer(-1),
                    effect_enabled: effect_info.is_enabled(),
                }),
                mix_info.node_id as u32,
            );
        }
    }

    fn push_biquad_filter_effect_commands(
        &mut self,
        mix_info: &MixInfo,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        let parameter = if self.behavior.is_effect_info_version2_supported() {
            effect_info.read_parameter::<biquad_filter::ParameterVersion2>()
        } else {
            Self::convert_biquad_parameter(
                effect_info.read_parameter::<biquad_filter::ParameterVersion1>(),
            )
        };

        let channel_count = parameter.channel_count.max(0) as usize;
        if channel_count == 0 || channel_count > MAX_CHANNELS {
            return;
        }

        if effect_info.is_enabled() {
            let needs_init = match parameter.state {
                crate::renderer::effect::effect_info_base::ParameterState::Initialized => true,
                crate::renderer::effect::effect_info_base::ParameterState::Updating => !self
                    .behavior
                    .is_biquad_filter_effect_state_clear_bug_fixed(),
                crate::renderer::effect::effect_info_base::ParameterState::Updated => false,
            };

            for channel in 0..channel_count {
                let state = self.translate_effect_state_address(
                    effect_info,
                    channel * std::mem::size_of::<BiquadFilterState>(),
                );
                let _ = self.command_buffer.push(
                    Command::BiquadFilter(BiquadFilterCommand {
                        input: mix_info.buffer_offset + i16::from(parameter.inputs[channel]),
                        output: mix_info.buffer_offset + i16::from(parameter.outputs[channel]),
                        biquad: Self::build_biquad_filter_parameter(&parameter),
                        state,
                        needs_init,
                        use_float_processing: self.behavior.use_biquad_filter_float_processing(),
                    }),
                    mix_info.node_id as u32,
                );
            }
        } else {
            for channel in 0..channel_count {
                let _ = self.command_buffer.push(
                    Command::CopyMixBuffer(CopyMixBufferCommand {
                        input_index: mix_info.buffer_offset + i16::from(parameter.inputs[channel]),
                        output_index: mix_info.buffer_offset
                            + i16::from(parameter.outputs[channel]),
                    }),
                    mix_info.node_id as u32,
                );
            }
        }
    }

    fn push_compressor_command(
        &mut self,
        mix_info: &MixInfo,
        effect_info: &mut crate::renderer::effect::EffectInfoBase,
    ) {
        let parameter = effect_info.read_parameter::<compressor::ParameterVersion2>();
        if !is_channel_count_valid(parameter.channel_count as u16) {
            return;
        }

        let (inputs, outputs) = Self::build_effect_channels(
            mix_info.buffer_offset.into(),
            &parameter.inputs,
            &parameter.outputs,
            parameter.channel_count as usize,
        );
        let _ = self.command_buffer.push(
            Command::Compressor(CompressorCommand {
                inputs,
                outputs,
                parameter,
                state: self.translate_effect_state_address(effect_info, 0),
                workbuffer: effect_info.get_workbuffer(-1),
                enabled: effect_info.is_enabled(),
            }),
            mix_info.node_id as u32,
        );
    }

    fn translate_effect_state_address(
        &self,
        effect_info: &crate::renderer::effect::EffectInfoBase,
        offset: usize,
    ) -> CpuAddr {
        self.effect_state_pool
            .translate(
                effect_info.get_state_address(),
                effect_info.get_state_size(),
            )
            .saturating_add(offset)
    }

    fn build_effect_channels(
        buffer_offset: i32,
        inputs: &[i8; MAX_CHANNELS],
        outputs: &[i8; MAX_CHANNELS],
        channel_count: usize,
    ) -> ([i16; MAX_CHANNELS], [i16; MAX_CHANNELS]) {
        let mut mapped_inputs = [0i16; MAX_CHANNELS];
        let mut mapped_outputs = [0i16; MAX_CHANNELS];
        let count = channel_count.min(MAX_CHANNELS);
        for channel in 0..count {
            mapped_inputs[channel] = buffer_offset as i16 + inputs[channel] as i16;
            mapped_outputs[channel] = buffer_offset as i16 + outputs[channel] as i16;
        }
        (mapped_inputs, mapped_outputs)
    }

    fn convert_buffer_mixer_parameter(
        parameter: buffer_mixer::ParameterVersion1,
    ) -> buffer_mixer::ParameterVersion2 {
        buffer_mixer::ParameterVersion2 {
            inputs: parameter.inputs,
            outputs: parameter.outputs,
            volumes: parameter.volumes,
            mix_count: parameter.mix_count,
        }
    }

    fn convert_aux_parameter(parameter: aux_::ParameterVersion1) -> aux_::ParameterVersion2 {
        aux_::ParameterVersion2 {
            inputs: parameter.inputs,
            outputs: parameter.outputs,
            mix_buffer_count: parameter.mix_buffer_count,
            sample_rate: parameter.sample_rate,
            count_max: parameter.count_max,
            mix_buffer_count_max: parameter.mix_buffer_count_max,
            send_buffer_info_address: parameter.send_buffer_info_address,
            send_buffer_address: parameter.send_buffer_address,
            return_buffer_info_address: parameter.return_buffer_info_address,
            return_buffer_address: parameter.return_buffer_address,
            mix_buffer_sample_size: parameter.mix_buffer_sample_size,
            sample_count: parameter.sample_count,
            mix_buffer_sample_count: parameter.mix_buffer_sample_count,
        }
    }

    fn convert_capture_parameter(parameter: aux_::ParameterVersion1) -> aux_::ParameterVersion2 {
        aux_::ParameterVersion2 {
            inputs: parameter.inputs,
            outputs: parameter.outputs,
            mix_buffer_count: parameter.mix_buffer_count,
            sample_rate: parameter.sample_rate,
            count_max: parameter.count_max,
            mix_buffer_count_max: parameter.mix_buffer_count_max,
            send_buffer_info_address: parameter.send_buffer_info_address,
            send_buffer_address: parameter.send_buffer_address,
            return_buffer_info_address: parameter.return_buffer_info_address,
            return_buffer_address: parameter.return_buffer_address,
            mix_buffer_sample_size: parameter.mix_buffer_sample_size,
            sample_count: parameter.sample_count,
            mix_buffer_sample_count: parameter.mix_buffer_sample_count,
        }
    }

    fn convert_delay_parameter(parameter: delay::ParameterVersion1) -> delay::ParameterVersion2 {
        delay::ParameterVersion2 {
            inputs: parameter.inputs,
            outputs: parameter.outputs,
            channel_count_max: parameter.channel_count_max as i16,
            channel_count: parameter.channel_count as i16,
            delay_time_max: parameter.delay_time_max as i32,
            delay_time: parameter.delay_time as i32,
            sample_rate: parameter.sample_rate as i32,
            in_gain: parameter.in_gain,
            feedback_gain: parameter.feedback_gain,
            wet_gain: parameter.wet_gain,
            dry_gain: parameter.dry_gain,
            channel_spread: parameter.channel_spread,
            lowpass_amount: parameter.lowpass_amount,
            state: parameter.state,
        }
    }

    fn convert_reverb_parameter(parameter: reverb::ParameterVersion1) -> reverb::ParameterVersion2 {
        reverb::ParameterVersion2 {
            inputs: parameter.inputs,
            outputs: parameter.outputs,
            channel_count_max: parameter.channel_count_max,
            channel_count: parameter.channel_count,
            sample_rate: parameter.sample_rate,
            early_mode: parameter.early_mode,
            early_gain: parameter.early_gain,
            pre_delay: parameter.pre_delay,
            late_mode: parameter.late_mode,
            late_gain: parameter.late_gain,
            decay_time: parameter.decay_time,
            high_freq_decay_ratio: parameter.high_freq_decay_ratio,
            colouration: parameter.colouration,
            base_gain: parameter.base_gain,
            wet_gain: parameter.wet_gain,
            dry_gain: parameter.dry_gain,
            state: parameter.state,
        }
    }

    fn convert_i3dl2_parameter(parameter: i3dl2::ParameterVersion1) -> i3dl2::ParameterVersion2 {
        i3dl2::ParameterVersion2 {
            inputs: parameter.inputs,
            outputs: parameter.outputs,
            channel_count_max: parameter.channel_count_max,
            channel_count: parameter.channel_count,
            unk10: parameter.unk10,
            sample_rate: parameter.sample_rate,
            room_hf_gain: parameter.room_hf_gain,
            reference_hf: parameter.reference_hf,
            late_reverb_decay_time: parameter.late_reverb_decay_time,
            late_reverb_hf_decay_ratio: parameter.late_reverb_hf_decay_ratio,
            room_gain: parameter.room_gain,
            reflection_gain: parameter.reflection_gain,
            reverb_gain: parameter.reverb_gain,
            late_reverb_diffusion: parameter.late_reverb_diffusion,
            reflection_delay: parameter.reflection_delay,
            late_reverb_delay_time: parameter.late_reverb_delay_time,
            late_reverb_density: parameter.late_reverb_density,
            dry_gain: parameter.dry_gain,
            state: parameter.state,
            unk49: parameter.unk49,
        }
    }

    fn convert_biquad_parameter(
        parameter: biquad_filter::ParameterVersion1,
    ) -> biquad_filter::ParameterVersion2 {
        biquad_filter::ParameterVersion2 {
            inputs: parameter.inputs,
            outputs: parameter.outputs,
            b: parameter.b,
            a: parameter.a,
            channel_count: parameter.channel_count,
            state: parameter.state,
        }
    }

    fn build_biquad_filter_parameter(
        parameter: &biquad_filter::ParameterVersion2,
    ) -> crate::renderer::voice::voice_info::BiquadFilterParameter {
        crate::renderer::voice::voice_info::BiquadFilterParameter {
            enabled: true,
            _padding: 0,
            b: parameter.b,
            a: parameter.a,
        }
    }

    fn mix_precision(behavior: &BehaviorInfo) -> u8 {
        if behavior.is_volume_mix_parameter_precision_q23_supported() {
            23
        } else {
            15
        }
    }

    fn depop_decay(sample_rate: u32) -> FixedPoint<49, 15> {
        if sample_rate == TARGET_SAMPLE_RATE {
            FixedPoint::from_f32(0.962_188_7)
        } else {
            FixedPoint::from_f32(0.943_695_07)
        }
    }

    fn generate_mix_commands(&mut self, mix_info: &MixInfo) {
        if !mix_info.has_any_connection() {
            return;
        }
        let Some((input_base, output_count)) = Self::checked_mix_buffer_span(mix_info) else {
            return;
        };
        let precision = Self::mix_precision(self.behavior);

        if mix_info.dst_mix_id == UNUSED_MIX_ID {
            if mix_info.dst_splitter_id == UNUSED_SPLITTER_ID {
                return;
            }

            let buffer_count = output_count.max(1) as i32;
            let mut dest_id = 0i32;
            while let Some(destination) = self
                .splitter_context
                .get_destination_data(mix_info.dst_splitter_id, dest_id)
            {
                if destination.is_configured() {
                    let splitter_mix_id = destination.get_mix_id();
                    let Some(splitter_mix_info) =
                        self.mix_context.get_info(splitter_mix_id).cloned()
                    else {
                        dest_id += 1;
                        continue;
                    };
                    let Some(splitter_output_count) =
                        u32::try_from(splitter_mix_info.buffer_count).ok()
                    else {
                        dest_id += 1;
                        continue;
                    };
                    let input_index = input_base + (dest_id.rem_euclid(buffer_count)) as u32;
                    for output_index in 0..splitter_output_count {
                        let volume = mix_info.volume * destination.get_mix_volume(output_index);
                        if volume == 0.0 {
                            continue;
                        }

                        let _ = self.command_buffer.push(
                            Command::Mix(MixCommand {
                                precision,
                                input_index: input_index as i16,
                                output_index: splitter_mix_info.buffer_offset + output_index as i16,
                                volume,
                            }),
                            mix_info.node_id as u32,
                        );
                    }
                }
                dest_id += 1;
            }
            return;
        }

        let Some(dest_mix_info) = self.mix_context.get_info(mix_info.dst_mix_id).cloned() else {
            return;
        };
        let Some(dest_buffer_count) = usize::try_from(dest_mix_info.buffer_count).ok() else {
            return;
        };

        for src_index in 0..output_count as usize {
            for dst_index in 0..dest_buffer_count {
                let volume = mix_info.volume * mix_info.mix_volumes[src_index][dst_index];
                if volume == 0.0 {
                    continue;
                }

                let _ = self.command_buffer.push(
                    Command::Mix(MixCommand {
                        precision,
                        input_index: mix_info.buffer_offset + src_index as i16,
                        output_index: dest_mix_info.buffer_offset + dst_index as i16,
                        volume,
                    }),
                    mix_info.node_id as u32,
                );
            }
        }
    }

    fn begin_performance_entry(
        &mut self,
        node_id: i32,
        entry_type: PerformanceEntryType,
    ) -> Option<PerformanceEntryAddresses> {
        let Some(manager) = self.performance_manager.as_deref_mut() else {
            return None;
        };

        let entry_aspect = EntryAspect::new(manager, entry_type, node_id);
        if !entry_aspect.initialized {
            return None;
        }

        let _ = self.command_buffer.push(
            Command::Performance(PerformanceCommand {
                state: PerformanceState::Start,
                entry_addresses: entry_aspect.performance_entry_address,
            }),
            node_id.max(HIGHEST_VOICE_PRIORITY) as u32,
        );
        Some(entry_aspect.performance_entry_address)
    }

    fn begin_performance_detail(
        &mut self,
        node_id: i32,
        entry_type: PerformanceEntryType,
        detail_type: PerformanceDetailType,
    ) -> Option<PerformanceEntryAddresses> {
        let Some(manager) = self.performance_manager.as_deref_mut() else {
            return None;
        };
        if detail_type == PerformanceDetailType::Invalid
            || !manager.is_detail_target(node_id as u32)
        {
            return None;
        }

        let detail_aspect = DetailAspect::new(manager, detail_type, entry_type, node_id);
        if !detail_aspect.initialized {
            return None;
        }

        let _ = self.command_buffer.push(
            Command::Performance(PerformanceCommand {
                state: PerformanceState::Start,
                entry_addresses: detail_aspect.performance_entry_address,
            }),
            node_id.max(HIGHEST_VOICE_PRIORITY) as u32,
        );
        Some(detail_aspect.performance_entry_address)
    }

    fn end_performance(&mut self, node_id: i32, entry_addresses: PerformanceEntryAddresses) {
        let _ = self.command_buffer.push(
            Command::Performance(PerformanceCommand {
                state: PerformanceState::Stop,
                entry_addresses,
            }),
            node_id.max(HIGHEST_VOICE_PRIORITY) as u32,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::renderer::command::command_processing_time_estimator::CommandProcessingTimeEstimator;
    use crate::renderer::memory::{MemoryPoolInfo, PoolLocation};
    use crate::renderer::sink::{DeviceInParameter, SinkContext, SinkInfoBase, SinkType};

    fn make_generator<'a>(
        command_buffer: &'a mut CommandBuffer,
        command_list_header: &'a mut CommandListHeader,
        behavior: &'a BehaviorInfo,
        voice_context: &'a mut VoiceContext,
        mix_context: &'a mut MixContext,
        effect_context: &'a mut EffectContext,
        sink_context: &'a mut SinkContext,
        splitter_context: &'a mut SplitterContext,
        upsampler_manager: &'a mut UpsamplerManager,
        depop_buffer: &'a [i32],
        depop_buffer_pool: &'a MemoryPoolInfo,
        voice_state_pool: &'a MemoryPoolInfo,
        effect_state_pool: &'a MemoryPoolInfo,
        effect_result_state_pool: &'a MemoryPoolInfo,
    ) -> CommandGenerator<'a> {
        CommandGenerator::new(
            command_buffer,
            command_list_header,
            behavior,
            voice_context,
            mix_context,
            effect_context,
            sink_context,
            splitter_context,
            None,
            voice_state_pool,
            effect_state_pool,
            effect_result_state_pool,
            upsampler_manager,
            17,
            2,
            depop_buffer,
            depop_buffer_pool,
        )
    }

    #[test]
    fn device_related_commands_use_sink_node_id() {
        let behavior = BehaviorInfo::new();
        let mut header = CommandListHeader {
            buffer_size: 4096,
            command_count: 0,
            samples_buffer: 0x1000,
            buffer_count: 6,
            sample_count: 160,
            sample_rate: 32_000,
        };
        let estimator = CommandProcessingTimeEstimator::new(&behavior, 160, 6);
        let mut command_buffer = CommandBuffer::new(4096, estimator);
        let mut voice_context = VoiceContext::new();
        let mut mix_context = MixContext::new();
        let mut effect_context = EffectContext::new();
        let mut sink_context = SinkContext::default();
        let mut splitter_context = SplitterContext::new();
        let mut upsampler_manager = UpsamplerManager::new(1);
        let upsampler_index = upsampler_manager.allocate().expect("upsampler");
        let depop_buffer = [0i32; MAX_MIX_BUFFERS as usize];
        let depop_buffer_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let voice_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let effect_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let effect_result_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let mut sink_info = SinkInfoBase {
            sink_type: SinkType::DeviceSink,
            in_use: true,
            node_id: 0x55,
            device_state: crate::renderer::sink::DeviceState {
                upsampler_info: Some(upsampler_index),
                ..Default::default()
            },
            device_parameter: DeviceInParameter {
                input_count: 2,
                inputs: [0, 1, 0, 0, 0, 0],
                downmix_enabled: true,
                downmix_coeff: [1.0, 0.5, 0.25, 0.125],
                ..Default::default()
            },
            ..Default::default()
        };

        let mut generator = make_generator(
            &mut command_buffer,
            &mut header,
            &behavior,
            &mut voice_context,
            &mut mix_context,
            &mut effect_context,
            &mut sink_context,
            &mut splitter_context,
            &mut upsampler_manager,
            &depop_buffer,
            &depop_buffer_pool,
            &voice_state_pool,
            &effect_state_pool,
            &effect_result_state_pool,
        );

        generator.generate_device_command(4, &mut sink_info);

        let entries = generator.command_buffer.entries();
        assert_eq!(entries.len(), 3);
        assert!(matches!(entries[0].command, Command::DownMix6chTo2ch(_)));
        assert!(matches!(entries[1].command, Command::Upsample(_)));
        assert!(matches!(entries[2].command, Command::DeviceSink(_)));
        assert!(entries.iter().all(|entry| entry.node_id == 0x55));
    }

    #[test]
    fn invalid_negative_mix_buffer_state_does_not_emit_mix_commands() {
        let behavior = BehaviorInfo::new();
        let mut header = CommandListHeader::default();
        let estimator = CommandProcessingTimeEstimator::new(&behavior, 160, 6);
        let mut command_buffer = CommandBuffer::new(4096, estimator);
        let mut voice_context = VoiceContext::new();
        let mut mix_context = MixContext::new();
        let mut effect_context = EffectContext::new();
        let mut sink_context = SinkContext::default();
        let mut splitter_context = SplitterContext::new();
        let mut upsampler_manager = UpsamplerManager::new(1);
        let depop_buffer = [0i32; MAX_MIX_BUFFERS as usize];
        let depop_buffer_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let voice_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let effect_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let effect_result_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);

        let mut generator = make_generator(
            &mut command_buffer,
            &mut header,
            &behavior,
            &mut voice_context,
            &mut mix_context,
            &mut effect_context,
            &mut sink_context,
            &mut splitter_context,
            &mut upsampler_manager,
            &depop_buffer,
            &depop_buffer_pool,
            &voice_state_pool,
            &effect_state_pool,
            &effect_result_state_pool,
        );

        let mut mix = MixInfo::new(0, &behavior);
        mix.in_use = true;
        mix.mix_id = 0;
        mix.node_id = 7;
        mix.dst_mix_id = 1;
        mix.buffer_offset = -1;
        mix.buffer_count = 2;

        generator.generate_submix_command(&mix);
        generator.generate_final_mix_command(&mix);
        generator.generate_mix_commands(&mix);

        assert!(generator.command_buffer.entries().is_empty());
    }

    #[test]
    fn invalid_negative_circular_sink_position_does_not_emit_sink_command() {
        let behavior = BehaviorInfo::new();
        let mut header = CommandListHeader::default();
        let estimator = CommandProcessingTimeEstimator::new(&behavior, 160, 6);
        let mut command_buffer = CommandBuffer::new(4096, estimator);
        let mut voice_context = VoiceContext::new();
        let mut mix_context = MixContext::new();
        let mut effect_context = EffectContext::new();
        let mut sink_context = SinkContext::default();
        let mut splitter_context = SplitterContext::new();
        let mut upsampler_manager = UpsamplerManager::new(1);
        let depop_buffer = [0i32; MAX_MIX_BUFFERS as usize];
        let depop_buffer_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let voice_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let effect_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let effect_result_state_pool = MemoryPoolInfo::new(PoolLocation::Dsp);

        let mut generator = make_generator(
            &mut command_buffer,
            &mut header,
            &behavior,
            &mut voice_context,
            &mut mix_context,
            &mut effect_context,
            &mut sink_context,
            &mut splitter_context,
            &mut upsampler_manager,
            &depop_buffer,
            &depop_buffer_pool,
            &voice_state_pool,
            &effect_state_pool,
            &effect_result_state_pool,
        );

        let mut sink_info = SinkInfoBase {
            sink_type: SinkType::CircularBufferSink,
            in_use: true,
            node_id: 0x22,
            circular_state: crate::renderer::sink::CircularBufferState {
                current_pos: -1,
                ..Default::default()
            },
            ..Default::default()
        };

        generator.generate_sink_command(0, &mut sink_info);

        assert!(generator.command_buffer.entries().is_empty());
    }
}
