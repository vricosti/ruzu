use crate::common::common::{
    get_sample_format_byte_size, CpuAddr, PlayState, SampleFormat, SrcQuality,
    HIGHEST_VOICE_PRIORITY, MAX_BIQUAD_FILTERS, MAX_CHANNELS, MAX_WAVE_BUFFERS, UNUSED_MIX_ID,
    UNUSED_SPLITTER_ID,
};
use crate::common::wave_buffer::{WaveBufferVersion1, WaveBufferVersion2};
use crate::errors::RESULT_INVALID_UPDATE_INFO;
use crate::renderer::behavior::{BehaviorInfo, ErrorInfo};
use crate::renderer::memory::{AddressInfo, PoolMapper};

use super::{VoiceContext, VoiceState};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ServerPlayState {
    Started,
    Stopped,
    RequestStop,
    Paused,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(transparent)]
pub struct Flags(pub u8);

impl Flags {
    pub fn is_voice_played_sample_count_reset_at_loop_point_supported(self) -> bool {
        self.0 & 0x1 != 0
    }

    pub fn is_voice_pitch_and_src_skipped_supported(self) -> bool {
        self.0 & 0x2 != 0
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct BiquadFilterParameter {
    pub enabled: bool,
    pub _padding: u8,
    pub b: [i16; 3],
    pub a: [i16; 2],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct WaveBufferInternal {
    pub address: CpuAddr,
    pub size: u64,
    pub start_offset: i32,
    pub end_offset: i32,
    pub looping: bool,
    pub stream_ended: bool,
    pub sent_to_dsp: bool,
    pub _padding1: u8,
    pub loop_count: i32,
    pub context_address: CpuAddr,
    pub context_size: u64,
    pub loop_start: u32,
    pub loop_end: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct InParameter {
    pub id: u32,
    pub node_id: u32,
    pub is_new: bool,
    pub in_use: bool,
    pub play_state: PlayState,
    pub sample_format: SampleFormat,
    pub sample_rate: u32,
    pub priority: i32,
    pub sort_order: i32,
    pub channel_count: u32,
    pub pitch: f32,
    pub volume: f32,
    pub biquads: [BiquadFilterParameter; MAX_BIQUAD_FILTERS as usize],
    pub wave_buffer_count: u32,
    pub wave_buffer_index: u16,
    pub _unk042: [u8; 0x6],
    pub src_data_address: CpuAddr,
    pub src_data_size: u64,
    pub mix_id: u32,
    pub splitter_id: u32,
    pub wave_buffer_internal: [WaveBufferInternal; MAX_WAVE_BUFFERS as usize],
    pub channel_resource_ids: [u32; MAX_CHANNELS],
    pub clear_voice_drop: bool,
    pub flush_buffer_count: u8,
    pub _unk15a: [u8; 0x2],
    pub flags: Flags,
    pub _unk15d: u8,
    pub src_quality: SrcQuality,
    pub _unk15f: [u8; 0x11],
}

const _: () = assert!(size_of::<WaveBufferInternal>() == 0x38);
const _: () = assert!(size_of::<BiquadFilterParameter>() == 0x0c);
const _: () = assert!(size_of::<InParameter>() == 0x170);
const _: () = assert!(size_of::<OutStatus>() == 0x10);

impl Default for InParameter {
    fn default() -> Self {
        Self {
            id: 0,
            node_id: 0,
            is_new: false,
            in_use: false,
            play_state: PlayState::Stopped,
            sample_format: SampleFormat::Invalid,
            sample_rate: 0,
            priority: 0,
            sort_order: 0,
            channel_count: 0,
            pitch: 0.0,
            volume: 0.0,
            biquads: [BiquadFilterParameter::default(); MAX_BIQUAD_FILTERS as usize],
            wave_buffer_count: 0,
            wave_buffer_index: 0,
            _unk042: [0; 0x6],
            src_data_address: 0,
            src_data_size: 0,
            mix_id: 0,
            splitter_id: 0,
            wave_buffer_internal: [WaveBufferInternal::default(); MAX_WAVE_BUFFERS as usize],
            channel_resource_ids: [0; MAX_CHANNELS],
            clear_voice_drop: false,
            flush_buffer_count: 0,
            _unk15a: [0; 0x2],
            flags: Flags::default(),
            _unk15d: 0,
            src_quality: SrcQuality::Medium,
            _unk15f: [0; 0x11],
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct OutStatus {
    pub played_sample_count: u64,
    pub wave_buffers_consumed: u32,
    pub voice_dropped: bool,
    pub _padding: [u8; 3],
}

#[derive(Debug, Clone, Default)]
pub struct WaveBuffer {
    pub buffer_address: AddressInfo,
    pub context_address: AddressInfo,
    pub start_offset: u32,
    pub end_offset: u32,
    pub looping: bool,
    pub stream_ended: bool,
    pub sent_to_dsp: bool,
    pub loop_start_offset: u32,
    pub loop_end_offset: u32,
    pub loop_count: i32,
}

impl WaveBuffer {
    pub fn initialize(&mut self) {
        self.buffer_address.setup(0, 0);
        self.context_address.setup(0, 0);
        self.start_offset = 0;
        self.end_offset = 0;
        self.looping = false;
        self.stream_ended = false;
        self.sent_to_dsp = true;
        self.loop_start_offset = 0;
        self.loop_end_offset = 0;
        self.loop_count = 0;
    }

    pub fn copy_to_v1(&mut self, other: &mut WaveBufferVersion1) {
        other.buffer = self.buffer_address.get_reference(true);
        other.buffer_size = self.buffer_address.get_size();
        other.start_offset = self.start_offset;
        other.end_offset = self.end_offset;
        other.looping = self.looping;
        other.stream_ended = self.stream_ended;
        if self.context_address.get_cpu_addr() != 0 {
            other.context = self.context_address.get_reference(true);
            other.context_size = self.context_address.get_size();
        } else {
            other.context = 0;
            other.context_size = 0;
        }
    }

    pub fn copy_to_v2(&mut self, other: &mut WaveBufferVersion2) {
        other.buffer = self.buffer_address.get_reference(true);
        other.buffer_size = self.buffer_address.get_size();
        other.start_offset = self.start_offset;
        other.end_offset = self.end_offset;
        other.loop_start_offset = self.loop_start_offset;
        other.loop_end_offset = self.loop_end_offset;
        other.loop_count = self.loop_count;
        other.looping = self.looping;
        other.stream_ended = self.stream_ended;
        if self.context_address.get_cpu_addr() != 0 {
            other.context = self.context_address.get_reference(true);
            other.context_size = self.context_address.get_size();
        } else {
            other.context = 0;
            other.context_size = 0;
        }
    }
}

#[derive(Debug, Clone)]
pub struct VoiceInfo {
    pub in_use: bool,
    pub is_new: bool,
    pub was_playing: bool,
    pub sample_format: SampleFormat,
    pub sample_rate: u32,
    pub channel_count: i8,
    pub id: u32,
    pub node_id: u32,
    pub mix_id: i32,
    pub current_play_state: ServerPlayState,
    pub last_play_state: ServerPlayState,
    pub priority: i32,
    pub sort_order: i32,
    pub pitch: f32,
    pub volume: f32,
    pub prev_volume: f32,
    pub biquads: [BiquadFilterParameter; MAX_BIQUAD_FILTERS as usize],
    pub wave_buffer_count: u32,
    pub wave_buffer_index: u16,
    pub flags: u16,
    pub data_address: AddressInfo,
    pub wavebuffers: [WaveBuffer; MAX_WAVE_BUFFERS as usize],
    pub channel_resource_ids: [u32; MAX_CHANNELS],
    pub splitter_id: i32,
    pub src_quality: SrcQuality,
    pub voice_dropped: bool,
    pub data_unmapped: bool,
    pub buffer_unmapped: bool,
    pub biquad_initialized: [bool; MAX_BIQUAD_FILTERS as usize],
    pub flush_buffer_count: u8,
}

impl Default for VoiceInfo {
    fn default() -> Self {
        let mut out = Self {
            in_use: false,
            is_new: false,
            was_playing: false,
            sample_format: SampleFormat::Invalid,
            sample_rate: 0,
            channel_count: 0,
            id: 0,
            node_id: 0,
            mix_id: UNUSED_MIX_ID,
            current_play_state: ServerPlayState::Stopped,
            last_play_state: ServerPlayState::Started,
            priority: HIGHEST_VOICE_PRIORITY,
            sort_order: 0,
            pitch: 0.0,
            volume: 0.0,
            prev_volume: 0.0,
            biquads: [BiquadFilterParameter::default(); MAX_BIQUAD_FILTERS as usize],
            wave_buffer_count: 0,
            wave_buffer_index: 0,
            flags: 0,
            data_address: AddressInfo::default(),
            wavebuffers: std::array::from_fn(|_| WaveBuffer::default()),
            channel_resource_ids: [0; MAX_CHANNELS],
            splitter_id: UNUSED_SPLITTER_ID,
            src_quality: SrcQuality::Medium,
            voice_dropped: false,
            data_unmapped: false,
            buffer_unmapped: false,
            biquad_initialized: [false; MAX_BIQUAD_FILTERS as usize],
            flush_buffer_count: 0,
        };
        out.initialize();
        out
    }
}

impl VoiceInfo {
    fn checked_channel_count(channel_count: i8) -> Option<usize> {
        usize::try_from(channel_count).ok()
    }

    pub fn initialize(&mut self) {
        self.in_use = false;
        self.is_new = false;
        self.id = 0;
        self.node_id = 0;
        self.current_play_state = ServerPlayState::Stopped;
        self.last_play_state = ServerPlayState::Started;
        self.src_quality = SrcQuality::Medium;
        self.priority = HIGHEST_VOICE_PRIORITY;
        self.sample_format = SampleFormat::Invalid;
        self.sample_rate = 0;
        self.channel_count = 0;
        self.wave_buffer_count = 0;
        self.wave_buffer_index = 0;
        self.pitch = 0.0;
        self.volume = 0.0;
        self.prev_volume = 0.0;
        self.mix_id = UNUSED_MIX_ID;
        self.splitter_id = UNUSED_SPLITTER_ID;
        self.biquads = [BiquadFilterParameter::default(); MAX_BIQUAD_FILTERS as usize];
        self.biquad_initialized = [false; MAX_BIQUAD_FILTERS as usize];
        self.voice_dropped = false;
        self.data_unmapped = false;
        self.buffer_unmapped = false;
        self.flush_buffer_count = 0;
        self.flags = 0;
        self.data_address.setup(0, 0);
        for wavebuffer in &mut self.wavebuffers {
            wavebuffer.initialize();
        }
    }

    pub fn should_update_parameters(&self, params: &InParameter) -> bool {
        self.data_address.get_cpu_addr() != params.src_data_address
            || self.data_address.get_size() != params.src_data_size
            || self.data_unmapped
    }

    pub fn update_parameters(
        &mut self,
        error_info: &mut ErrorInfo,
        params: &InParameter,
        pool_mapper: &PoolMapper<'_>,
        behavior: &BehaviorInfo,
    ) {
        self.in_use = params.in_use;
        self.id = params.id;
        self.node_id = params.node_id;
        self.update_play_state(params.play_state);
        self.update_src_quality(params.src_quality);
        self.priority = params.priority;
        self.sort_order = params.sort_order;
        self.sample_rate = params.sample_rate;
        self.sample_format = params.sample_format;
        self.channel_count = params.channel_count.min(MAX_CHANNELS as u32) as i8;
        self.pitch = params.pitch;
        self.volume = params.volume;
        self.biquads = params.biquads;
        self.wave_buffer_count = params.wave_buffer_count;
        self.wave_buffer_index = params.wave_buffer_index;

        if behavior.is_flush_voice_wave_buffers_supported() {
            self.flush_buffer_count = self
                .flush_buffer_count
                .saturating_add(params.flush_buffer_count);
        }

        self.mix_id = params.mix_id as i32;
        self.splitter_id = if behavior.is_splitter_supported() {
            params.splitter_id as i32
        } else {
            UNUSED_SPLITTER_ID
        };
        self.channel_resource_ids = params.channel_resource_ids;

        self.flags &= !0b11;
        if behavior.is_voice_played_sample_count_reset_at_loop_point_supported() {
            self.flags |= u16::from(
                params
                    .flags
                    .is_voice_played_sample_count_reset_at_loop_point_supported(),
            );
        }
        if behavior.is_voice_pitch_and_src_skipped_supported() {
            self.flags |= u16::from(params.flags.is_voice_pitch_and_src_skipped_supported()) << 1;
        }
        if params.clear_voice_drop {
            self.voice_dropped = false;
        }

        if self.should_update_parameters(params) {
            self.data_unmapped = !pool_mapper.try_attach_buffer(
                error_info,
                &mut self.data_address,
                params.src_data_address,
                params.src_data_size,
            );
        } else {
            error_info.error_code = common::ResultCode::SUCCESS;
            error_info.address = 0;
        }
    }

    pub fn update_play_state(&mut self, state: PlayState) {
        self.last_play_state = self.current_play_state;
        self.current_play_state = match state {
            PlayState::Started => ServerPlayState::Started,
            PlayState::Stopped => {
                if self.current_play_state != ServerPlayState::Stopped {
                    ServerPlayState::RequestStop
                } else {
                    ServerPlayState::Stopped
                }
            }
            PlayState::Paused => ServerPlayState::Paused,
        };
    }

    pub fn update_src_quality(&mut self, quality: SrcQuality) {
        self.src_quality = quality;
    }

    pub fn update_wave_buffers(
        &mut self,
        error_infos: &mut [[ErrorInfo; 2]],
        params: &InParameter,
        voice_states: &mut [VoiceState],
        pool_mapper: &PoolMapper<'_>,
        behavior: &BehaviorInfo,
    ) {
        if params.is_new {
            for wavebuffer in &mut self.wavebuffers {
                wavebuffer.initialize();
            }
            for state in voice_states
                .iter_mut()
                .take(params.channel_count.min(MAX_CHANNELS as u32) as usize)
            {
                state.wave_buffer_valid.fill(false);
            }
        }

        let valid_state = voice_states.first().copied().unwrap_or_default();
        for i in 0..MAX_WAVE_BUFFERS as usize {
            let valid = valid_state.wave_buffer_valid[i];
            self.update_wave_buffer(
                &mut error_infos[i],
                i,
                &params.wave_buffer_internal[i],
                params.sample_format,
                valid,
                pool_mapper,
                behavior,
            );
        }
    }

    pub fn update_wave_buffer(
        &mut self,
        error_info: &mut [ErrorInfo; 2],
        index: usize,
        wave_buffer_internal: &WaveBufferInternal,
        sample_format: SampleFormat,
        valid: bool,
        pool_mapper: &PoolMapper<'_>,
        behavior: &BehaviorInfo,
    ) {
        let should_update = self.should_update_wave_buffer(wave_buffer_internal);
        let wave_buffer = &mut self.wavebuffers[index];

        if !valid && wave_buffer.sent_to_dsp && wave_buffer.buffer_address.get_cpu_addr() != 0 {
            pool_mapper.force_unmap_pointer(&wave_buffer.buffer_address);
            wave_buffer.buffer_address.setup(0, 0);
        }

        if !should_update {
            return;
        }

        let invalid_offset =
            wave_buffer_internal.start_offset < 0 || wave_buffer_internal.end_offset < 0;
        if invalid_offset {
            error_info[0].error_code = RESULT_INVALID_UPDATE_INFO;
            error_info[0].address = wave_buffer_internal.address;
            return;
        }

        match sample_format {
            SampleFormat::PcmInt16 | SampleFormat::PcmFloat => {
                let byte_size = get_sample_format_byte_size(sample_format) as i64;
                let start = wave_buffer_internal.start_offset as i64 * byte_size;
                let end = wave_buffer_internal.end_offset as i64 * byte_size;
                if start > wave_buffer_internal.size as i64
                    || end > wave_buffer_internal.size as i64
                {
                    error_info[0].error_code = RESULT_INVALID_UPDATE_INFO;
                    error_info[0].address = wave_buffer_internal.address;
                    return;
                }
            }
            SampleFormat::Adpcm => {
                let start_frame = wave_buffer_internal.start_offset / 14;
                let start_extra = if wave_buffer_internal.start_offset % 14 == 0 {
                    0
                } else {
                    (wave_buffer_internal.start_offset % 14) / 2
                        + 1
                        + ((wave_buffer_internal.start_offset % 14) % 2)
                };
                let end_frame = wave_buffer_internal.end_offset / 14;
                let end_extra = if wave_buffer_internal.end_offset % 14 == 0 {
                    0
                } else {
                    (wave_buffer_internal.end_offset % 14) / 2
                        + 1
                        + ((wave_buffer_internal.end_offset % 14) % 2)
                };
                let start = start_frame * 8 + start_extra;
                let end = end_frame * 8 + end_extra;
                if start as u64 > wave_buffer_internal.size
                    || end as u64 > wave_buffer_internal.size
                {
                    error_info[0].error_code = RESULT_INVALID_UPDATE_INFO;
                    error_info[0].address = wave_buffer_internal.address;
                    return;
                }
            }
            _ => {}
        }

        wave_buffer.start_offset = wave_buffer_internal.start_offset as u32;
        wave_buffer.end_offset = wave_buffer_internal.end_offset as u32;
        wave_buffer.looping = wave_buffer_internal.looping;
        wave_buffer.stream_ended = wave_buffer_internal.stream_ended;
        wave_buffer.sent_to_dsp = false;
        wave_buffer.loop_start_offset = wave_buffer_internal.loop_start;
        wave_buffer.loop_end_offset = wave_buffer_internal.loop_end;
        wave_buffer.loop_count = wave_buffer_internal.loop_count;

        self.buffer_unmapped = !pool_mapper.try_attach_buffer(
            &mut error_info[0],
            &mut wave_buffer.buffer_address,
            wave_buffer_internal.address,
            wave_buffer_internal.size,
        );

        if sample_format == SampleFormat::Adpcm
            && behavior.is_adpcm_loop_context_bug_fixed()
            && wave_buffer_internal.context_address != 0
        {
            self.buffer_unmapped = !pool_mapper.try_attach_buffer(
                &mut error_info[1],
                &mut wave_buffer.context_address,
                wave_buffer_internal.context_address,
                wave_buffer_internal.context_size,
            ) || self.data_unmapped;
        } else {
            wave_buffer.context_address.setup(0, 0);
        }
    }

    pub fn should_update_wave_buffer(&self, wave_buffer_internal: &WaveBufferInternal) -> bool {
        !wave_buffer_internal.sent_to_dsp || self.buffer_unmapped
    }

    pub fn write_out_status(
        &mut self,
        out_status: &mut OutStatus,
        in_params: &InParameter,
        voice_states: &[VoiceState],
    ) {
        if in_params.is_new {
            self.is_new = true;
        }

        if in_params.is_new || self.is_new {
            out_status.played_sample_count = 0;
            out_status.wave_buffers_consumed = 0;
            out_status.voice_dropped = false;
        } else if let Some(state) = voice_states.first() {
            out_status.played_sample_count = state.played_sample_count;
            out_status.wave_buffers_consumed = state.wave_buffers_consumed;
            out_status.voice_dropped = self.voice_dropped;
        }
    }

    pub fn should_skip(&self) -> bool {
        !self.in_use
            || self.wave_buffer_count == 0
            || self.data_unmapped
            || self.buffer_unmapped
            || self.voice_dropped
    }

    pub fn has_any_connection(&self) -> bool {
        self.mix_id != UNUSED_MIX_ID || self.splitter_id != UNUSED_SPLITTER_ID
    }

    pub fn flush_wave_buffers(
        &mut self,
        flush_count: u32,
        voice_states: &mut [VoiceState],
        channel_count: i8,
    ) {
        let Some(channel_count) = Self::checked_channel_count(channel_count) else {
            return;
        };
        let mut wave_index = self.wave_buffer_index as usize;
        for _ in 0..flush_count as usize {
            self.wavebuffers[wave_index].sent_to_dsp = true;
            for state in voice_states.iter_mut().take(channel_count) {
                if state.wave_buffer_index as usize == wave_index {
                    state.wave_buffer_index =
                        ((state.wave_buffer_index + 1) % MAX_WAVE_BUFFERS) as u32;
                    state.wave_buffers_consumed += 1;
                }
                state.wave_buffer_valid[wave_index] = false;
            }
            wave_index = (wave_index + 1) % MAX_WAVE_BUFFERS as usize;
        }
    }

    pub fn update_parameters_for_command_generation(
        &mut self,
        voice_states: &mut [VoiceState],
    ) -> bool {
        if self.flush_buffer_count > 0 {
            self.flush_wave_buffers(
                self.flush_buffer_count as u32,
                voice_states,
                self.channel_count,
            );
            self.flush_buffer_count = 0;
        }

        match self.current_play_state {
            ServerPlayState::Started => {
                for i in 0..MAX_WAVE_BUFFERS as usize {
                    if !self.wavebuffers[i].sent_to_dsp {
                        let Some(channel_count) = Self::checked_channel_count(self.channel_count)
                        else {
                            return false;
                        };
                        for state in voice_states.iter_mut().take(channel_count) {
                            state.wave_buffer_valid[i] = true;
                        }
                        self.wavebuffers[i].sent_to_dsp = true;
                    }
                }

                self.was_playing = false;
                if let Some(state) = voice_states.first() {
                    for valid in state.wave_buffer_valid {
                        if valid {
                            return true;
                        }
                    }
                }
            }
            ServerPlayState::Stopped | ServerPlayState::Paused => {
                for wavebuffer in &mut self.wavebuffers {
                    if !wavebuffer.sent_to_dsp {
                        let _ = wavebuffer.buffer_address.get_reference(true);
                        let _ = wavebuffer.context_address.get_reference(true);
                    }
                }
                if self.sample_format == SampleFormat::Adpcm
                    && self.data_address.get_cpu_addr() != 0
                {
                    let _ = self.data_address.get_reference(true);
                }
                self.was_playing = self.last_play_state == ServerPlayState::Started;
            }
            ServerPlayState::RequestStop => {
                let Some(channel_count) = Self::checked_channel_count(self.channel_count) else {
                    return false;
                };
                for i in 0..MAX_WAVE_BUFFERS as usize {
                    self.wavebuffers[i].sent_to_dsp = true;
                    for state in voice_states.iter_mut().take(channel_count) {
                        if state.wave_buffer_valid[i] {
                            state.wave_buffer_index =
                                ((state.wave_buffer_index + 1) % MAX_WAVE_BUFFERS) as u32;
                            state.wave_buffers_consumed += 1;
                        }
                        state.wave_buffer_valid[i] = false;
                    }
                }
                for state in voice_states.iter_mut().take(channel_count) {
                    state.offset = 0;
                    state.played_sample_count = 0;
                    state.adpcm_context = Default::default();
                    state.sample_history.fill(0);
                    state.fraction = Default::default();
                }
                self.current_play_state = ServerPlayState::Stopped;
                self.was_playing = self.last_play_state == ServerPlayState::Started;
            }
        }

        self.was_playing
    }

    pub fn update_for_command_generation(&mut self, voice_context: &mut VoiceContext) -> bool {
        if self.is_new {
            self.reset_resources(voice_context);
            self.prev_volume = self.volume;
            self.is_new = false;
        }

        let Some(channel_count) = Self::checked_channel_count(self.channel_count) else {
            return false;
        };

        let mut states = Vec::with_capacity(channel_count);
        let mut indices = Vec::with_capacity(channel_count);
        for channel in 0..channel_count {
            let index = self.channel_resource_ids[channel];
            let state = voice_context
                .get_dsp_shared_state(index)
                .copied()
                .unwrap_or_default();
            indices.push(index);
            states.push(state);
        }

        let result = self.update_parameters_for_command_generation(&mut states);

        for (slot, state) in indices.into_iter().zip(states.into_iter()) {
            if let Some(target) = voice_context.get_dsp_shared_state(slot) {
                *target = state;
            }
        }

        result
    }

    pub fn reset_resources(&self, voice_context: &mut VoiceContext) {
        let Some(channel_count) = Self::checked_channel_count(self.channel_count) else {
            return;
        };
        for channel in 0..channel_count {
            let state_index = self.channel_resource_ids[channel];
            if let Some(state) = voice_context.get_dsp_shared_state(state_index) {
                *state = VoiceState::default();
            }
            if let Some(channel_resource) = voice_context.get_channel_resource(state_index) {
                channel_resource.prev_mix_volumes = channel_resource.mix_volumes;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn invalid_negative_channel_count_does_not_generate_commands() {
        let mut voice = VoiceInfo::default();
        let mut context = VoiceContext::new();
        context.initialize(1, 1, 1);
        voice.in_use = true;
        voice.channel_count = -1;

        assert!(!voice.update_for_command_generation(&mut context));
    }

    #[test]
    fn invalid_negative_channel_count_does_not_flush_wave_buffers() {
        let mut voice = VoiceInfo::default();
        let mut states = [VoiceState::default(); 1];
        voice.wavebuffers[0].sent_to_dsp = false;
        voice.wave_buffer_index = 0;

        voice.flush_wave_buffers(1, &mut states, -1);

        assert!(!voice.wavebuffers[0].sent_to_dsp);
        assert_eq!(states[0].wave_buffers_consumed, 0);
    }
}
