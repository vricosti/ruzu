use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::command::mix::copy_mix_buffer;
use crate::renderer::command::util::write_copy;
use crate::renderer::effect::effect_info_base::ParameterState;
use crate::renderer::effect::light_limiter;
use crate::renderer::effect::light_limiter::{ProcessingMode, StatisticsInternal};
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct LightLimiterVersion1Payload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: light_limiter::ParameterVersion1,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct LightLimiterVersion1Command {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: light_limiter::ParameterVersion1,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
}

impl LightLimiterVersion1Payload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_light_limiter_v1_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_light_limiter_v1_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_light_limiter_v1_command(self, dump);
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct LightLimiterVersion2Payload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: light_limiter::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub result_state: CpuAddr,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct LightLimiterVersion2Command {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: light_limiter::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub result_state: CpuAddr,
    pub effect_enabled: bool,
}

impl LightLimiterVersion2Payload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_light_limiter_v2_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_light_limiter_v2_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_light_limiter_v2_command(self, dump);
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct LightLimiterState {
    pub samples_average: [f32; MAX_CHANNELS as usize],
    pub compression_gain: [f32; MAX_CHANNELS as usize],
    pub look_ahead_sample_offsets: [u32; MAX_CHANNELS as usize],
    pub look_ahead_samples_max: u32,
    pub _padding0: [u8; 0x4f0],
}

impl Default for LightLimiterState {
    fn default() -> Self {
        Self {
            samples_average: [0.0; MAX_CHANNELS as usize],
            compression_gain: [1.0; MAX_CHANNELS as usize],
            look_ahead_sample_offsets: [0; MAX_CHANNELS as usize],
            look_ahead_samples_max: 0,
            _padding0: [0; 0x4f0],
        }
    }
}

pub fn write_light_limiter_v1_payload(
    cmd: &LightLimiterVersion1Command,
    output: &mut [u8],
) -> usize {
    let mut payload: LightLimiterVersion1Payload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.parameter = cmd.parameter;
    payload.state = cmd.state;
    payload.workbuffer = cmd.workbuffer;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn write_light_limiter_v2_payload(
    cmd: &LightLimiterVersion2Command,
    output: &mut [u8],
) -> usize {
    let mut payload: LightLimiterVersion2Payload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.parameter = cmd.parameter;
    payload.state = cmd.state;
    payload.workbuffer = cmd.workbuffer;
    payload.result_state = cmd.result_state;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn process_light_limiter_v1_command(
    payload: &LightLimiterVersion1Payload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let parameter = light_limiter_v1_to_v2(payload.parameter);
    process_light_limiter_command(
        &parameter,
        &payload.inputs,
        &payload.outputs,
        payload.state,
        payload.workbuffer,
        0,
        payload.effect_enabled,
        mix_buffers,
        sample_count,
    );
}

pub fn verify_light_limiter_v1_command(_payload: &LightLimiterVersion1Payload) -> bool {
    true
}

pub fn dump_light_limiter_v1_command(payload: &LightLimiterVersion1Payload, dump: &mut String) {
    let _ = write!(dump, "LightLimiterVersion1Command\n\tinputs: ");
    for input in &payload.inputs {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in &payload.outputs {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
}

pub fn process_light_limiter_v2_command(
    payload: &LightLimiterVersion2Payload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    process_light_limiter_command(
        &payload.parameter,
        &payload.inputs,
        &payload.outputs,
        payload.state,
        payload.workbuffer,
        payload.result_state,
        payload.effect_enabled,
        mix_buffers,
        sample_count,
    );
}

pub fn verify_light_limiter_v2_command(_payload: &LightLimiterVersion2Payload) -> bool {
    true
}

pub fn dump_light_limiter_v2_command(payload: &LightLimiterVersion2Payload, dump: &mut String) {
    let _ = write!(dump, "LightLimiterVersion2Command\n\tinputs: \n");
    for input in &payload.inputs {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in &payload.outputs {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
}

pub fn process_light_limiter_command(
    parameter: &light_limiter::ParameterVersion2,
    inputs: &[i16; MAX_CHANNELS as usize],
    outputs: &[i16; MAX_CHANNELS as usize],
    state_addr: CpuAddr,
    workbuffer_addr: CpuAddr,
    result_state_addr: CpuAddr,
    effect_enabled: bool,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let channel_count = parameter.channel_count.max(0) as usize;
    if channel_count == 0 {
        return;
    }

    let Some(state) = read_light_limiter_state_mut(state_addr) else {
        return;
    };

    if effect_enabled {
        match parameter.state {
            ParameterState::Updating => {
                update_light_limiter_effect_parameter(parameter, state);
            }
            ParameterState::Initialized => {
                initialize_light_limiter_effect(parameter, state, workbuffer_addr);
            }
            ParameterState::Updated => {}
        }
    }

    apply_light_limiter_effect(
        parameter,
        state,
        effect_enabled,
        inputs,
        outputs,
        mix_buffers,
        sample_count,
        workbuffer_addr,
        result_state_addr,
    );
}

pub fn light_limiter_v1_to_v2(
    parameter: light_limiter::ParameterVersion1,
) -> light_limiter::ParameterVersion2 {
    light_limiter::ParameterVersion2 {
        inputs: parameter.inputs,
        outputs: parameter.outputs,
        channel_count_max: parameter.channel_count_max,
        channel_count: parameter.channel_count,
        sample_rate: parameter.sample_rate,
        look_ahead_time_max: parameter.look_ahead_time_max,
        attack_time: parameter.attack_time,
        release_time: parameter.release_time,
        look_ahead_time: parameter.look_ahead_time,
        attack_coeff: parameter.attack_coeff,
        release_coeff: parameter.release_coeff,
        threshold: parameter.threshold,
        input_gain: parameter.input_gain,
        output_gain: parameter.output_gain,
        look_ahead_samples_min: parameter.look_ahead_samples_min,
        look_ahead_samples_max: parameter.look_ahead_samples_max,
        state: parameter.state,
        statistics_enabled: parameter.statistics_enabled,
        statistics_reset_required: parameter.statistics_reset_required,
        processing_mode: parameter.processing_mode,
    }
}

pub fn update_light_limiter_effect_parameter(
    _parameter: &light_limiter::ParameterVersion2,
    _state: &mut LightLimiterState,
) {
}

pub fn initialize_light_limiter_effect(
    parameter: &light_limiter::ParameterVersion2,
    state: &mut LightLimiterState,
    workbuffer_addr: CpuAddr,
) {
    *state = LightLimiterState::default();
    let channel_count = parameter.channel_count.max(0) as usize;
    state.look_ahead_samples_max = parameter.look_ahead_samples_max.max(0) as u32;
    if let Some(buffer) = read_f32_slice_mut(
        workbuffer_addr,
        channel_count.saturating_mul(state.look_ahead_samples_max as usize),
    ) {
        buffer.fill(0.0);
    }
}

pub fn apply_light_limiter_effect(
    parameter: &light_limiter::ParameterVersion2,
    state: &mut LightLimiterState,
    enabled: bool,
    inputs: &[i16; MAX_CHANNELS as usize],
    outputs: &[i16; MAX_CHANNELS as usize],
    mix_buffers: &mut [i32],
    sample_count: usize,
    workbuffer_addr: CpuAddr,
    result_state_addr: CpuAddr,
) {
    let channel_count = parameter.channel_count.max(0) as usize;
    let active_channels = channel_count.min(inputs.len()).min(outputs.len());
    if active_channels == 0 {
        return;
    }

    if !enabled {
        for channel in 0..active_channels {
            copy_mix_buffer(mix_buffers, sample_count, outputs[channel], inputs[channel]);
        }
        return;
    }

    let lookahead_len = parameter.look_ahead_samples_max.max(0) as usize;
    let mut lookahead_buffers = read_f32_slice_mut(
        workbuffer_addr,
        active_channels.saturating_mul(lookahead_len),
    );
    let mut statistics = if parameter.statistics_enabled {
        read_statistics_internal_mut(result_state_addr)
    } else {
        None
    };

    if let Some(stats) = statistics.as_deref_mut() {
        if parameter.statistics_reset_required {
            stats.channel_compression_gain_min = [1.0; MAX_CHANNELS as usize];
            stats.channel_max_sample = [0.0; MAX_CHANNELS as usize];
        }
    }

    for sample_index in 0..sample_count {
        for channel in 0..active_channels {
            let mut sample =
                mix_buffer_sample(mix_buffers, inputs[channel], sample_count, sample_index) as f32;
            sample *= parameter.input_gain;
            let abs_sample = sample.abs();
            let coeff = if abs_sample > state.samples_average[channel] {
                parameter.attack_coeff
            } else {
                parameter.release_coeff
            };
            state.samples_average[channel] += (abs_sample - state.samples_average[channel]) * coeff;

            let average = state.samples_average[channel];
            let mut new_average_sample = recip_estimate_f32(average.max(f32::MIN_POSITIVE));
            if !matches!(parameter.processing_mode, ProcessingMode::Mode1) {
                let temp = 2.0 - (average * new_average_sample);
                new_average_sample = 2.0 - (average * temp);
            }

            let attenuation = if average > parameter.threshold {
                parameter.threshold * new_average_sample
            } else {
                1.0
            };
            let coeff = if attenuation < state.compression_gain[channel] {
                parameter.attack_coeff
            } else {
                parameter.release_coeff
            };
            state.compression_gain[channel] +=
                (attenuation - state.compression_gain[channel]) * coeff;

            let lookahead_sample = if let Some(buffers) = lookahead_buffers.as_deref_mut() {
                if lookahead_len == 0 {
                    sample
                } else {
                    let channel_base = channel.saturating_mul(lookahead_len);
                    let offset = (state.look_ahead_sample_offsets[channel] as usize)
                        .min(lookahead_len.saturating_sub(1));
                    let previous = buffers[channel_base + offset];
                    buffers[channel_base + offset] = sample;
                    let modulo = parameter.look_ahead_samples_min.max(1) as u32;
                    state.look_ahead_sample_offsets[channel] =
                        (state.look_ahead_sample_offsets[channel] + 1) % modulo;
                    previous
                }
            } else {
                sample
            };

            let output_sample =
                lookahead_sample * state.compression_gain[channel] * parameter.output_gain;
            set_mix_buffer_sample(
                mix_buffers,
                outputs[channel],
                sample_count,
                sample_index,
                output_sample
                    .clamp(i32::MIN as f32, i32::MAX as f32)
                    .round() as i32,
            );

            if let Some(stats) = statistics.as_deref_mut() {
                stats.channel_max_sample[channel] =
                    stats.channel_max_sample[channel].max(abs_sample);
                stats.channel_compression_gain_min[channel] = stats.channel_compression_gain_min
                    [channel]
                    .min(state.compression_gain[channel]);
            }
        }
    }
}

fn read_light_limiter_state_mut(addr: CpuAddr) -> Option<&'static mut LightLimiterState> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { &mut *(addr as *mut LightLimiterState) })
}

fn read_statistics_internal_mut(addr: CpuAddr) -> Option<&'static mut StatisticsInternal> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { &mut *(addr as *mut StatisticsInternal) })
}

fn read_f32_slice_mut(addr: CpuAddr, len: usize) -> Option<&'static mut [f32]> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts_mut(addr as *mut f32, len) })
}

fn recip_estimate_f32(value: f32) -> f32 {
    let q = (value as f64 * 512.0).floor() as i32;
    let r = 1.0 / (((q as f64) + 0.5) / 512.0);
    let s = (256.0 * r + 0.5) as i32;
    (s as f64 / 256.0) as f32
}

fn mix_buffer_sample(
    mix_buffers: &[i32],
    buffer_index: i16,
    sample_count: usize,
    sample_index: usize,
) -> i32 {
    if buffer_index < 0 {
        return 0;
    }
    let buffer_index = buffer_index as usize;
    mix_buffers
        .get(buffer_index * sample_count + sample_index)
        .copied()
        .unwrap_or(0)
}

fn set_mix_buffer_sample(
    mix_buffers: &mut [i32],
    buffer_index: i16,
    sample_count: usize,
    sample_index: usize,
    value: i32,
) {
    if buffer_index < 0 {
        return;
    }
    let buffer_index = buffer_index as usize;
    if let Some(sample) = mix_buffers.get_mut(buffer_index * sample_count + sample_index) {
        *sample = value;
    }
}
