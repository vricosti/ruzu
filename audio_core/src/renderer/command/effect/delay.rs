use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::command::mix::copy_mix_buffer;
use crate::renderer::command::util::write_copy;
use crate::renderer::effect::delay;
use crate::renderer::effect::effect_info_base::ParameterState;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DelayPayload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: delay::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct DelayCommand {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: delay::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
}

impl DelayPayload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_delay_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_delay_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_delay_command(self, dump);
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DelayState {
    pub sample_count_max: [u32; MAX_CHANNELS as usize],
    pub sample_count: [u32; MAX_CHANNELS as usize],
    pub buffer_pos: [u32; MAX_CHANNELS as usize],
    pub feedback_gain: f32,
    pub delay_feedback_gain: f32,
    pub delay_feedback_cross_gain: f32,
    pub lowpass_gain: f32,
    pub lowpass_feedback_gain: f32,
    pub lowpass_z: [f32; MAX_CHANNELS as usize],
    pub _padding0: [u8; 0x490],
}

impl Default for DelayState {
    fn default() -> Self {
        Self {
            sample_count_max: [0; MAX_CHANNELS as usize],
            sample_count: [0; MAX_CHANNELS as usize],
            buffer_pos: [0; MAX_CHANNELS as usize],
            feedback_gain: 0.0,
            delay_feedback_gain: 0.0,
            delay_feedback_cross_gain: 0.0,
            lowpass_gain: 1.0,
            lowpass_feedback_gain: 0.0,
            lowpass_z: [0.0; MAX_CHANNELS as usize],
            _padding0: [0; 0x490],
        }
    }
}

pub fn write_delay_payload(cmd: &DelayCommand, output: &mut [u8]) -> usize {
    let mut payload: DelayPayload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.parameter = cmd.parameter;
    payload.state = cmd.state;
    payload.workbuffer = cmd.workbuffer;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn process_delay_command(payload: &DelayPayload, mix_buffers: &mut [i32], sample_count: usize) {
    let channel_count = payload.parameter.channel_count.max(0) as usize;
    if channel_count == 0 {
        return;
    }

    let Some(state) = read_delay_state_mut(payload.state) else {
        return;
    };
    if payload.effect_enabled {
        match payload.parameter.state {
            ParameterState::Updating => set_delay_effect_parameter(&payload.parameter, state),
            ParameterState::Initialized => {
                initialize_delay_effect(&payload.parameter, state, payload.workbuffer)
            }
            ParameterState::Updated => {}
        }
    }

    apply_delay_effect(
        &payload.parameter,
        state,
        payload.effect_enabled,
        &payload.inputs,
        &payload.outputs,
        mix_buffers,
        sample_count,
        payload.workbuffer,
    );
}

pub fn verify_delay_command(_payload: &DelayPayload) -> bool {
    true
}

pub fn dump_delay_command(payload: &DelayPayload, dump: &mut String) {
    let _ = write!(
        dump,
        "DelayCommand\n\tenabled {} \n\tinputs: ",
        payload.effect_enabled
    );
    for input in &payload.inputs {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in &payload.outputs {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
}

pub fn set_delay_effect_parameter(parameter: &delay::ParameterVersion2, state: &mut DelayState) {
    let mut channel_spread = fixed_14_to_f32(parameter.channel_spread);
    state.feedback_gain = fixed_14_to_f32(parameter.feedback_gain) * 0.97998047;
    state.delay_feedback_gain = state.feedback_gain * (1.0 - channel_spread);
    if parameter.channel_count == 4 || parameter.channel_count == 6 {
        channel_spread *= 0.5;
    }
    state.delay_feedback_cross_gain = channel_spread * state.feedback_gain;
    state.lowpass_feedback_gain = fixed_14_to_f32(parameter.lowpass_amount) * 0.9499512;
    state.lowpass_gain = 1.0 - state.lowpass_feedback_gain;
}

pub fn initialize_delay_effect(
    parameter: &delay::ParameterVersion2,
    state: &mut DelayState,
    workbuffer_addr: CpuAddr,
) {
    *state = DelayState::default();

    let channel_count = parameter.channel_count.max(0) as usize;
    let sample_rate = fixed_14_to_f32(parameter.sample_rate).floor().max(0.0);
    let sample_count_max =
        (0.064f32 * sample_rate * parameter.delay_time_max.max(0) as f32).floor() as u32;
    let delay_samples =
        (((parameter.delay_time.max(0) as f32) * sample_rate) / 1000.0).floor() as u32;

    let mut total_samples = 0usize;
    for channel in 0..channel_count.min(MAX_CHANNELS as usize) {
        let channel_samples = delay_samples.min(sample_count_max).max(1);
        state.sample_count_max[channel] = sample_count_max;
        state.sample_count[channel] = channel_samples;
        total_samples = total_samples.saturating_add(channel_samples as usize);
    }
    if let Some(workbuffer) = read_f32_slice_mut(workbuffer_addr, total_samples) {
        workbuffer.fill(0.0);
    }

    set_delay_effect_parameter(parameter, state);
}

pub fn apply_delay_effect(
    parameter: &delay::ParameterVersion2,
    state: &mut DelayState,
    enabled: bool,
    inputs: &[i16; MAX_CHANNELS as usize],
    outputs: &[i16; MAX_CHANNELS as usize],
    mix_buffers: &mut [i32],
    sample_count: usize,
    workbuffer_addr: CpuAddr,
) {
    let channel_count = parameter.channel_count.max(0) as usize;
    let active_channels = channel_count.min(inputs.len()).min(outputs.len());
    if active_channels == 0 {
        return;
    }

    if !enabled || !matches!(active_channels, 1 | 2 | 4 | 6) {
        for channel in 0..active_channels {
            copy_mix_buffer(mix_buffers, sample_count, outputs[channel], inputs[channel]);
        }
        return;
    }

    let total_delay_samples = (0..active_channels)
        .map(|channel| state.sample_count[channel].max(1) as usize)
        .sum();
    let Some(delay_buffer) = read_f32_slice_mut(workbuffer_addr, total_delay_samples) else {
        for channel in 0..active_channels {
            copy_mix_buffer(mix_buffers, sample_count, outputs[channel], inputs[channel]);
        }
        return;
    };
    let offsets = delay_workbuffer_offsets(state, active_channels);

    let in_gain = fixed_14_to_f32(parameter.in_gain);
    let wet_gain = fixed_14_to_f32(parameter.wet_gain);
    let dry_gain = fixed_14_to_f32(parameter.dry_gain);

    for sample_index in 0..sample_count {
        let mut input_samples = [0.0f32; MAX_CHANNELS as usize];
        let mut delay_samples = [0.0f32; MAX_CHANNELS as usize];

        for channel in 0..active_channels {
            input_samples[channel] =
                mix_buffer_sample(mix_buffers, inputs[channel], sample_count, sample_index) as f32;

            let channel_samples = state.sample_count[channel].max(1) as usize;
            let pos = (state.buffer_pos[channel] as usize) % channel_samples;
            delay_samples[channel] = delay_buffer[offsets[channel] + pos];
        }

        let mut gained_samples = [0.0f32; MAX_CHANNELS as usize];
        for dst in 0..active_channels {
            let mut delay = 0.0f32;
            for (src, &delay_sample) in delay_samples.iter().enumerate().take(active_channels) {
                delay += delay_sample * delay_matrix_value(state, active_channels, src, dst);
            }
            gained_samples[dst] = input_samples[dst] * in_gain + delay;
        }

        for channel in 0..active_channels {
            state.lowpass_z[channel] = gained_samples[channel] * state.lowpass_gain
                + state.lowpass_z[channel] * state.lowpass_feedback_gain;
            let channel_samples = state.sample_count[channel].max(1) as usize;
            let pos = (state.buffer_pos[channel] as usize) % channel_samples;
            delay_buffer[offsets[channel] + pos] = state.lowpass_z[channel];
            state.buffer_pos[channel] = ((pos + 1) % channel_samples) as u32;
        }

        for channel in 0..active_channels {
            let output_sample =
                input_samples[channel] * dry_gain + delay_samples[channel] * wet_gain;
            set_mix_buffer_sample(
                mix_buffers,
                outputs[channel],
                sample_count,
                sample_index,
                output_sample
                    .clamp(i32::MIN as f32, i32::MAX as f32)
                    .floor() as i32,
            );
        }
    }
}

fn read_delay_state_mut(addr: CpuAddr) -> Option<&'static mut DelayState> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { &mut *(addr as *mut DelayState) })
}

fn read_f32_slice_mut(addr: CpuAddr, len: usize) -> Option<&'static mut [f32]> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts_mut(addr as *mut f32, len) })
}

fn fixed_14_to_f32(value: i32) -> f32 {
    value as f32 / 16384.0
}

fn delay_workbuffer_offsets(
    state: &DelayState,
    channel_count: usize,
) -> [usize; MAX_CHANNELS as usize] {
    let mut offsets = [0usize; MAX_CHANNELS as usize];
    let mut current = 0usize;
    for channel in 0..channel_count.min(MAX_CHANNELS as usize) {
        offsets[channel] = current;
        current = current.saturating_add(state.sample_count[channel].max(1) as usize);
    }
    offsets
}

fn delay_matrix_value(state: &DelayState, channel_count: usize, src: usize, dst: usize) -> f32 {
    match channel_count {
        1 => state.feedback_gain,
        2 => {
            if src == dst {
                state.delay_feedback_gain
            } else {
                state.delay_feedback_cross_gain
            }
        }
        4 => match (src, dst) {
            (0, 0) | (1, 1) | (2, 2) | (3, 3) => state.delay_feedback_gain,
            (0, 1) | (0, 2) | (1, 0) | (1, 3) | (2, 0) | (2, 3) | (3, 1) | (3, 2) => {
                state.delay_feedback_cross_gain
            }
            _ => 0.0,
        },
        6 => match (src, dst) {
            (0, 0) | (1, 1) | (2, 2) | (4, 4) | (5, 5) => state.delay_feedback_gain,
            (3, 3) => state.feedback_gain,
            (0, 2)
            | (0, 4)
            | (1, 2)
            | (1, 5)
            | (2, 0)
            | (2, 1)
            | (4, 0)
            | (4, 5)
            | (5, 1)
            | (5, 4) => state.delay_feedback_cross_gain,
            _ => 0.0,
        },
        _ => 0.0,
    }
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
