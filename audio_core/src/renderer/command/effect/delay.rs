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

#[derive(Debug, Clone)]
#[repr(C)]
pub struct DelayState {
    pub unk_000: [i32; 8],
    pub delay_lines: [DelayLine; MAX_CHANNELS as usize],
    pub feedback_gain: f32,
    pub delay_feedback_gain: f32,
    pub delay_feedback_cross_gain: f32,
    pub lowpass_gain: f32,
    pub lowpass_feedback_gain: f32,
    pub lowpass_z: [f32; MAX_CHANNELS as usize],
}

const _: () = assert!(std::mem::size_of::<DelayState>() <= 0x500);

#[derive(Debug, Clone)]
#[repr(C)]
pub struct DelayLine {
    pub sample_count_max: i32,
    pub sample_count: i32,
    pub buffer: Vec<f32>,
    pub buffer_pos: u32,
    pub decay_rate: f32,
}

impl DelayLine {
    fn read(&self) -> f32 {
        if self.buffer.is_empty() {
            return 0.0;
        }
        self.buffer[self.buffer_pos as usize]
    }

    fn write(&mut self, value: f32) {
        if self.buffer.is_empty() {
            self.buffer.push(0.0);
            self.sample_count = 1;
            self.buffer_pos = 0;
        }
        self.buffer[self.buffer_pos as usize] = value;
        self.buffer_pos = (self.buffer_pos + 1) % self.buffer.len() as u32;
    }
}

impl Default for DelayLine {
    fn default() -> Self {
        Self {
            sample_count_max: 0,
            sample_count: 0,
            buffer: Vec::new(),
            buffer_pos: 0,
            decay_rate: 0.0,
        }
    }
}

impl Default for DelayState {
    fn default() -> Self {
        Self {
            unk_000: [0; 8],
            delay_lines: std::array::from_fn(|_| DelayLine::default()),
            feedback_gain: 0.0,
            delay_feedback_gain: 0.0,
            delay_feedback_cross_gain: 0.0,
            lowpass_gain: 1.0,
            lowpass_feedback_gain: 0.0,
            lowpass_z: [0.0; MAX_CHANNELS as usize],
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
    _workbuffer_addr: CpuAddr,
) {
    // The state pointer references EffectInfoBase's raw state storage. Build
    // Vec-backed delay lines in place; upstream's workbuffer parameter is unused.
    unsafe { std::ptr::write(state, DelayState::default()) };

    let channel_count = parameter.channel_count.max(0) as usize;
    let sample_rate = fixed_14_to_f32(parameter.sample_rate).floor().max(0.0);
    let sample_count_max =
        (0.064f32 * sample_rate * parameter.delay_time_max.max(0) as f32).floor() as u32;
    let delay_samples =
        (((parameter.delay_time.max(0) as f32) * sample_rate) / 1000.0).floor() as u32;

    for channel in 0..channel_count.min(MAX_CHANNELS as usize) {
        let channel_samples = delay_samples.min(sample_count_max);
        let buffer_len = channel_samples.max(1) as usize;
        state.delay_lines[channel].sample_count_max = sample_count_max as i32;
        state.delay_lines[channel].sample_count = channel_samples as i32;
        state.delay_lines[channel].buffer = vec![0.0; buffer_len];
        state.delay_lines[channel].buffer_pos = 0;
        state.delay_lines[channel].decay_rate = 1.0;
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
    _workbuffer_addr: CpuAddr,
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

    let in_gain = fixed_14_to_f32(parameter.in_gain);
    let wet_gain = fixed_14_to_f32(parameter.wet_gain);
    let dry_gain = fixed_14_to_f32(parameter.dry_gain);

    for sample_index in 0..sample_count {
        let mut input_samples = [0.0f32; MAX_CHANNELS as usize];
        let mut delay_samples = [0.0f32; MAX_CHANNELS as usize];

        for channel in 0..active_channels {
            input_samples[channel] =
                mix_buffer_sample(mix_buffers, inputs[channel], sample_count, sample_index) as f32;

            delay_samples[channel] = state.delay_lines[channel].read();
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
            state.delay_lines[channel].write(state.lowpass_z[channel]);
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
    crate::raw_write_trace::maybe_trace_write_at(
        "delay:state_mut",
        addr,
        std::mem::size_of::<DelayState>(),
    );
    Some(unsafe { &mut *(addr as *mut DelayState) })
}

fn fixed_14_to_f32(value: i32) -> f32 {
    value as f32 / 16384.0
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
