use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::command::util::write_copy;
use crate::renderer::effect::effect_info_base::ParameterState;
use crate::renderer::effect::i3dl2;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct I3dl2ReverbPayload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: i3dl2::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct I3dl2ReverbCommand {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: i3dl2::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
}

impl I3dl2ReverbPayload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_i3dl2_reverb_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_i3dl2_reverb_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_i3dl2_reverb_command(self, dump);
    }
}

pub fn write_i3dl2_reverb_payload(cmd: &I3dl2ReverbCommand, output: &mut [u8]) -> usize {
    let mut payload: I3dl2ReverbPayload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.parameter = cmd.parameter;
    payload.state = cmd.state;
    payload.workbuffer = cmd.workbuffer;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn process_i3dl2_reverb_command(
    payload: &I3dl2ReverbPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let channel_count = payload.parameter.channel_count.max(0) as usize;
    if channel_count == 0 {
        return;
    }

    let Some(state) = read_i3dl2_reverb_state_mut(payload.state) else {
        return;
    };
    if payload.effect_enabled {
        match payload.parameter.state {
            ParameterState::Updating => {
                update_i3dl2_reverb_effect_parameter(&payload.parameter, state, false);
            }
            ParameterState::Initialized => {
                let total_samples = total_workbuffer_samples(state);
                let Some(workbuffer) = read_f32_slice_mut(payload.workbuffer, total_samples) else {
                    return;
                };
                initialize_i3dl2_reverb_effect(&payload.parameter, state, workbuffer);
            }
            ParameterState::Updated => {}
        }
    }

    let total_samples = total_workbuffer_samples(state);
    let mut empty_workbuffer = [];
    let workbuffer =
        read_f32_slice_mut(payload.workbuffer, total_samples).unwrap_or(&mut empty_workbuffer);
    apply_i3dl2_reverb_effect(
        &payload.parameter,
        state,
        payload.effect_enabled,
        &payload.inputs,
        &payload.outputs,
        mix_buffers,
        sample_count,
        workbuffer,
    );
}

pub fn verify_i3dl2_reverb_command(_payload: &I3dl2ReverbPayload) -> bool {
    true
}

pub fn dump_i3dl2_reverb_command(payload: &I3dl2ReverbPayload, dump: &mut String) {
    let channel_count = payload.parameter.channel_count.max(0) as usize;
    let _ = write!(
        dump,
        "I3dl2ReverbCommand\n\tenabled {} \n\tinputs: ",
        payload.effect_enabled
    );
    for input in payload.inputs.iter().take(channel_count) {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in payload.outputs.iter().take(channel_count) {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct I3dl2ReverbState {
    pub dry_gain: f32,
    pub early_gain: f32,
    pub late_gain: f32,
    pub lowpass_0: f32,
    pub lowpass_1: f32,
    pub lowpass_2: f32,
    pub early_to_late_taps: u32,
    pub last_reverb_echo: f32,
    pub early_tap_steps: [u32; 20],
    pub fdn_delay_samples: [u32; 4],
    pub decay0_delay_samples: [u32; 4],
    pub decay1_delay_samples: [u32; 4],
    pub fdn_positions: [u32; 4],
    pub decay0_positions: [u32; 4],
    pub decay1_positions: [u32; 4],
    pub lowpass_coeff: [[f32; 3]; 4],
    pub shelf_filter: [f32; 4],
    pub center_delay_pos: u32,
    pub center_delay_len: u32,
    pub early_delay_pos: u32,
    pub early_delay_len: u32,
    pub _padding0: [u8; 0x3e0],
}

impl Default for I3dl2ReverbState {
    fn default() -> Self {
        Self {
            dry_gain: 0.0,
            early_gain: 0.0,
            late_gain: 0.0,
            lowpass_0: 0.0,
            lowpass_1: 0.0,
            lowpass_2: 1.0,
            early_to_late_taps: 0,
            last_reverb_echo: 0.0,
            early_tap_steps: [0; 20],
            fdn_delay_samples: [0; 4],
            decay0_delay_samples: [0; 4],
            decay1_delay_samples: [0; 4],
            fdn_positions: [0; 4],
            decay0_positions: [0; 4],
            decay1_positions: [0; 4],
            lowpass_coeff: [[0.0; 3]; 4],
            shelf_filter: [0.0; 4],
            center_delay_pos: 0,
            center_delay_len: 0,
            early_delay_pos: 0,
            early_delay_len: 0,
            _padding0: [0; 0x3e0],
        }
    }
}

pub const MIN_DELAY_LINE_TIMES: [f32; 4] = [5.0, 6.0, 13.0, 14.0];
pub const MAX_DELAY_LINE_TIMES: [f32; 4] = [45.704_2, 82.781_7, 149.938_3, 271.575_8];
pub const DECAY0_MAX_DELAY_LINE_TIMES: [f32; 4] = [17.0, 13.0, 9.0, 7.0];
pub const DECAY1_MAX_DELAY_LINE_TIMES: [f32; 4] = [19.0, 11.0, 10.0, 6.0];
pub const EARLY_TAP_TIMES: [f32; 20] = [
    0.017_136, 0.059_154, 0.161_733, 0.390_186, 0.425_262, 0.455_411, 0.689_737, 0.745_91,
    0.833_844, 0.859_502, 0.0, 0.075_024, 0.168_788, 0.299_901, 0.337_443, 0.371_903, 0.599_011,
    0.716_741, 0.817_859, 0.851_664,
];
pub const EARLY_GAINS: [f32; 20] = [
    0.670_96, 0.610_27, 1.0, 0.3568, 0.683_61, 0.659_78, 0.519_39, 0.247_12, 0.459_45, 0.450_21,
    0.641_96, 0.548_79, 0.929_25, 0.3827, 0.728_67, 0.697_94, 0.5464, 0.245_63, 0.452_14, 0.440_42,
];

pub fn update_i3dl2_reverb_effect_parameter(
    parameter: &i3dl2::ParameterVersion2,
    state: &mut I3dl2ReverbState,
    reset: bool,
) {
    let pow10 = |value: f32| {
        if value >= 0.0 {
            1.0
        } else if value <= -5.3 {
            0.0
        } else {
            10.0f32.powf(value)
        }
    };

    let sr_ms = parameter.sample_rate.max(1) as f32 / 1000.0;
    state.dry_gain = parameter.dry_gain;
    state.early_gain =
        pow10((parameter.room_gain + parameter.reflection_gain).min(5000.0) / 2000.0);
    state.late_gain = pow10((parameter.room_gain + parameter.reverb_gain).min(5000.0) / 2000.0);

    let hf_gain = pow10(parameter.room_hf_gain / 2000.0);
    if hf_gain >= 1.0 {
        state.lowpass_1 = 0.0;
        state.lowpass_2 = 1.0;
    } else {
        let reference_hf = (parameter.reference_hf * 256.0) / parameter.sample_rate.max(1) as f32;
        let a = 1.0 - hf_gain;
        let b = 2.0 + (-reference_hf.cos() * (hf_gain * 2.0));
        let c = (b * b + (a * a * -4.0)).max(0.0).sqrt();
        state.lowpass_1 = ((b - c) / (a * 2.0)).min(0.997_23);
        state.lowpass_2 = 1.0 - state.lowpass_1;
    }

    state.early_to_late_taps = (((parameter.reflection_delay + parameter.late_reverb_delay_time)
        * 1000.0)
        * sr_ms)
        .floor() as u32;
    state.last_reverb_echo = parameter.late_reverb_diffusion * 0.006;

    for i in 0..4 {
        let current_delay = ((MIN_DELAY_LINE_TIMES[i]
            + (parameter.late_reverb_density / 100.0)
                * (MAX_DELAY_LINE_TIMES[i] - MIN_DELAY_LINE_TIMES[i]))
            * sr_ms)
            .floor() as u32;
        state.fdn_delay_samples[i] = current_delay.max(1);

        let a = (((state.fdn_delay_samples[i]
            + state.decay0_delay_samples[i]
            + state.decay1_delay_samples[i]) as f32)
            * -60.0)
            / (parameter.late_reverb_decay_time.max(0.001) * parameter.sample_rate.max(1) as f32);
        let b = a / parameter.late_reverb_hf_decay_ratio.max(0.001);
        let phase = ((parameter.reference_hf * 0.5) * 128.0) / parameter.sample_rate.max(1) as f32;
        let c = phase.cos() / phase.sin().max(1.0e-6);
        let d = pow10((b - a) / 40.0);
        let e = pow10((b + a) / 40.0) * 0.7071;

        state.lowpass_coeff[i][0] = ((c * d + 1.0) * e) / (c + d).max(1.0e-6);
        state.lowpass_coeff[i][1] = ((1.0 - (c * d)) * e) / (c + d).max(1.0e-6);
        state.lowpass_coeff[i][2] = (c - d) / (c + d).max(1.0e-6);
    }

    if reset {
        state.shelf_filter.fill(0.0);
        state.lowpass_0 = 0.0;
        state.fdn_positions.fill(0);
        state.decay0_positions.fill(0);
        state.decay1_positions.fill(0);
        state.center_delay_pos = 0;
        state.early_delay_pos = 0;
    }

    let reflection_time = (parameter.late_reverb_delay_time * 0.9998 + 0.02) * 1000.0;
    let reflection_delay = parameter.reflection_delay * 1000.0;
    for (i, tap_time) in EARLY_TAP_TIMES.iter().copied().enumerate() {
        let mut length = ((reflection_delay + reflection_time * tap_time) * sr_ms).floor() as u32;
        if length >= state.early_delay_len {
            length = state.early_delay_len.saturating_sub(1);
        }
        state.early_tap_steps[i] = length;
    }
}

pub fn initialize_i3dl2_reverb_effect(
    parameter: &i3dl2::ParameterVersion2,
    state: &mut I3dl2ReverbState,
    workbuffer: &mut [f32],
) {
    *state = I3dl2ReverbState::default();

    let sr_ms = parameter.sample_rate.max(1) as f32 / 1000.0;
    for i in 0..4 {
        state.fdn_delay_samples[i] = (MAX_DELAY_LINE_TIMES[i] * sr_ms).floor() as u32;
        state.decay0_delay_samples[i] = (DECAY0_MAX_DELAY_LINE_TIMES[i] * sr_ms).floor() as u32;
        state.decay1_delay_samples[i] = (DECAY1_MAX_DELAY_LINE_TIMES[i] * sr_ms).floor() as u32;
    }
    state.center_delay_len = ((5.0 * sr_ms).floor() as u32).max(1);
    state.early_delay_len = ((400.0 * sr_ms).floor() as u32).max(1);

    update_i3dl2_reverb_effect_parameter(parameter, state, true);
    let total_samples = total_workbuffer_samples(state);
    workbuffer
        .get_mut(..total_samples)
        .into_iter()
        .flatten()
        .for_each(|sample| *sample = 0.0);
}

pub fn apply_i3dl2_reverb_effect_bypass(
    inputs: &[i16; MAX_CHANNELS as usize],
    outputs: &[i16; MAX_CHANNELS as usize],
    channel_count: usize,
    sample_count: usize,
    mix_buffers: &mut [i32],
) {
    for channel in 0..channel_count.min(MAX_CHANNELS as usize) {
        copy_mix_buffer(mix_buffers, sample_count, outputs[channel], inputs[channel]);
    }
}

pub fn total_workbuffer_samples(state: &I3dl2ReverbState) -> usize {
    state.early_delay_len as usize
        + state.center_delay_len as usize
        + state
            .fdn_delay_samples
            .iter()
            .map(|&v| v.max(1) as usize)
            .sum::<usize>()
        + state
            .decay0_delay_samples
            .iter()
            .map(|&v| v.max(1) as usize)
            .sum::<usize>()
        + state
            .decay1_delay_samples
            .iter()
            .map(|&v| v.max(1) as usize)
            .sum::<usize>()
}

pub fn workbuffer_offsets(
    state: &I3dl2ReverbState,
) -> (usize, usize, [usize; 4], [usize; 4], [usize; 4], usize) {
    let early = 0usize;
    let center = early + state.early_delay_len as usize;
    let mut current = center + state.center_delay_len as usize;
    let mut fdn = [0usize; 4];
    let mut decay0 = [0usize; 4];
    let mut decay1 = [0usize; 4];
    for i in 0..4 {
        fdn[i] = current;
        current = current.saturating_add(state.fdn_delay_samples[i].max(1) as usize);
    }
    for i in 0..4 {
        decay0[i] = current;
        current = current.saturating_add(state.decay0_delay_samples[i].max(1) as usize);
    }
    for i in 0..4 {
        decay1[i] = current;
        current = current.saturating_add(state.decay1_delay_samples[i].max(1) as usize);
    }
    (early, center, fdn, decay0, decay1, current)
}

pub fn tap_indexes(channel_count: usize) -> &'static [u8] {
    match channel_count {
        1 => &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        2 => &[0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1],
        4 => &[0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 0, 0, 0, 0, 3, 3, 3],
        6 => &[2, 0, 0, 1, 1, 1, 1, 4, 4, 4, 1, 1, 1, 0, 0, 0, 0, 5, 5, 5],
        _ => &[],
    }
}

pub fn apply_i3dl2_reverb_effect(
    parameter: &i3dl2::ParameterVersion2,
    state: &mut I3dl2ReverbState,
    enabled: bool,
    inputs: &[i16; MAX_CHANNELS as usize],
    outputs: &[i16; MAX_CHANNELS as usize],
    mix_buffers: &mut [i32],
    sample_count: usize,
    workbuffer: &mut [f32],
) {
    let channel_count = parameter.channel_count.max(0) as usize;
    let active_channels = channel_count.min(inputs.len()).min(outputs.len());
    if active_channels == 0 {
        return;
    }

    if !enabled || !matches!(active_channels, 1 | 2 | 4 | 6) {
        apply_i3dl2_reverb_effect_bypass(
            inputs,
            outputs,
            active_channels,
            sample_count,
            mix_buffers,
        );
        return;
    }

    let total_samples = total_workbuffer_samples(state);
    let Some(workbuffer) = workbuffer.get_mut(..total_samples) else {
        apply_i3dl2_reverb_effect_bypass(
            inputs,
            outputs,
            active_channels,
            sample_count,
            mix_buffers,
        );
        return;
    };

    let (early_offset, center_offset, fdn_offsets, decay0_offsets, decay1_offsets, _) =
        workbuffer_offsets(state);
    let tap_indexes = tap_indexes(active_channels);

    for sample_index in 0..sample_count {
        let early_to_late_tap = ring_tap(
            workbuffer,
            early_offset,
            state.early_delay_len as usize,
            state.early_delay_pos,
            state.early_to_late_taps,
        );
        let mut output_samples = [0.0f32; MAX_CHANNELS as usize];

        for early_tap in 0..20 {
            let tap = ring_tap(
                workbuffer,
                early_offset,
                state.early_delay_len as usize,
                state.early_delay_pos,
                state.early_tap_steps[early_tap],
            ) * EARLY_GAINS[early_tap];
            output_samples[tap_indexes[early_tap] as usize] += tap;
            if active_channels == 6 {
                output_samples[3] += tap;
            }
        }

        let mut current_sample = 0.0f32;
        for channel in 0..active_channels {
            current_sample +=
                mix_buffer_sample(mix_buffers, inputs[channel], sample_count, sample_index) as f32;
        }

        state.lowpass_0 = current_sample * state.lowpass_2 + state.lowpass_0 * state.lowpass_1;
        let _ = ring_tick(
            workbuffer,
            early_offset,
            state.early_delay_len as usize,
            &mut state.early_delay_pos,
            state.lowpass_0,
        );

        for sample in output_samples.iter_mut().take(active_channels) {
            *sample *= state.early_gain;
        }

        let mut filtered = [0.0f32; 4];
        for delay_line in 0..4 {
            let fdn_read = ring_tap(
                workbuffer,
                fdn_offsets[delay_line],
                state.fdn_delay_samples[delay_line].max(1) as usize,
                state.fdn_positions[delay_line],
                0,
            );
            filtered[delay_line] =
                fdn_read * state.lowpass_coeff[delay_line][0] + state.shelf_filter[delay_line];
            state.shelf_filter[delay_line] = filtered[delay_line]
                * state.lowpass_coeff[delay_line][2]
                + fdn_read * state.lowpass_coeff[delay_line][1];
        }

        let mix_matrix = [
            filtered[1] + filtered[2] + early_to_late_tap * state.late_gain,
            -filtered[0] - filtered[3] + early_to_late_tap * state.late_gain,
            filtered[0] - filtered[3] + early_to_late_tap * state.late_gain,
            filtered[1] - filtered[2] + early_to_late_tap * state.late_gain,
        ];

        let mut allpass = [0.0f32; 4];
        for delay_line in 0..4 {
            allpass[delay_line] = axfx2_all_pass_tick(
                workbuffer,
                decay0_offsets[delay_line],
                state.decay0_delay_samples[delay_line].max(1) as usize,
                &mut state.decay0_positions[delay_line],
                state.last_reverb_echo,
                decay1_offsets[delay_line],
                state.decay1_delay_samples[delay_line].max(1) as usize,
                &mut state.decay1_positions[delay_line],
                state.last_reverb_echo * -0.9,
                fdn_offsets[delay_line],
                state.fdn_delay_samples[delay_line].max(1) as usize,
                &mut state.fdn_positions[delay_line],
                mix_matrix[delay_line],
            );
        }

        for channel in 0..active_channels {
            let input =
                mix_buffer_sample(mix_buffers, inputs[channel], sample_count, sample_index) as f32;
            let wet = if active_channels == 6 {
                let mapped = match channel {
                    0 => allpass[0],
                    1 => allpass[1],
                    2 => allpass[2] - allpass[3],
                    3 => ring_tick(
                        workbuffer,
                        center_offset,
                        state.center_delay_len as usize,
                        &mut state.center_delay_pos,
                        allpass[3] * 0.5,
                    ),
                    4 => allpass[2],
                    5 => allpass[3],
                    _ => 0.0,
                };
                output_samples[channel] + mapped
            } else {
                output_samples[channel] + allpass[channel.min(3)]
            };
            set_mix_buffer_sample(
                mix_buffers,
                outputs[channel],
                sample_count,
                sample_index,
                (wet + state.dry_gain * input)
                    .clamp(-8_388_600.0, 8_388_600.0)
                    .round() as i32,
            );
        }
    }
}

fn axfx2_all_pass_tick(
    workbuffer: &mut [f32],
    decay0_start: usize,
    decay0_len: usize,
    decay0_pos: &mut u32,
    decay0_wet_gain: f32,
    decay1_start: usize,
    decay1_len: usize,
    decay1_pos: &mut u32,
    decay1_wet_gain: f32,
    fdn_start: usize,
    fdn_len: usize,
    fdn_pos: &mut u32,
    mix: f32,
) -> f32 {
    let value0 = ring_tap(workbuffer, decay0_start, decay0_len, *decay0_pos, 0);
    let mixed0 = mix - (value0 * decay0_wet_gain);
    let out0 = ring_tick(workbuffer, decay0_start, decay0_len, decay0_pos, mixed0)
        + mixed0 * decay0_wet_gain;

    let value1 = ring_tap(workbuffer, decay1_start, decay1_len, *decay1_pos, 0);
    let mixed1 = out0 - (value1 * decay1_wet_gain);
    let out1 = ring_tick(workbuffer, decay1_start, decay1_len, decay1_pos, mixed1)
        + mixed1 * decay1_wet_gain;

    let _ = ring_tick(workbuffer, fdn_start, fdn_len, fdn_pos, out1);
    out1
}

fn ring_tap(buffer: &[f32], start: usize, len: usize, pos: u32, delay: u32) -> f32 {
    if len == 0 {
        return 0.0;
    }
    let delay = delay.min(len.saturating_sub(1) as u32) as usize;
    let idx = (pos as usize + len - delay - 1) % len;
    buffer[start + idx]
}

fn ring_tick(buffer: &mut [f32], start: usize, len: usize, pos: &mut u32, value: f32) -> f32 {
    if len == 0 {
        return 0.0;
    }
    let idx = (*pos as usize) % len;
    let out = buffer[start + idx];
    buffer[start + idx] = value;
    *pos = ((idx + 1) % len) as u32;
    out
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

fn mix_buffer_range(
    mix_buffers: &[i32],
    buffer_index: i16,
    sample_count: usize,
) -> Option<std::ops::Range<usize>> {
    if buffer_index < 0 {
        return None;
    }
    let start = buffer_index as usize * sample_count;
    let end = start.saturating_add(sample_count);
    (end <= mix_buffers.len()).then_some(start..end)
}

fn copy_mix_buffer(
    mix_buffers: &mut [i32],
    sample_count: usize,
    output_index: i16,
    input_index: i16,
) {
    let Some(input_range) = mix_buffer_range(mix_buffers, input_index, sample_count) else {
        return;
    };
    let Some(output_range) = mix_buffer_range(mix_buffers, output_index, sample_count) else {
        return;
    };
    if input_range == output_range {
        return;
    }
    let input = mix_buffers[input_range].to_vec();
    mix_buffers[output_range].copy_from_slice(&input);
}

fn read_i3dl2_reverb_state_mut(addr: CpuAddr) -> Option<&'static mut I3dl2ReverbState> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { &mut *(addr as *mut I3dl2ReverbState) })
}

fn read_f32_slice_mut(addr: CpuAddr, len: usize) -> Option<&'static mut [f32]> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts_mut(addr as *mut f32, len) })
}
