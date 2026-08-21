use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::command::util::write_copy;
use crate::renderer::effect::effect_info_base::ParameterState;
use crate::renderer::effect::i3dl2::{self, I3dl2DelayLine, I3dl2Fixed, I3dl2ReverbState};
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
                if state.is_initialized() {
                    update_i3dl2_reverb_effect_parameter(&payload.parameter, state, false);
                }
            }
            ParameterState::Initialized => {
                initialize_i3dl2_reverb_effect(&payload.parameter, state, payload.workbuffer);
            }
            ParameterState::Updated => {}
        }
    }

    if !state.is_initialized() {
        apply_i3dl2_reverb_effect_bypass(
            &payload.inputs,
            &payload.outputs,
            channel_count,
            sample_count,
            mix_buffers,
        );
        return;
    }
    apply_i3dl2_reverb_effect(
        &payload.parameter,
        state,
        payload.effect_enabled,
        &payload.inputs,
        &payload.outputs,
        mix_buffers,
        sample_count,
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

    let sin = |degrees: f32| degrees.to_radians().sin();
    let cos = |degrees: f32| degrees.to_radians().cos();
    let delay = I3dl2Fixed::from_f32(parameter.sample_rate as f32 / 1000.0);

    state.dry_gain = parameter.dry_gain;
    let early_gain = I3dl2Fixed::from_f32(
        (parameter.room_gain + parameter.reflection_gain).min(5000.0) / 2000.0,
    );
    state.early_gain = pow10(early_gain.to_f32());
    let late_gain =
        I3dl2Fixed::from_f32((parameter.room_gain + parameter.reverb_gain).min(5000.0) / 2000.0);
    state.late_gain = pow10(late_gain.to_f32());

    let hf_gain = I3dl2Fixed::from_f32(pow10(parameter.room_hf_gain / 2000.0));
    if hf_gain.to_f32() >= 1.0 {
        state.lowpass_1 = 0.0;
        state.lowpass_2 = 1.0;
    } else {
        let reference_hf = (parameter.reference_hf * 256.0) / parameter.sample_rate as f32;
        let a = I3dl2Fixed::from_f32(1.0 - hf_gain.to_f32());
        let b = I3dl2Fixed::from_f32(
            2.0 + (-cos(reference_hf) * (hf_gain * I3dl2Fixed::from_f32(2.0)).to_f32()),
        );
        let c = I3dl2Fixed::from_f32((b.to_f32().powi(2) + a.to_f32().powi(2) * -4.0).sqrt());
        state.lowpass_1 = ((b - c) / (a * I3dl2Fixed::from_f32(2.0)))
            .to_f32()
            .min(0.997_23);
        state.lowpass_2 = 1.0 - state.lowpass_1;
    }

    state.early_to_late_taps = (I3dl2Fixed::from_f32(
        (parameter.reflection_delay + parameter.late_reverb_delay_time) * 1000.0,
    ) * delay)
        .to_int();
    state.last_reverb_echo = parameter.late_reverb_diffusion * 0.006;

    for i in 0..4 {
        let current_delay = (I3dl2Fixed::from_f32(
            MIN_DELAY_LINE_TIMES[i]
                + (parameter.late_reverb_density / 100.0)
                    * (MAX_DELAY_LINE_TIMES[i] - MIN_DELAY_LINE_TIMES[i]),
        ) * delay)
            .to_int();
        state.fdn_delay_lines[i].set_delay(current_delay);

        let a = (((state.fdn_delay_lines[i].delay
            + state.decay_delay_lines0[i].delay
            + state.decay_delay_lines1[i].delay) as f32)
            * -60.0)
            / (parameter.late_reverb_decay_time * parameter.sample_rate as f32);
        let b = a / parameter.late_reverb_hf_decay_ratio;
        let phase = ((parameter.reference_hf * 0.5) * 128.0) / parameter.sample_rate as f32;
        let c = cos(phase) / sin(phase);
        let d = pow10((b - a) / 40.0);
        let e = pow10((b + a) / 40.0) * 0.7071;

        state.lowpass_coeff[i][0] = ((c * d + 1.0) * e) / (c + d);
        state.lowpass_coeff[i][1] = ((1.0 - c * d) * e) / (c + d);
        state.lowpass_coeff[i][2] = (c - d) / (c + d);
        state.decay_delay_lines0[i].wet_gain = state.last_reverb_echo;
        state.decay_delay_lines1[i].wet_gain = state.last_reverb_echo * -0.9;
    }

    if reset {
        state.shelf_filter.fill(0.0);
        state.lowpass_0 = 0.0;
        for delay_line in 0..4 {
            state.fdn_delay_lines[delay_line]
                .buffer
                .fill(I3dl2Fixed::default());
            state.decay_delay_lines0[delay_line]
                .buffer
                .fill(I3dl2Fixed::default());
            state.decay_delay_lines1[delay_line]
                .buffer
                .fill(I3dl2Fixed::default());
        }
        state.center_delay_line.buffer.fill(I3dl2Fixed::default());
        state.early_delay_line.buffer.fill(I3dl2Fixed::default());
    }

    let reflection_time = (parameter.late_reverb_delay_time * 0.9998 + 0.02) * 1000.0;
    let reflection_delay = parameter.reflection_delay * 1000.0;
    for (i, tap_time) in EARLY_TAP_TIMES.iter().copied().enumerate() {
        let mut length =
            (I3dl2Fixed::from_f32(reflection_delay + reflection_time * tap_time) * delay).to_int();
        if length >= state.early_delay_line.max_delay {
            length = state.early_delay_line.max_delay;
        }
        state.early_tap_steps[i] = length;
    }
}

pub fn initialize_i3dl2_reverb_effect(
    parameter: &i3dl2::ParameterVersion2,
    state: &mut I3dl2ReverbState,
    _workbuffer: CpuAddr,
) {
    state.reset_and_mark_initialized();

    let delay = I3dl2Fixed::from_f32(parameter.sample_rate as f32 / 1000.0);
    for i in 0..4 {
        state.fdn_delay_lines[i].initialize(
            (I3dl2Fixed::from_f32(MAX_DELAY_LINE_TIMES[i]) * delay).to_uint_floor() as i32,
        );
        state.decay_delay_lines0[i].initialize(
            (I3dl2Fixed::from_f32(DECAY0_MAX_DELAY_LINE_TIMES[i]) * delay).to_uint_floor() as i32,
        );
        state.decay_delay_lines1[i].initialize(
            (I3dl2Fixed::from_f32(DECAY1_MAX_DELAY_LINE_TIMES[i]) * delay).to_uint_floor() as i32,
        );
    }
    state
        .center_delay_line
        .initialize((I3dl2Fixed::from_int(5) * delay).to_uint_floor() as i32);
    state
        .early_delay_line
        .initialize((I3dl2Fixed::from_int(400) * delay).to_uint_floor() as i32);

    update_i3dl2_reverb_effect_parameter(parameter, state, true);
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

    let state = &mut **state;
    let tap_indexes = tap_indexes(active_channels);

    for sample_index in 0..sample_count {
        let early_to_late_tap = state.early_delay_line.tap_out(state.early_to_late_taps);
        let mut output_samples = [I3dl2Fixed::default(); MAX_CHANNELS as usize];

        for early_tap in 0..20 {
            let tap = state
                .early_delay_line
                .tap_out(state.early_tap_steps[early_tap])
                * I3dl2Fixed::from_f32(EARLY_GAINS[early_tap]);
            output_samples[tap_indexes[early_tap] as usize] += tap;
            if active_channels == 6 {
                output_samples[3] += tap;
            }
        }

        let mut current_sample = I3dl2Fixed::default();
        for channel in 0..active_channels {
            current_sample += I3dl2Fixed::from_int(mix_buffer_sample(
                mix_buffers,
                inputs[channel],
                sample_count,
                sample_index,
            ) as i64);
        }

        state.lowpass_0 = (current_sample * I3dl2Fixed::from_f32(state.lowpass_2)
            + I3dl2Fixed::from_f32(state.lowpass_0) * I3dl2Fixed::from_f32(state.lowpass_1))
        .to_f32();
        let lowpass_0 = state.lowpass_0;
        state.early_delay_line.tick(I3dl2Fixed::from_f32(lowpass_0));

        for sample in output_samples.iter_mut().take(active_channels) {
            *sample *= I3dl2Fixed::from_f32(state.early_gain);
        }

        let mut filtered = [I3dl2Fixed::default(); 4];
        for delay_line in 0..4 {
            let fdn_read = state.fdn_delay_lines[delay_line].read();
            filtered[delay_line] = fdn_read
                * I3dl2Fixed::from_f32(state.lowpass_coeff[delay_line][0])
                + I3dl2Fixed::from_f32(state.shelf_filter[delay_line]);
            state.shelf_filter[delay_line] = (filtered[delay_line]
                * I3dl2Fixed::from_f32(state.lowpass_coeff[delay_line][2])
                + fdn_read * I3dl2Fixed::from_f32(state.lowpass_coeff[delay_line][1]))
            .to_f32();
        }

        let mix_matrix = [
            filtered[1] + filtered[2] + early_to_late_tap * I3dl2Fixed::from_f32(state.late_gain),
            -filtered[0] - filtered[3] + early_to_late_tap * I3dl2Fixed::from_f32(state.late_gain),
            filtered[0] - filtered[3] + early_to_late_tap * I3dl2Fixed::from_f32(state.late_gain),
            filtered[1] - filtered[2] + early_to_late_tap * I3dl2Fixed::from_f32(state.late_gain),
        ];

        let mut allpass = [I3dl2Fixed::default(); 4];
        for delay_line in 0..4 {
            allpass[delay_line] = axfx2_all_pass_tick(
                &mut state.decay_delay_lines0[delay_line],
                &mut state.decay_delay_lines1[delay_line],
                &mut state.fdn_delay_lines[delay_line],
                mix_matrix[delay_line],
            );
        }

        for channel in 0..active_channels {
            let input = I3dl2Fixed::from_int(mix_buffer_sample(
                mix_buffers,
                inputs[channel],
                sample_count,
                sample_index,
            ) as i64);
            let wet = if active_channels == 6 {
                let mapped = match channel {
                    0 => allpass[0],
                    1 => allpass[1],
                    2 => allpass[2] - allpass[3],
                    3 => state
                        .center_delay_line
                        .tick(allpass[3] * I3dl2Fixed::from_f32(0.5)),
                    4 => allpass[2],
                    5 => allpass[3],
                    _ => I3dl2Fixed::default(),
                };
                output_samples[channel] + mapped
            } else {
                output_samples[channel] + allpass[channel.min(3)]
            };
            let output = wet + I3dl2Fixed::from_f32(state.dry_gain * input.to_f32());
            set_mix_buffer_sample(
                mix_buffers,
                outputs[channel],
                sample_count,
                sample_index,
                output.to_f32().clamp(-8_388_600.0, 8_388_600.0) as i32,
            );
        }
    }
}

fn axfx2_all_pass_tick(
    decay0: &mut I3dl2DelayLine,
    decay1: &mut I3dl2DelayLine,
    fdn: &mut I3dl2DelayLine,
    mix_: I3dl2Fixed,
) -> I3dl2Fixed {
    let value = decay0.read();
    let mixed = mix_ - value * I3dl2Fixed::from_f32(decay0.wet_gain);
    let output = decay0.tick(mixed) + mixed * I3dl2Fixed::from_f32(decay0.wet_gain);

    let value = decay1.read();
    let mixed = output - value * I3dl2Fixed::from_f32(decay1.wet_gain);
    let output = decay1.tick(mixed) + mixed * I3dl2Fixed::from_f32(decay1.wet_gain);

    fdn.tick(output);
    output
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
    crate::raw_write_trace::maybe_trace_write_at(
        "i3dl2_reverb:state_mut",
        addr,
        std::mem::size_of::<I3dl2ReverbState>(),
    );
    Some(unsafe { &mut *(addr as *mut I3dl2ReverbState) })
}
