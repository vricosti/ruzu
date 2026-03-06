use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::command::mix::copy_mix_buffer;
use crate::renderer::command::util::write_copy;
use crate::renderer::effect::compressor;
use crate::renderer::effect::effect_info_base::ParameterState;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CompressorPayload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: compressor::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct CompressorCommand {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: compressor::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub enabled: bool,
}

impl CompressorPayload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_compressor_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_compressor_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_compressor_command(self, dump);
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct CompressorState {
    pub unk_00: f32,
    pub unk_04: f32,
    pub unk_08: f32,
    pub unk_0c: f32,
    pub unk_10: f32,
    pub unk_14: f32,
    pub unk_18: f32,
    pub makeup_gain: f32,
    pub unk_20: f32,
    pub _padding: [u8; 0x1c],
}

pub fn write_compressor_payload(cmd: &CompressorCommand, output: &mut [u8]) -> usize {
    let mut payload: CompressorPayload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.parameter = cmd.parameter;
    payload.state = cmd.state;
    payload.workbuffer = cmd.workbuffer;
    payload.enabled = cmd.enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn process_compressor_command(
    payload: &CompressorPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let channel_count = payload.parameter.channel_count.max(0) as usize;
    if channel_count == 0 {
        return;
    }

    let Some(state) = read_compressor_state_mut(payload.state) else {
        return;
    };
    if payload.enabled {
        match payload.parameter.state {
            ParameterState::Updating => set_compressor_effect_parameter(&payload.parameter, state),
            ParameterState::Initialized => initialize_compressor_effect(&payload.parameter, state),
            ParameterState::Updated => {}
        }
    }

    if payload.enabled {
        apply_compressor_effect(payload, state, mix_buffers, sample_count);
    } else {
        for channel in 0..channel_count
            .min(payload.inputs.len())
            .min(payload.outputs.len())
        {
            copy_mix_buffer(
                mix_buffers,
                sample_count,
                payload.outputs[channel],
                payload.inputs[channel],
            );
        }
    }
}

pub fn verify_compressor_command(_payload: &CompressorPayload) -> bool {
    true
}

pub fn dump_compressor_command(payload: &CompressorPayload, dump: &mut String) {
    let channel_count = payload.parameter.channel_count.max(0) as usize;
    let _ = write!(
        dump,
        "CompressorCommand\n\tenabled {} \n\tinputs: ",
        payload.enabled
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

pub fn set_compressor_effect_parameter(
    params: &compressor::ParameterVersion2,
    state: &mut CompressorState,
) {
    let ratio = 1.0 / params.compressor_ratio.max(f32::MIN_POSITIVE);
    let makeup_gain = if params.makeup_gain_enabled {
        (params.threshold * 0.5) * (ratio - 1.0) - 3.0
    } else {
        0.0
    };
    state.makeup_gain = makeup_gain;
    state.unk_18 = params.unk_28;

    let a = (params.out_gain + makeup_gain) / 20.0 * 3.3219;
    let b = (a - a.trunc()) * 0.69315;
    let c = 2.0f32.powf(b);

    state.unk_0c = (1.0 - ratio) / 6.0;
    state.unk_14 = params.threshold + 1.5;
    state.unk_10 = params.threshold - 1.5;
    state.unk_20 = c;
}

pub fn initialize_compressor_effect(
    params: &compressor::ParameterVersion2,
    state: &mut CompressorState,
) {
    *state = CompressorState::default();
    state.unk_00 = 0.0;
    state.unk_04 = 1.0;
    state.unk_08 = 1.0;
    set_compressor_effect_parameter(params, state);
}

pub fn apply_compressor_effect(
    payload: &CompressorPayload,
    state: &mut CompressorState,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let channel_count = payload.parameter.channel_count.max(0) as usize;
    if channel_count == 0 {
        return;
    }

    let mut state_00 = state.unk_00;
    let mut state_04 = state.unk_04;
    let mut state_08 = state.unk_08;
    let mut state_18 = state.unk_18;

    for sample_index in 0..sample_count {
        let mut a = 0.0f32;
        for channel in 0..channel_count
            .min(payload.inputs.len())
            .min(payload.outputs.len())
        {
            let input_sample = mix_buffer_sample(
                mix_buffers,
                payload.inputs[channel],
                sample_count,
                sample_index,
            ) as f32;
            a += input_sample * input_sample;
        }

        state_00 += payload.parameter.unk_24 * ((a / channel_count as f32) - state.unk_00);

        let mut b = -100.0f32;
        let mut c = 0.0f32;
        if state_00 >= 1.0e-10 {
            b = state_00.log10() * 10.0;
            c = 1.0;
        }

        if b >= state.unk_10 {
            let d = if b >= state.unk_14 {
                ((1.0 / payload.parameter.compressor_ratio.max(f32::MIN_POSITIVE)) - 1.0)
                    * (b - payload.parameter.threshold)
            } else {
                (b - state.unk_10) * (b - state.unk_10) * -state.unk_0c
            };
            let e = d / 20.0 * 3.3219;
            let f = (e - e.trunc()) * 0.69315;
            c = 2.0f32.powf(f);
        }

        state_18 = payload.parameter.unk_28;
        let mut tmp = c;
        if (state_04 - c) <= 0.08 {
            state_18 = payload.parameter.unk_2c;
            if (state_04 - c) >= -0.08 && (state_08 - c).abs() >= 0.001 {
                tmp = state_04;
            }
        }

        state_04 = tmp;
        state_08 += (c - state_08) * state_18;

        for channel in 0..channel_count
            .min(payload.inputs.len())
            .min(payload.outputs.len())
        {
            let input_sample = mix_buffer_sample(
                mix_buffers,
                payload.inputs[channel],
                sample_count,
                sample_index,
            ) as f32;
            set_mix_buffer_sample(
                mix_buffers,
                payload.outputs[channel],
                sample_count,
                sample_index,
                (input_sample * state_08 * state.unk_20) as i32,
            );
        }
    }

    state.unk_00 = state_00;
    state.unk_04 = state_04;
    state.unk_08 = state_08;
    state.unk_18 = state_18;
}

fn read_compressor_state_mut(addr: CpuAddr) -> Option<&'static mut CompressorState> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { &mut *(addr as *mut CompressorState) })
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
