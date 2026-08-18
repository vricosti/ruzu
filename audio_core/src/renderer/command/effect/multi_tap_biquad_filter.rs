use crate::common::common::{CpuAddr, MAX_BIQUAD_FILTERS};
use crate::renderer::command::effect::biquad_filter::{
    apply_biquad_filter_float, apply_biquad_filter_float2, read_biquad_state_mut,
};
use crate::renderer::command::util::write_copy;
use crate::renderer::voice::voice_info::{BiquadFilterParameter, BiquadFilterParameter2};
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MultiTapBiquadFilterPayload {
    pub input: i16,
    pub output: i16,
    pub biquads: [BiquadFilterParameter; MAX_BIQUAD_FILTERS as usize],
    pub biquads_float: [BiquadFilterParameter2; MAX_BIQUAD_FILTERS as usize],
    pub _padding0: [u8; 4],
    pub states: [CpuAddr; MAX_BIQUAD_FILTERS as usize],
    pub needs_init: [bool; MAX_BIQUAD_FILTERS as usize],
    pub filter_tap_count: u8,
    pub use_float_coefficients: bool,
    pub _padding1: [u8; 3],
}

#[derive(Debug, Clone, Copy, Default)]
pub struct MultiTapBiquadFilterCommand {
    pub input: i16,
    pub output: i16,
    pub biquads: [BiquadFilterParameter; MAX_BIQUAD_FILTERS as usize],
    pub biquads_float: [BiquadFilterParameter2; MAX_BIQUAD_FILTERS as usize],
    pub states: [CpuAddr; MAX_BIQUAD_FILTERS as usize],
    pub needs_init: [bool; MAX_BIQUAD_FILTERS as usize],
    pub filter_tap_count: u8,
    pub use_float_coefficients: bool,
}

impl MultiTapBiquadFilterPayload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_multi_tap_biquad_filter_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_multi_tap_biquad_filter_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_multi_tap_biquad_filter_command(self, dump);
    }
}

pub fn write_multi_tap_biquad_filter_payload(
    cmd: &MultiTapBiquadFilterCommand,
    output: &mut [u8],
) -> usize {
    let mut payload: MultiTapBiquadFilterPayload = unsafe { std::mem::zeroed() };
    payload.input = cmd.input;
    payload.output = cmd.output;
    payload.biquads = cmd.biquads;
    payload.biquads_float = cmd.biquads_float;
    payload._padding0 = [0; 4];
    payload.states = cmd.states;
    payload.needs_init = cmd.needs_init;
    payload.filter_tap_count = cmd.filter_tap_count;
    payload.use_float_coefficients = cmd.use_float_coefficients;
    payload._padding1 = [0; 3];
    write_copy(&payload, output)
}

pub fn process_multi_tap_biquad_filter_command(
    payload: &MultiTapBiquadFilterPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let Some(input_range) = mix_buffer_range(mix_buffers, payload.input, sample_count) else {
        return;
    };
    let Some(output_range) = mix_buffer_range(mix_buffers, payload.output, sample_count) else {
        return;
    };

    let tap_count = payload
        .filter_tap_count
        .min(payload.states.len() as u8)
        .min(payload.biquads.len() as u8) as usize;
    for i in 0..tap_count {
        let Some(state) = read_biquad_state_mut(payload.states[i]) else {
            continue;
        };
        if payload.needs_init[i] {
            *state = Default::default();
        }
        if payload.use_float_coefficients {
            apply_biquad_filter_float2(
                mix_buffers,
                input_range.clone(),
                output_range.clone(),
                payload.biquads_float[i],
                state,
            );
        } else {
            apply_biquad_filter_float(
                mix_buffers,
                input_range.clone(),
                output_range.clone(),
                payload.biquads[i],
                state,
            );
        }
    }
}

pub fn verify_multi_tap_biquad_filter_command(_payload: &MultiTapBiquadFilterPayload) -> bool {
    true
}

pub fn dump_multi_tap_biquad_filter_command(
    payload: &MultiTapBiquadFilterPayload,
    dump: &mut String,
) {
    let _ = writeln!(
        dump,
        "MultiTapBiquadFilterCommand\n\tinput {:02X}\n\toutput {:02X}\n\tneeds_init ({}, {})",
        payload.input, payload.output, payload.needs_init[0], payload.needs_init[1]
    );
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::{offset_of, size_of};

    #[test]
    fn revision_15_multitap_payload_layout_is_deterministic() {
        assert_eq!(size_of::<MultiTapBiquadFilterPayload>(), 0x68);
        assert_eq!(offset_of!(MultiTapBiquadFilterPayload, biquads_float), 0x1C);
        assert_eq!(offset_of!(MultiTapBiquadFilterPayload, states), 0x50);
        assert_eq!(
            offset_of!(MultiTapBiquadFilterPayload, filter_tap_count),
            0x62
        );
        assert_eq!(
            offset_of!(MultiTapBiquadFilterPayload, use_float_coefficients),
            0x63
        );
    }
}
