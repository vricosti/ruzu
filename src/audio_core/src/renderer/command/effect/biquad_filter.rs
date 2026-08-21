use crate::common::common::CpuAddr;
use crate::renderer::command::util::write_copy;
use crate::renderer::voice::voice_info::{BiquadFilterParameter, BiquadFilterParameter2};
use crate::renderer::voice::voice_state::BiquadFilterState;
use common::fixed_point::FixedPoint;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct BiquadFilterPayload {
    pub input: i16,
    pub output: i16,
    pub biquad: BiquadFilterParameter,
    pub biquad_float: BiquadFilterParameter2,
    pub state: CpuAddr,
    pub needs_init: bool,
    pub use_float_processing: bool,
    pub use_float_coefficients: bool,
    pub _padding0: [u8; 5],
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BiquadFilterCommand {
    pub input: i16,
    pub output: i16,
    pub biquad: BiquadFilterParameter,
    pub biquad_float: BiquadFilterParameter2,
    pub state: CpuAddr,
    pub needs_init: bool,
    pub use_float_processing: bool,
    pub use_float_coefficients: bool,
}

impl BiquadFilterPayload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_biquad_filter_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_biquad_filter_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_biquad_filter_command(self, dump);
    }
}

pub fn write_biquad_filter_payload(cmd: &BiquadFilterCommand, output: &mut [u8]) -> usize {
    let mut payload: BiquadFilterPayload = unsafe { std::mem::zeroed() };
    payload.input = cmd.input;
    payload.output = cmd.output;
    payload.biquad = cmd.biquad;
    payload.biquad_float = cmd.biquad_float;
    payload.state = cmd.state;
    payload.needs_init = cmd.needs_init;
    payload.use_float_processing = cmd.use_float_processing;
    payload.use_float_coefficients = cmd.use_float_coefficients;
    payload._padding0 = [0; 5];
    write_copy(&payload, output)
}

pub fn process_biquad_filter_command(
    payload: &BiquadFilterPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let Some(input_range) = mix_buffer_range(mix_buffers, payload.input, sample_count) else {
        return;
    };
    let Some(output_range) = mix_buffer_range(mix_buffers, payload.output, sample_count) else {
        return;
    };
    let Some(state) = read_biquad_state_mut(payload.state) else {
        return;
    };
    if payload.needs_init {
        *state = Default::default();
    }
    if payload.use_float_processing {
        if payload.use_float_coefficients {
            apply_biquad_filter_float2(
                mix_buffers,
                input_range,
                output_range,
                payload.biquad_float,
                state,
            );
        } else {
            apply_biquad_filter_float(
                mix_buffers,
                input_range,
                output_range,
                payload.biquad,
                state,
            );
        }
    } else {
        apply_biquad_filter_int(
            mix_buffers,
            input_range,
            output_range,
            payload.biquad,
            state,
        );
    }
}

pub fn verify_biquad_filter_command(_payload: &BiquadFilterPayload) -> bool {
    true
}

pub fn dump_biquad_filter_command(payload: &BiquadFilterPayload, dump: &mut String) {
    let _ = writeln!(
        dump,
        "BiquadFilterCommand\n\tinput {:02X} output {:02X} needs_init {} use_float_processing {}",
        payload.input, payload.output, payload.needs_init, payload.use_float_processing
    );
}

pub fn apply_biquad_filter_float(
    mix_buffers: &mut [i32],
    input_range: std::ops::Range<usize>,
    output_range: std::ops::Range<usize>,
    biquad: BiquadFilterParameter,
    state: &mut BiquadFilterState,
) {
    let b = [
        FixedPoint::<50, 14>::from_base(biquad.b[0] as i64).to_f64(),
        FixedPoint::<50, 14>::from_base(biquad.b[1] as i64).to_f64(),
        FixedPoint::<50, 14>::from_base(biquad.b[2] as i64).to_f64(),
    ];
    let a = [
        FixedPoint::<50, 14>::from_base(biquad.a[0] as i64).to_f64(),
        FixedPoint::<50, 14>::from_base(biquad.a[1] as i64).to_f64(),
    ];
    let mut s = [
        f64::from_bits(state.s0 as u64),
        f64::from_bits(state.s1 as u64),
        f64::from_bits(state.s2 as u64),
        f64::from_bits(state.s3 as u64),
    ];

    for i in 0..input_range.len() {
        let input = mix_buffers[input_range.start + i] as f64;
        let sample = input * b[0] + s[0] * b[1] + s[1] * b[2] + s[2] * a[0] + s[3] * a[1];
        mix_buffers[output_range.start + i] = sample.clamp(i32::MIN as f64, i32::MAX as f64) as i32;
        s[1] = s[0];
        s[0] = input;
        s[3] = s[2];
        s[2] = sample;
    }

    state.s0 = s[0].to_bits() as i64;
    state.s1 = s[1].to_bits() as i64;
    state.s2 = s[2].to_bits() as i64;
    state.s3 = s[3].to_bits() as i64;
}

pub fn apply_biquad_filter_float2(
    mix_buffers: &mut [i32],
    input_range: std::ops::Range<usize>,
    output_range: std::ops::Range<usize>,
    biquad: BiquadFilterParameter2,
    state: &mut BiquadFilterState,
) {
    let b = biquad.numerator.map(f64::from);
    let a = biquad.denominator.map(f64::from);
    let mut s = [
        f64::from_bits(state.s0 as u64),
        f64::from_bits(state.s1 as u64),
        f64::from_bits(state.s2 as u64),
        f64::from_bits(state.s3 as u64),
    ];

    for i in 0..input_range.len() {
        let input = mix_buffers[input_range.start + i] as f64;
        let sample = input * b[0] + s[0] * b[1] + s[1] * b[2] + s[2] * a[0] + s[3] * a[1];
        mix_buffers[output_range.start + i] = sample.clamp(i32::MIN as f64, i32::MAX as f64) as i32;
        s[1] = s[0];
        s[0] = input;
        s[3] = s[2];
        s[2] = sample;
    }

    state.s0 = s[0].to_bits() as i64;
    state.s1 = s[1].to_bits() as i64;
    state.s2 = s[2].to_bits() as i64;
    state.s3 = s[3].to_bits() as i64;
}

fn apply_biquad_filter_int(
    mix_buffers: &mut [i32],
    input_range: std::ops::Range<usize>,
    output_range: std::ops::Range<usize>,
    biquad: BiquadFilterParameter,
    state: &mut BiquadFilterState,
) {
    for i in 0..input_range.len() {
        let input = mix_buffers[input_range.start + i] as i64;
        let sample = input
            .saturating_mul(biquad.b[0] as i64)
            .saturating_add(state.s0);
        let out_sample =
            ((sample.saturating_add(1 << 13)) >> 14).clamp(i32::MIN as i64, i32::MAX as i64) as i32;
        mix_buffers[output_range.start + i] = out_sample;
        state.s0 = state
            .s1
            .saturating_add((biquad.b[1] as i64).saturating_mul(input))
            .saturating_add((biquad.a[0] as i64).saturating_mul(out_sample as i64));
        state.s1 = (biquad.b[2] as i64)
            .saturating_mul(input)
            .saturating_add((biquad.a[1] as i64).saturating_mul(out_sample as i64));
    }
}

pub fn read_biquad_state_mut(addr: CpuAddr) -> Option<&'static mut BiquadFilterState> {
    if addr == 0 {
        return None;
    }
    crate::raw_write_trace::maybe_trace_write_at(
        "biquad_filter:state_mut",
        addr,
        std::mem::size_of::<BiquadFilterState>(),
    );
    Some(unsafe { &mut *(addr as *mut BiquadFilterState) })
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
    fn native_float_coefficients_are_used_for_revision_15_voice_filter() {
        let mut buffers = vec![1000, -500, 2000, 0, 0, 0];
        let mut state = BiquadFilterState::default();
        let native = BiquadFilterParameter2 {
            enabled: true,
            numerator: [0.5, 0.0, 0.0],
            denominator: [0.0, 0.0],
            ..Default::default()
        };

        apply_biquad_filter_float2(&mut buffers, 0..3, 3..6, native, &mut state);

        assert_eq!(&buffers[3..6], &[500, -250, 1000]);
    }

    #[test]
    fn revision_15_payload_layout_is_deterministic() {
        assert_eq!(size_of::<BiquadFilterPayload>(), 0x38);
        assert_eq!(offset_of!(BiquadFilterPayload, biquad_float), 0x10);
        assert_eq!(offset_of!(BiquadFilterPayload, state), 0x28);
        assert_eq!(
            offset_of!(BiquadFilterPayload, use_float_coefficients),
            0x32
        );
    }
}
