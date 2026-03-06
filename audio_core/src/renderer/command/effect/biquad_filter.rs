use crate::common::common::CpuAddr;
use crate::renderer::command::util::write_copy;
use crate::renderer::voice::voice_info::BiquadFilterParameter;
use crate::renderer::voice::voice_state::BiquadFilterState;
use common::fixed_point::FixedPoint;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct BiquadFilterPayload {
    pub input: i16,
    pub output: i16,
    pub biquad: BiquadFilterParameter,
    pub state: CpuAddr,
    pub needs_init: bool,
    pub use_float_processing: bool,
    pub _padding0: [u8; 6],
}

#[derive(Debug, Clone, Copy)]
pub struct BiquadFilterCommand {
    pub input: i16,
    pub output: i16,
    pub biquad: BiquadFilterParameter,
    pub state: CpuAddr,
    pub needs_init: bool,
    pub use_float_processing: bool,
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
    payload.state = cmd.state;
    payload.needs_init = cmd.needs_init;
    payload.use_float_processing = cmd.use_float_processing;
    payload._padding0 = [0; 6];
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
    apply_biquad_filter(
        mix_buffers,
        input_range,
        output_range,
        payload.biquad,
        state,
        payload.use_float_processing,
    );
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

pub fn apply_biquad_filter(
    mix_buffers: &mut [i32],
    input_range: std::ops::Range<usize>,
    output_range: std::ops::Range<usize>,
    biquad: BiquadFilterParameter,
    state: &mut BiquadFilterState,
    use_float_processing: bool,
) {
    if use_float_processing {
        let b = [
            FixedPoint::<50, 14>::from_base(biquad.b[0] as i64).to_f64(),
            FixedPoint::<50, 14>::from_base(biquad.b[1] as i64).to_f64(),
            FixedPoint::<50, 14>::from_base(biquad.b[2] as i64).to_f64(),
        ];
        let a = [
            FixedPoint::<50, 14>::from_base(biquad.a[0] as i64).to_f64(),
            FixedPoint::<50, 14>::from_base(biquad.a[1] as i64).to_f64(),
        ];
        let mut s0 = f64::from_bits(state.s0 as u64);
        let mut s1 = f64::from_bits(state.s1 as u64);
        let mut s2 = f64::from_bits(state.s2 as u64);
        let mut s3 = f64::from_bits(state.s3 as u64);

        for i in 0..input_range.len() {
            let input = mix_buffers[input_range.start + i] as f64;
            let sample = input * b[0] + s0 * b[1] + s1 * b[2] + s2 * a[0] + s3 * a[1];
            mix_buffers[output_range.start + i] =
                sample.clamp(i32::MIN as f64, i32::MAX as f64) as i32;
            s1 = s0;
            s0 = input;
            s3 = s2;
            s2 = sample;
        }

        state.s0 = s0.to_bits() as i64;
        state.s1 = s1.to_bits() as i64;
        state.s2 = s2.to_bits() as i64;
        state.s3 = s3.to_bits() as i64;
    } else {
        for i in 0..input_range.len() {
            let input = mix_buffers[input_range.start + i] as i64;
            let sample = input
                .saturating_mul(biquad.b[0] as i64)
                .saturating_add(state.s0);
            let out_sample = ((sample.saturating_add(1 << 13)) >> 14)
                .clamp(i32::MIN as i64, i32::MAX as i64) as i32;
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
}

pub fn read_biquad_state_mut(addr: CpuAddr) -> Option<&'static mut BiquadFilterState> {
    if addr == 0 {
        return None;
    }
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
