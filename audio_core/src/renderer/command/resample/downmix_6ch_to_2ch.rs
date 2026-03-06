use crate::common::common::MAX_CHANNELS;
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DownMix6chTo2chPayload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub down_mix_coeff: [i64; 4],
}

#[derive(Debug, Clone, Copy)]
pub struct DownMix6chTo2chCommand {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub down_mix_coeff: [i64; 4],
}

pub fn write_downmix_6ch_to_2ch_payload(cmd: &DownMix6chTo2chCommand, output: &mut [u8]) -> usize {
    let mut payload: DownMix6chTo2chPayload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.down_mix_coeff = cmd.down_mix_coeff;
    write_copy(&payload, output)
}

impl DownMix6chTo2chPayload {
    pub fn process(self, mix_buffers: &mut [i32], sample_count: usize) {
        if sample_count == 0 {
            return;
        }

        let coeffs = self.down_mix_coeff;
        for i in 0..sample_count {
            let in_front_left = mix_buffer_sample(mix_buffers, self.inputs[0], sample_count, i);
            let in_front_right = mix_buffer_sample(mix_buffers, self.inputs[1], sample_count, i);
            let in_center = mix_buffer_sample(mix_buffers, self.inputs[2], sample_count, i);
            let in_lfe = mix_buffer_sample(mix_buffers, self.inputs[3], sample_count, i);
            let in_back_left = mix_buffer_sample(mix_buffers, self.inputs[4], sample_count, i);
            let in_back_right = mix_buffer_sample(mix_buffers, self.inputs[5], sample_count, i);

            let left_sample =
                downmix_sample(in_front_left, in_center, in_lfe, in_back_left, coeffs);
            let right_sample =
                downmix_sample(in_front_right, in_center, in_lfe, in_back_right, coeffs);

            set_mix_buffer_sample(mix_buffers, self.outputs[0], sample_count, i, left_sample);
            set_mix_buffer_sample(mix_buffers, self.outputs[1], sample_count, i, right_sample);
        }

        for &output in &self.outputs[2..] {
            clear_mix_buffer_channel(mix_buffers, output, sample_count);
        }
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = write!(dump, "DownMix6chTo2chCommand\n\tinputs:  ");
        for input in &self.inputs {
            let _ = write!(dump, "{:02X}, ", input);
        }
        let _ = write!(dump, "\n\toutputs: ");
        for output in &self.outputs {
            let _ = write!(dump, "{:02X}, ", output);
        }
        let _ = writeln!(dump);
    }
}

pub fn process_downmix_6ch_to_2ch_command(
    payload: &DownMix6chTo2chPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    if sample_count == 0 {
        return;
    }

    let coeffs = payload.down_mix_coeff;
    for i in 0..sample_count {
        let in_front_left = mix_buffer_sample(mix_buffers, payload.inputs[0], sample_count, i);
        let in_front_right = mix_buffer_sample(mix_buffers, payload.inputs[1], sample_count, i);
        let in_center = mix_buffer_sample(mix_buffers, payload.inputs[2], sample_count, i);
        let in_lfe = mix_buffer_sample(mix_buffers, payload.inputs[3], sample_count, i);
        let in_back_left = mix_buffer_sample(mix_buffers, payload.inputs[4], sample_count, i);
        let in_back_right = mix_buffer_sample(mix_buffers, payload.inputs[5], sample_count, i);

        let left_sample = downmix_sample(in_front_left, in_center, in_lfe, in_back_left, coeffs);
        let right_sample = downmix_sample(in_front_right, in_center, in_lfe, in_back_right, coeffs);

        set_mix_buffer_sample(
            mix_buffers,
            payload.outputs[0],
            sample_count,
            i,
            left_sample,
        );
        set_mix_buffer_sample(
            mix_buffers,
            payload.outputs[1],
            sample_count,
            i,
            right_sample,
        );
    }

    for &output in &payload.outputs[2..] {
        clear_mix_buffer_channel(mix_buffers, output, sample_count);
    }
}

pub fn verify_downmix_6ch_to_2ch_command(_payload: &DownMix6chTo2chPayload) -> bool {
    true
}

pub fn dump_downmix_6ch_to_2ch_command(payload: &DownMix6chTo2chPayload, dump: &mut String) {
    let _ = write!(dump, "DownMix6chTo2chCommand\n\tinputs:  ");
    for input in &payload.inputs {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in &payload.outputs {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
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
    let Some(base) = buffer_index.checked_mul(sample_count) else {
        return 0;
    };
    mix_buffers
        .get(base.saturating_add(sample_index))
        .copied()
        .unwrap_or(0)
}

fn set_mix_buffer_sample(
    mix_buffers: &mut [i32],
    buffer_index: i16,
    sample_count: usize,
    sample_index: usize,
    sample: i32,
) {
    if buffer_index < 0 {
        return;
    }
    let buffer_index = buffer_index as usize;
    let Some(base) = buffer_index.checked_mul(sample_count) else {
        return;
    };
    if let Some(slot) = mix_buffers.get_mut(base.saturating_add(sample_index)) {
        *slot = sample;
    }
}

fn clear_mix_buffer_channel(mix_buffers: &mut [i32], buffer_index: i16, sample_count: usize) {
    if buffer_index < 0 {
        return;
    }
    let buffer_index = buffer_index as usize;
    let Some(base) = buffer_index.checked_mul(sample_count) else {
        return;
    };
    let end = base.saturating_add(sample_count).min(mix_buffers.len());
    mix_buffers[base..end].fill(0);
}

fn downmix_sample(front: i32, center: i32, lfe: i32, back: i32, coeffs: [i64; 4]) -> i32 {
    let sum = (front as i64).saturating_mul(coeffs[0])
        + (center as i64).saturating_mul(coeffs[1])
        + (lfe as i64).saturating_mul(coeffs[2])
        + (back as i64).saturating_mul(coeffs[3]);
    (sum >> 16) as i32
}
