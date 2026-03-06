use crate::adsp::apps::audio_renderer::command_list_processor::CommandListProcessor;
use crate::common::common::CpuAddr;
use crate::renderer::command::icommand::CommandId;
use common::fixed_point::FixedPoint;

pub mod clear_mix;
pub mod copy_mix;
pub mod depop_for_mix_buffers;
pub mod depop_prepare;
pub mod mix;
pub mod mix_ramp;
pub mod mix_ramp_grouped;
pub mod volume;
pub mod volume_ramp;

pub use clear_mix::{
    dump_clear_mix_buffer_command, verify_clear_mix_buffer_command, write_clear_mix_buffer_payload,
    ClearMixBufferCommand, ClearMixBufferPayload,
};
pub use copy_mix::{
    dump_copy_mix_buffer_command, process_copy_mix_buffer_command, verify_copy_mix_buffer_command,
    write_copy_mix_buffer_payload, CopyMixBufferCommand, CopyMixBufferPayload,
};
pub use depop_for_mix_buffers::{
    dump_depop_for_mix_buffers_command, process_depop_for_mix_buffers_command,
    verify_depop_for_mix_buffers_command, write_depop_for_mix_buffers_payload,
    DepopForMixBuffersCommand, DepopForMixBuffersPayload,
};
pub use depop_prepare::{
    dump_depop_prepare_command, process_depop_prepare_command, verify_depop_prepare_command,
    write_depop_prepare_payload, DepopPrepareCommand, DepopPreparePayload,
};
pub use mix::{
    dump_mix_command, process_mix_command, verify_mix_command, write_mix_payload, MixCommand,
    MixPayload,
};
pub use mix_ramp::{
    dump_mix_ramp_command, process_mix_ramp_command, verify_mix_ramp_command,
    write_mix_ramp_payload, MixRampCommand, MixRampPayload,
};
pub use mix_ramp_grouped::{
    dump_mix_ramp_grouped_command, process_mix_ramp_grouped_command,
    verify_mix_ramp_grouped_command, write_mix_ramp_grouped_payload, MixRampGroupedCommand,
    MixRampGroupedPayload,
};
pub use volume::{
    dump_volume_command, process_volume_command, verify_volume_command, write_volume_payload,
    VolumeCommand, VolumePayload,
};
pub use volume_ramp::{
    dump_volume_ramp_command, process_volume_ramp_command, verify_volume_ramp_command,
    write_volume_ramp_payload, VolumeRampCommand, VolumeRampPayload,
};

fn read_pod<T: Copy>(addr: CpuAddr) -> Option<T> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { (addr as *const T).read_unaligned() })
}

pub fn dump_mix_family_command(
    command_id: CommandId,
    payload_addr: CpuAddr,
    dump: &mut String,
) -> bool {
    match command_id {
        CommandId::ClearMixBuffer => read_pod::<ClearMixBufferPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::Volume => read_pod::<VolumePayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::VolumeRamp => read_pod::<VolumeRampPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::Mix => read_pod::<MixPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::MixRamp => read_pod::<MixRampPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::MixRampGrouped => read_pod::<MixRampGroupedPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::DepopPrepare => read_pod::<DepopPreparePayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::DepopForMixBuffers => read_pod::<DepopForMixBuffersPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::CopyMixBuffer => read_pod::<CopyMixBufferPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        _ => false,
    }
}

pub fn verify_mix_family_command(command_id: CommandId, payload_addr: CpuAddr) -> Option<bool> {
    match command_id {
        CommandId::ClearMixBuffer => Some(
            read_pod::<ClearMixBufferPayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        CommandId::Volume => {
            Some(read_pod::<VolumePayload>(payload_addr).is_some_and(|payload| payload.verify()))
        }
        CommandId::VolumeRamp => Some(
            read_pod::<VolumeRampPayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        CommandId::Mix => {
            Some(read_pod::<MixPayload>(payload_addr).is_some_and(|payload| payload.verify()))
        }
        CommandId::MixRamp => {
            Some(read_pod::<MixRampPayload>(payload_addr).is_some_and(|payload| payload.verify()))
        }
        CommandId::MixRampGrouped => Some(
            read_pod::<MixRampGroupedPayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        CommandId::DepopPrepare => Some(
            read_pod::<DepopPreparePayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        CommandId::DepopForMixBuffers => Some(
            read_pod::<DepopForMixBuffersPayload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        CommandId::CopyMixBuffer => Some(
            read_pod::<CopyMixBufferPayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        _ => None,
    }
}

pub fn process_mix_family_command(
    processor: &mut CommandListProcessor,
    command_id: CommandId,
    payload_addr: CpuAddr,
) -> bool {
    match command_id {
        CommandId::ClearMixBuffer => {
            let Some(payload) = read_pod::<ClearMixBufferPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::Volume => {
            let Some(payload) = read_pod::<VolumePayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::VolumeRamp => {
            let Some(payload) = read_pod::<VolumeRampPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::Mix => {
            let Some(payload) = read_pod::<MixPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::MixRamp => {
            let Some(payload) = read_pod::<MixRampPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::MixRampGrouped => {
            let Some(payload) = read_pod::<MixRampGroupedPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::DepopPrepare => {
            let Some(payload) = read_pod::<DepopPreparePayload>(payload_addr) else {
                return true;
            };
            payload.process(processor.get_mix_buffer_count());
            true
        }
        CommandId::DepopForMixBuffers => {
            let Some(payload) = read_pod::<DepopForMixBuffersPayload>(payload_addr) else {
                return true;
            };
            let buffer_count = processor.get_mix_buffer_count();
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count, buffer_count);
            });
            true
        }
        CommandId::CopyMixBuffer => {
            let Some(payload) = read_pod::<CopyMixBufferPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        _ => false,
    }
}

pub(crate) fn apply_uniform_gain(
    mix_buffers: &mut [i32],
    sample_count: usize,
    output_index: i16,
    input_index: i16,
    volume: f32,
    precision: u8,
) {
    if input_index == output_index && volume == 1.0 {
        return;
    }
    let Some(input_range) = mix_buffer_range(mix_buffers, input_index, sample_count) else {
        return;
    };
    let Some(output_range) = mix_buffer_range(mix_buffers, output_index, sample_count) else {
        return;
    };
    if volume == 1.0 {
        if input_range != output_range {
            let input = mix_buffers[input_range.clone()].to_vec();
            mix_buffers[output_range].copy_from_slice(&input);
        }
        return;
    }
    let Some(gain_raw) = gain_to_raw(volume, precision) else {
        return;
    };
    for i in 0..sample_count {
        let input_sample = mix_buffers[input_range.start + i];
        let output_sample =
            fixed_raw_to_int((input_sample as i64).saturating_mul(gain_raw), precision);
        mix_buffers[output_range.start + i] = output_sample;
    }
}

pub(crate) fn apply_linear_envelope_gain(
    mix_buffers: &mut [i32],
    sample_count: usize,
    output_index: i16,
    input_index: i16,
    prev_volume: f32,
    volume: f32,
    precision: u8,
) {
    let Some(input_range) = mix_buffer_range(mix_buffers, input_index, sample_count) else {
        return;
    };
    let Some(output_range) = mix_buffer_range(mix_buffers, output_index, sample_count) else {
        return;
    };
    let ramp = (volume - prev_volume) / sample_count.max(1) as f32;
    if input_index == output_index && prev_volume == 1.0 && ramp == 0.0 {
        return;
    }
    if prev_volume == 0.0 && ramp == 0.0 {
        mix_buffers[output_range].fill(0);
        return;
    }

    let Some(mut gain_raw) = gain_to_raw(prev_volume, precision) else {
        return;
    };
    let ramp_raw = gain_to_raw(ramp, precision).unwrap_or(0);
    for i in 0..sample_count {
        let input_sample = mix_buffers[input_range.start + i];
        let output_sample =
            fixed_raw_to_int((input_sample as i64).saturating_mul(gain_raw), precision);
        mix_buffers[output_range.start + i] = output_sample;
        gain_raw = gain_raw.saturating_add(ramp_raw);
    }
}

pub(crate) fn apply_mix(
    mix_buffers: &mut [i32],
    sample_count: usize,
    output_index: i16,
    input_index: i16,
    volume: f32,
    precision: u8,
) {
    if volume == 0.0 {
        return;
    }
    let Some(input_range) = mix_buffer_range(mix_buffers, input_index, sample_count) else {
        return;
    };
    let Some(output_range) = mix_buffer_range(mix_buffers, output_index, sample_count) else {
        return;
    };
    let Some(gain_raw) = gain_to_raw(volume, precision) else {
        return;
    };
    for i in 0..sample_count {
        let input_sample = mix_buffers[input_range.start + i] as i64;
        let output_sample = mix_buffers[output_range.start + i] as i64;
        let mixed_raw = output_sample
            .saturating_mul(1_i64 << precision)
            .saturating_add(input_sample.saturating_mul(gain_raw));
        mix_buffers[output_range.start + i] = fixed_raw_to_int(mixed_raw, precision);
    }
}

pub(crate) fn apply_mix_ramp(
    mix_buffers: &mut [i32],
    sample_count: usize,
    output_index: i16,
    input_index: i16,
    prev_volume: f32,
    volume: f32,
    precision: u8,
) -> i32 {
    let Some(input_range) = mix_buffer_range(mix_buffers, input_index, sample_count) else {
        return 0;
    };
    let Some(output_range) = mix_buffer_range(mix_buffers, output_index, sample_count) else {
        return 0;
    };
    let ramp = (volume - prev_volume) / sample_count.max(1) as f32;
    if prev_volume == 0.0 && ramp == 0.0 {
        return 0;
    }

    let Some(mut gain_raw) = gain_to_raw(prev_volume, precision) else {
        return 0;
    };
    let ramp_raw = gain_to_raw(ramp, precision).unwrap_or(0);
    let mut last_sample = 0;
    for i in 0..sample_count {
        let input_sample = mix_buffers[input_range.start + i] as i64;
        let mixed_sample_raw = input_sample.saturating_mul(gain_raw);
        let output_sample = mix_buffers[output_range.start + i] as i64;
        let mixed_raw = output_sample
            .saturating_mul(1_i64 << precision)
            .saturating_add(mixed_sample_raw);
        last_sample = fixed_raw_to_int(mixed_sample_raw, precision);
        mix_buffers[output_range.start + i] = fixed_raw_to_int(mixed_raw, precision);
        gain_raw = gain_raw.saturating_add(ramp_raw);
    }
    last_sample
}

pub(crate) fn apply_depop_mix(
    mix_buffers: &mut [i32],
    sample_count: usize,
    buffer_index: usize,
    depop_sample: i32,
    decay_raw: i64,
) -> i32 {
    let start = buffer_index.saturating_mul(sample_count);
    let end = start.saturating_add(sample_count).min(mix_buffers.len());
    if start >= end {
        return 0;
    }

    let mut sample = depop_sample.saturating_abs();
    if depop_sample <= 0 {
        for output in &mut mix_buffers[start..end] {
            sample = ((sample as i64).saturating_mul(decay_raw) >> 15) as i32;
            *output = output.saturating_sub(sample);
        }
        -sample
    } else {
        for output in &mut mix_buffers[start..end] {
            sample = ((sample as i64).saturating_mul(decay_raw) >> 15) as i32;
            *output = output.saturating_add(sample);
        }
        sample
    }
}

pub(crate) fn copy_mix_buffer(
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

fn precision_bits(precision: u8) -> Option<u8> {
    match precision {
        15 | 23 => Some(precision),
        _ => None,
    }
}

fn gain_to_raw(volume: f32, precision: u8) -> Option<i64> {
    match precision_bits(precision)? {
        15 => Some(FixedPoint::<49, 15>::from_f32(volume).to_raw()),
        23 => Some(FixedPoint::<41, 23>::from_f32(volume).to_raw()),
        _ => None,
    }
}

fn fixed_raw_to_int(mut data: i64, fractional_bits: u8) -> i32 {
    let fractional_mask = (1_i64 << fractional_bits) - 1;
    let integer_mask = !fractional_mask;
    data = data.saturating_add((data & fractional_mask) >> 1);
    ((data & integer_mask) >> fractional_bits) as i32
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
