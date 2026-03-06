use crate::adsp::apps::audio_renderer::command_list_processor::CommandListProcessor;
use crate::common::common::CpuAddr;
use crate::renderer::command::icommand::CommandId;

pub mod aux_;
pub mod biquad_filter;
pub mod capture;
pub mod compressor;
pub mod delay;
pub mod i3dl2_reverb;
pub mod light_limiter;
pub mod multi_tap_biquad_filter;
pub mod reverb;

pub use aux_::{dump_aux_command, process_aux_command, verify_aux_command, AuxCommand, AuxPayload};
pub use biquad_filter::{
    dump_biquad_filter_command, process_biquad_filter_command, verify_biquad_filter_command,
    BiquadFilterCommand, BiquadFilterPayload,
};
pub use capture::{
    dump_capture_command, process_capture_command, verify_capture_command, CaptureCommand,
    CapturePayload,
};
pub use compressor::{
    dump_compressor_command, process_compressor_command, verify_compressor_command,
    CompressorCommand, CompressorPayload,
};
pub use delay::{
    dump_delay_command, process_delay_command, verify_delay_command, DelayCommand, DelayPayload,
};
pub use i3dl2_reverb::{
    dump_i3dl2_reverb_command, process_i3dl2_reverb_command, verify_i3dl2_reverb_command,
    I3dl2ReverbCommand, I3dl2ReverbPayload,
};
pub use light_limiter::{
    dump_light_limiter_v1_command, dump_light_limiter_v2_command, process_light_limiter_v1_command,
    process_light_limiter_v2_command, verify_light_limiter_v1_command,
    verify_light_limiter_v2_command, LightLimiterVersion1Command, LightLimiterVersion1Payload,
    LightLimiterVersion2Command, LightLimiterVersion2Payload,
};
pub use multi_tap_biquad_filter::{
    dump_multi_tap_biquad_filter_command, process_multi_tap_biquad_filter_command,
    verify_multi_tap_biquad_filter_command, MultiTapBiquadFilterCommand,
    MultiTapBiquadFilterPayload,
};
pub use reverb::{
    dump_reverb_command, process_reverb_command, verify_reverb_command, ReverbCommand,
    ReverbPayload,
};

fn read_pod<T: Copy>(addr: CpuAddr) -> Option<T> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { (addr as *const T).read_unaligned() })
}

pub fn dump_effect_command(
    command_id: CommandId,
    payload_addr: CpuAddr,
    dump: &mut String,
) -> bool {
    match command_id {
        CommandId::Aux => read_pod::<AuxPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::BiquadFilter => read_pod::<BiquadFilterPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::Capture => read_pod::<CapturePayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::Delay => read_pod::<DelayPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::Reverb => read_pod::<ReverbPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::I3dl2Reverb => read_pod::<I3dl2ReverbPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::LightLimiterVersion1 => read_pod::<LightLimiterVersion1Payload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::LightLimiterVersion2 => read_pod::<LightLimiterVersion2Payload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::Compressor => read_pod::<CompressorPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::MultiTapBiquadFilter => read_pod::<MultiTapBiquadFilterPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        _ => false,
    }
}

pub fn verify_effect_command(command_id: CommandId, payload_addr: CpuAddr) -> Option<bool> {
    match command_id {
        CommandId::Aux => {
            Some(read_pod::<AuxPayload>(payload_addr).is_some_and(|payload| payload.verify()))
        }
        CommandId::BiquadFilter => Some(
            read_pod::<BiquadFilterPayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        CommandId::Capture => {
            Some(read_pod::<CapturePayload>(payload_addr).is_some_and(|payload| payload.verify()))
        }
        CommandId::Delay => {
            Some(read_pod::<DelayPayload>(payload_addr).is_some_and(|payload| payload.verify()))
        }
        CommandId::Reverb => {
            Some(read_pod::<ReverbPayload>(payload_addr).is_some_and(|payload| payload.verify()))
        }
        CommandId::I3dl2Reverb => Some(
            read_pod::<I3dl2ReverbPayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        CommandId::LightLimiterVersion1 => Some(
            read_pod::<LightLimiterVersion1Payload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        CommandId::LightLimiterVersion2 => Some(
            read_pod::<LightLimiterVersion2Payload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        CommandId::Compressor => Some(
            read_pod::<CompressorPayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        CommandId::MultiTapBiquadFilter => Some(
            read_pod::<MultiTapBiquadFilterPayload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        _ => None,
    }
}

pub fn process_effect_command(
    processor: &mut CommandListProcessor,
    command_id: CommandId,
    payload_addr: CpuAddr,
) -> bool {
    match command_id {
        CommandId::Aux => {
            let Some(payload) = read_pod::<AuxPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::BiquadFilter => {
            let Some(payload) = read_pod::<BiquadFilterPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::Capture => {
            let Some(payload) = read_pod::<CapturePayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::Delay => {
            let Some(payload) = read_pod::<DelayPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::Reverb => {
            let Some(payload) = read_pod::<ReverbPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::I3dl2Reverb => {
            let Some(payload) = read_pod::<I3dl2ReverbPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::LightLimiterVersion1 => {
            let Some(payload) = read_pod::<LightLimiterVersion1Payload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::LightLimiterVersion2 => {
            let Some(payload) = read_pod::<LightLimiterVersion2Payload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::Compressor => {
            let Some(payload) = read_pod::<CompressorPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::MultiTapBiquadFilter => {
            let Some(payload) = read_pod::<MultiTapBiquadFilterPayload>(payload_addr) else {
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
