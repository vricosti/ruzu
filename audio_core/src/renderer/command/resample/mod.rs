use crate::adsp::apps::audio_renderer::command_list_processor::CommandListProcessor;
use crate::common::common::CpuAddr;
use crate::renderer::command::icommand::CommandId;

pub mod downmix_6ch_to_2ch;
pub mod resample;
pub mod upsample;

pub use downmix_6ch_to_2ch::{
    dump_downmix_6ch_to_2ch_command as dump_down_mix_6ch_to_2ch_command,
    process_downmix_6ch_to_2ch_command as process_down_mix_6ch_to_2ch_command,
    verify_downmix_6ch_to_2ch_command as verify_down_mix_6ch_to_2ch_command,
    write_downmix_6ch_to_2ch_payload as write_down_mix_6ch_to_2ch_payload,
};
pub use downmix_6ch_to_2ch::{
    dump_downmix_6ch_to_2ch_command, process_downmix_6ch_to_2ch_command,
    verify_downmix_6ch_to_2ch_command, write_downmix_6ch_to_2ch_payload, DownMix6chTo2chCommand,
    DownMix6chTo2chPayload,
};
pub use resample::{resample, resample_low_quality, resample_normal_quality, src_process_frame};
pub use upsample::{
    dump_upsample_command, process_upsample_command, verify_upsample_command,
    write_upsample_payload, UpsampleCommand, UpsamplePayload,
};

fn read_pod<T: Copy>(addr: CpuAddr) -> Option<T> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { (addr as *const T).read_unaligned() })
}

pub fn dump_resample_command(
    command_id: CommandId,
    payload_addr: CpuAddr,
    dump: &mut String,
) -> bool {
    match command_id {
        CommandId::DownMix6chTo2ch => read_pod::<DownMix6chTo2chPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::Upsample => read_pod::<UpsamplePayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        _ => false,
    }
}

pub fn verify_resample_command(command_id: CommandId, payload_addr: CpuAddr) -> Option<bool> {
    match command_id {
        CommandId::DownMix6chTo2ch => Some(
            read_pod::<DownMix6chTo2chPayload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        CommandId::Upsample => {
            Some(read_pod::<UpsamplePayload>(payload_addr).is_some_and(|payload| payload.verify()))
        }
        _ => None,
    }
}

pub fn process_resample_command(
    processor: &mut CommandListProcessor,
    command_id: CommandId,
    payload_addr: CpuAddr,
) -> bool {
    match command_id {
        CommandId::DownMix6chTo2ch => {
            let Some(payload) = read_pod::<DownMix6chTo2chPayload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count);
            });
            true
        }
        CommandId::Upsample => {
            let Some(payload) = read_pod::<UpsamplePayload>(payload_addr) else {
                return true;
            };
            let buffer_count = processor.get_buffer_count_raw();
            let _ = processor.with_mix_buffers(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, buffer_count, sample_count);
            });
            true
        }
        _ => false,
    }
}
