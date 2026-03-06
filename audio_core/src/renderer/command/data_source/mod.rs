use crate::adsp::apps::audio_renderer::command_list_processor::CommandListProcessor;
use crate::common::common::CpuAddr;
use crate::renderer::command::icommand::CommandId;

pub mod adpcm;
pub mod decode;
pub mod pcm_float;
pub mod pcm_int16;

pub use adpcm::{
    dump_adpcm_data_source_version1_command, dump_adpcm_data_source_version2_command,
    process_adpcm_data_source_version1_command, process_adpcm_data_source_version2_command,
    verify_adpcm_data_source_version1_command, verify_adpcm_data_source_version2_command,
    write_adpcm_data_source_version1_payload, write_adpcm_data_source_version2_payload,
    AdpcmDataSourceVersion1Payload, AdpcmDataSourceVersion2Payload,
};
pub use decode::{decode_from_wave_buffers, DataSourceCommand, DecodeFromWaveBuffersArgs};
pub use pcm_float::{
    dump_pcm_float_data_source_version1_command, dump_pcm_float_data_source_version2_command,
    process_pcm_float_data_source_version1_command, process_pcm_float_data_source_version2_command,
    verify_pcm_float_data_source_version1_command, verify_pcm_float_data_source_version2_command,
    write_pcm_float_data_source_version1_payload, write_pcm_float_data_source_version2_payload,
    PcmFloatDataSourceVersion1Payload, PcmFloatDataSourceVersion2Payload,
};
pub use pcm_int16::{
    dump_pcm_int16_data_source_version1_command, dump_pcm_int16_data_source_version2_command,
    process_pcm_int16_data_source_version1_command, process_pcm_int16_data_source_version2_command,
    verify_pcm_int16_data_source_version1_command, verify_pcm_int16_data_source_version2_command,
    write_pcm_int16_data_source_version1_payload, write_pcm_int16_data_source_version2_payload,
    PcmInt16DataSourceVersion1Payload, PcmInt16DataSourceVersion2Payload,
};

fn read_pod<T: Copy>(addr: CpuAddr) -> Option<T> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { (addr as *const T).read_unaligned() })
}

pub fn dump_data_source_command(
    command_id: CommandId,
    payload_addr: CpuAddr,
    target_sample_rate: u32,
    dump: &mut String,
) -> bool {
    match command_id {
        CommandId::DataSourcePcmInt16Version1 => {
            read_pod::<PcmInt16DataSourceVersion1Payload>(payload_addr)
                .map(|payload| payload.dump(target_sample_rate, dump))
                .is_some()
        }
        CommandId::DataSourcePcmInt16Version2 => {
            read_pod::<PcmInt16DataSourceVersion2Payload>(payload_addr)
                .map(|payload| payload.dump_v2(target_sample_rate, dump))
                .is_some()
        }
        CommandId::DataSourcePcmFloatVersion1 => {
            read_pod::<PcmFloatDataSourceVersion1Payload>(payload_addr)
                .map(|payload| payload.dump(target_sample_rate, dump))
                .is_some()
        }
        CommandId::DataSourcePcmFloatVersion2 => {
            read_pod::<PcmFloatDataSourceVersion2Payload>(payload_addr)
                .map(|payload| payload.dump_v2(target_sample_rate, dump))
                .is_some()
        }
        CommandId::DataSourceAdpcmVersion1 => {
            read_pod::<AdpcmDataSourceVersion1Payload>(payload_addr)
                .map(|payload| payload.dump(target_sample_rate, dump))
                .is_some()
        }
        CommandId::DataSourceAdpcmVersion2 => {
            read_pod::<AdpcmDataSourceVersion2Payload>(payload_addr)
                .map(|payload| payload.dump(target_sample_rate, dump))
                .is_some()
        }
        _ => false,
    }
}

pub fn verify_data_source_command(command_id: CommandId, payload_addr: CpuAddr) -> Option<bool> {
    match command_id {
        CommandId::DataSourcePcmInt16Version1 => Some(
            read_pod::<PcmInt16DataSourceVersion1Payload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        CommandId::DataSourcePcmInt16Version2 => Some(
            read_pod::<PcmInt16DataSourceVersion2Payload>(payload_addr)
                .is_some_and(|payload| payload.verify_v2()),
        ),
        CommandId::DataSourcePcmFloatVersion1 => Some(
            read_pod::<PcmFloatDataSourceVersion1Payload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        CommandId::DataSourcePcmFloatVersion2 => Some(
            read_pod::<PcmFloatDataSourceVersion2Payload>(payload_addr)
                .is_some_and(|payload| payload.verify_v2()),
        ),
        CommandId::DataSourceAdpcmVersion1 => Some(
            read_pod::<AdpcmDataSourceVersion1Payload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        CommandId::DataSourceAdpcmVersion2 => Some(
            read_pod::<AdpcmDataSourceVersion2Payload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        _ => None,
    }
}

pub fn process_data_source_command(
    processor: &mut CommandListProcessor,
    command_id: CommandId,
    payload_addr: CpuAddr,
) -> bool {
    let sample_rate = processor.get_target_sample_rate();
    match command_id {
        CommandId::DataSourcePcmInt16Version1 => {
            let Some(payload) = read_pod::<PcmInt16DataSourceVersion1Payload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count, sample_rate);
            });
            true
        }
        CommandId::DataSourcePcmInt16Version2 => {
            let Some(payload) = read_pod::<PcmInt16DataSourceVersion2Payload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process_v2(mix_buffers, sample_count, sample_rate);
            });
            true
        }
        CommandId::DataSourcePcmFloatVersion1 => {
            let Some(payload) = read_pod::<PcmFloatDataSourceVersion1Payload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count, sample_rate);
            });
            true
        }
        CommandId::DataSourcePcmFloatVersion2 => {
            let Some(payload) = read_pod::<PcmFloatDataSourceVersion2Payload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process_v2(mix_buffers, sample_count, sample_rate);
            });
            true
        }
        CommandId::DataSourceAdpcmVersion1 => {
            let Some(payload) = read_pod::<AdpcmDataSourceVersion1Payload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count, sample_rate);
            });
            true
        }
        CommandId::DataSourceAdpcmVersion2 => {
            let Some(payload) = read_pod::<AdpcmDataSourceVersion2Payload>(payload_addr) else {
                return true;
            };
            let _ = processor.with_mix_buffers_mut(|mix_buffers, sample_count, _| {
                payload.process(mix_buffers, sample_count, sample_rate);
            });
            true
        }
        _ => false,
    }
}
