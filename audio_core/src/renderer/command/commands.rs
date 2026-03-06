use crate::adsp::apps::audio_renderer::command_list_processor::CommandListProcessor;
use crate::common::common::CpuAddr;
#[cfg(test)]
use crate::common::common::SrcQuality;
#[cfg(test)]
use crate::common::common::MAX_CHANNELS;
#[cfg(test)]
use crate::common::common::MAX_MIX_BUFFERS;
use crate::renderer::command::data_source::{
    dump_data_source_command, process_data_source_command, verify_data_source_command,
    write_adpcm_data_source_version1_payload, write_adpcm_data_source_version2_payload,
    write_pcm_float_data_source_version1_payload, write_pcm_float_data_source_version2_payload,
    write_pcm_int16_data_source_version1_payload, write_pcm_int16_data_source_version2_payload,
};
pub use crate::renderer::command::data_source::{
    AdpcmDataSourceVersion1Payload, AdpcmDataSourceVersion2Payload, DataSourceCommand,
    PcmFloatDataSourceVersion1Payload, PcmFloatDataSourceVersion2Payload,
    PcmInt16DataSourceVersion1Payload, PcmInt16DataSourceVersion2Payload,
};
use crate::renderer::command::effect::{
    aux_::write_aux_payload,
    biquad_filter::write_biquad_filter_payload,
    capture::write_capture_payload,
    compressor::write_compressor_payload,
    delay::write_delay_payload,
    dump_effect_command,
    i3dl2_reverb::write_i3dl2_reverb_payload,
    light_limiter::{write_light_limiter_v1_payload, write_light_limiter_v2_payload},
    multi_tap_biquad_filter::write_multi_tap_biquad_filter_payload,
    process_effect_command,
    reverb::write_reverb_payload,
    verify_effect_command,
};
pub use crate::renderer::command::effect::{
    AuxCommand, AuxPayload, BiquadFilterCommand, BiquadFilterPayload, CaptureCommand,
    CapturePayload, CompressorCommand, CompressorPayload, DelayCommand, DelayPayload,
    I3dl2ReverbCommand, I3dl2ReverbPayload, LightLimiterVersion1Command,
    LightLimiterVersion1Payload, LightLimiterVersion2Command, LightLimiterVersion2Payload,
    MultiTapBiquadFilterCommand, MultiTapBiquadFilterPayload, ReverbCommand, ReverbPayload,
};
use crate::renderer::command::icommand::{CommandHeader, CommandId};
use crate::renderer::command::mix::{
    dump_mix_family_command, process_mix_family_command, verify_mix_family_command,
    write_clear_mix_buffer_payload, write_copy_mix_buffer_payload,
    write_depop_for_mix_buffers_payload, write_depop_prepare_payload, write_mix_payload,
    write_mix_ramp_grouped_payload, write_mix_ramp_payload, write_volume_payload,
    write_volume_ramp_payload,
};
pub use crate::renderer::command::mix::{
    ClearMixBufferCommand, ClearMixBufferPayload, CopyMixBufferCommand, CopyMixBufferPayload,
    DepopForMixBuffersCommand, DepopForMixBuffersPayload, DepopPrepareCommand, DepopPreparePayload,
    MixCommand, MixPayload, MixRampCommand, MixRampGroupedCommand, MixRampGroupedPayload,
    MixRampPayload, VolumeCommand, VolumePayload, VolumeRampCommand, VolumeRampPayload,
};
use crate::renderer::command::performance::{
    dump_performance_family_command, process_performance_family_command,
    verify_performance_family_command, write_performance_payload,
};
pub use crate::renderer::command::performance::{PerformanceCommand, PerformancePayload};
use crate::renderer::command::resample::{
    dump_resample_command, process_resample_command, verify_resample_command,
    write_downmix_6ch_to_2ch_payload, write_upsample_payload,
};
pub use crate::renderer::command::resample::{
    DownMix6chTo2chCommand, DownMix6chTo2chPayload, UpsampleCommand, UpsamplePayload,
};
use crate::renderer::command::sink::{
    dump_sink_command, process_sink_command, verify_sink_command, write_circular_buffer_payload,
    write_device_payload,
};
pub use crate::renderer::command::sink::{
    CircularBufferSinkCommand, CircularBufferSinkPayload, DeviceSinkCommand, DeviceSinkPayload,
};
#[cfg(test)]
use crate::renderer::effect::{light_limiter, reverb};

#[derive(Debug, Clone, Copy)]
pub enum Command {
    DataSourcePcmInt16Version1(DataSourceCommand),
    DataSourcePcmInt16Version2(DataSourceCommand),
    DataSourcePcmFloatVersion1(DataSourceCommand),
    DataSourcePcmFloatVersion2(DataSourceCommand),
    DataSourceAdpcmVersion1(DataSourceCommand),
    DataSourceAdpcmVersion2(DataSourceCommand),
    Volume(VolumeCommand),
    VolumeRamp(VolumeRampCommand),
    BiquadFilter(BiquadFilterCommand),
    Mix(MixCommand),
    MixRamp(MixRampCommand),
    MixRampGrouped(MixRampGroupedCommand),
    DepopPrepare(DepopPrepareCommand),
    DepopForMixBuffers(DepopForMixBuffersCommand),
    Delay(DelayCommand),
    Upsample(UpsampleCommand),
    DownMix6chTo2ch(DownMix6chTo2chCommand),
    Aux(AuxCommand),
    DeviceSink(DeviceSinkCommand),
    CircularBufferSink(CircularBufferSinkCommand),
    Reverb(ReverbCommand),
    I3dl2Reverb(I3dl2ReverbCommand),
    Performance(PerformanceCommand),
    ClearMixBuffer(ClearMixBufferCommand),
    CopyMixBuffer(CopyMixBufferCommand),
    LightLimiterVersion1(LightLimiterVersion1Command),
    LightLimiterVersion2(LightLimiterVersion2Command),
    MultiTapBiquadFilter(MultiTapBiquadFilterCommand),
    Capture(CaptureCommand),
    Compressor(CompressorCommand),
}

impl Command {
    pub fn is_voice_data_source(&self) -> bool {
        matches!(
            self,
            Self::DataSourcePcmInt16Version1(_)
                | Self::DataSourcePcmInt16Version2(_)
                | Self::DataSourcePcmFloatVersion1(_)
                | Self::DataSourcePcmFloatVersion2(_)
                | Self::DataSourceAdpcmVersion1(_)
                | Self::DataSourceAdpcmVersion2(_)
        )
    }

    pub fn is_performance(&self) -> bool {
        matches!(self, Self::Performance(_))
    }

    pub fn is_depop_prepare(&self) -> bool {
        matches!(self, Self::DepopPrepare(_))
    }

    pub fn id(&self) -> CommandId {
        match self {
            Self::DataSourcePcmInt16Version1(_) => CommandId::DataSourcePcmInt16Version1,
            Self::DataSourcePcmInt16Version2(_) => CommandId::DataSourcePcmInt16Version2,
            Self::DataSourcePcmFloatVersion1(_) => CommandId::DataSourcePcmFloatVersion1,
            Self::DataSourcePcmFloatVersion2(_) => CommandId::DataSourcePcmFloatVersion2,
            Self::DataSourceAdpcmVersion1(_) => CommandId::DataSourceAdpcmVersion1,
            Self::DataSourceAdpcmVersion2(_) => CommandId::DataSourceAdpcmVersion2,
            Self::Volume(_) => CommandId::Volume,
            Self::VolumeRamp(_) => CommandId::VolumeRamp,
            Self::BiquadFilter(_) => CommandId::BiquadFilter,
            Self::Mix(_) => CommandId::Mix,
            Self::MixRamp(_) => CommandId::MixRamp,
            Self::MixRampGrouped(_) => CommandId::MixRampGrouped,
            Self::DepopPrepare(_) => CommandId::DepopPrepare,
            Self::DepopForMixBuffers(_) => CommandId::DepopForMixBuffers,
            Self::Delay(_) => CommandId::Delay,
            Self::Upsample(_) => CommandId::Upsample,
            Self::DownMix6chTo2ch(_) => CommandId::DownMix6chTo2ch,
            Self::Aux(_) => CommandId::Aux,
            Self::DeviceSink(_) => CommandId::DeviceSink,
            Self::CircularBufferSink(_) => CommandId::CircularBufferSink,
            Self::Reverb(_) => CommandId::Reverb,
            Self::I3dl2Reverb(_) => CommandId::I3dl2Reverb,
            Self::Performance(_) => CommandId::Performance,
            Self::ClearMixBuffer(_) => CommandId::ClearMixBuffer,
            Self::CopyMixBuffer(_) => CommandId::CopyMixBuffer,
            Self::LightLimiterVersion1(_) => CommandId::LightLimiterVersion1,
            Self::LightLimiterVersion2(_) => CommandId::LightLimiterVersion2,
            Self::MultiTapBiquadFilter(_) => CommandId::MultiTapBiquadFilter,
            Self::Capture(_) => CommandId::Capture,
            Self::Compressor(_) => CommandId::Compressor,
        }
    }

    pub fn payload_size(&self) -> usize {
        match self {
            Self::DataSourcePcmInt16Version1(_) | Self::DataSourcePcmInt16Version2(_) => {
                std::mem::size_of::<PcmInt16DataSourceVersion1Payload>()
            }
            Self::DataSourcePcmFloatVersion1(_) | Self::DataSourcePcmFloatVersion2(_) => {
                std::mem::size_of::<PcmFloatDataSourceVersion1Payload>()
            }
            Self::DataSourceAdpcmVersion1(_) => {
                std::mem::size_of::<AdpcmDataSourceVersion1Payload>()
            }
            Self::DataSourceAdpcmVersion2(_) => {
                std::mem::size_of::<AdpcmDataSourceVersion2Payload>()
            }
            Self::Volume(_) => std::mem::size_of::<VolumePayload>(),
            Self::VolumeRamp(_) => std::mem::size_of::<VolumeRampPayload>(),
            Self::BiquadFilter(_) => std::mem::size_of::<BiquadFilterPayload>(),
            Self::Mix(_) => std::mem::size_of::<MixPayload>(),
            Self::MixRamp(_) => std::mem::size_of::<MixRampPayload>(),
            Self::MixRampGrouped(_) => std::mem::size_of::<MixRampGroupedPayload>(),
            Self::DepopPrepare(_) => std::mem::size_of::<DepopPreparePayload>(),
            Self::DepopForMixBuffers(_) => std::mem::size_of::<DepopForMixBuffersPayload>(),
            Self::Delay(_) => std::mem::size_of::<DelayPayload>(),
            Self::Upsample(_) => std::mem::size_of::<UpsamplePayload>(),
            Self::DownMix6chTo2ch(_) => std::mem::size_of::<DownMix6chTo2chPayload>(),
            Self::Aux(_) => std::mem::size_of::<AuxPayload>(),
            Self::DeviceSink(_) => std::mem::size_of::<DeviceSinkPayload>(),
            Self::CircularBufferSink(_) => std::mem::size_of::<CircularBufferSinkPayload>(),
            Self::Reverb(_) => std::mem::size_of::<ReverbPayload>(),
            Self::I3dl2Reverb(_) => std::mem::size_of::<I3dl2ReverbPayload>(),
            Self::Performance(_) => std::mem::size_of::<PerformancePayload>(),
            Self::ClearMixBuffer(_) => std::mem::size_of::<ClearMixBufferPayload>(),
            Self::CopyMixBuffer(_) => std::mem::size_of::<CopyMixBufferPayload>(),
            Self::LightLimiterVersion1(_) => std::mem::size_of::<LightLimiterVersion1Payload>(),
            Self::LightLimiterVersion2(_) => std::mem::size_of::<LightLimiterVersion2Payload>(),
            Self::MultiTapBiquadFilter(_) => std::mem::size_of::<MultiTapBiquadFilterPayload>(),
            Self::Capture(_) => std::mem::size_of::<CapturePayload>(),
            Self::Compressor(_) => std::mem::size_of::<CompressorPayload>(),
        }
    }

    pub fn serialized_size(&self) -> usize {
        std::mem::size_of::<CommandHeader>() + self.payload_size()
    }

    pub fn write_payload(&self, output: &mut [u8]) -> usize {
        match self {
            Self::DataSourcePcmInt16Version1(cmd) => {
                write_pcm_int16_data_source_version1_payload(cmd, output)
            }
            Self::DataSourcePcmInt16Version2(cmd) => {
                write_pcm_int16_data_source_version2_payload(cmd, output)
            }
            Self::DataSourcePcmFloatVersion1(cmd) => {
                write_pcm_float_data_source_version1_payload(cmd, output)
            }
            Self::DataSourcePcmFloatVersion2(cmd) => {
                write_pcm_float_data_source_version2_payload(cmd, output)
            }
            Self::DataSourceAdpcmVersion1(cmd) => {
                write_adpcm_data_source_version1_payload(cmd, output)
            }
            Self::DataSourceAdpcmVersion2(cmd) => {
                write_adpcm_data_source_version2_payload(cmd, output)
            }
            Self::Volume(cmd) => write_volume_payload(cmd, output),
            Self::VolumeRamp(cmd) => write_volume_ramp_payload(cmd, output),
            Self::BiquadFilter(cmd) => write_biquad_filter_payload(cmd, output),
            Self::Mix(cmd) => write_mix_payload(cmd, output),
            Self::MixRamp(cmd) => write_mix_ramp_payload(cmd, output),
            Self::MixRampGrouped(cmd) => write_mix_ramp_grouped_payload(cmd, output),
            Self::DepopPrepare(cmd) => write_depop_prepare_payload(cmd, output),
            Self::DepopForMixBuffers(cmd) => write_depop_for_mix_buffers_payload(cmd, output),
            Self::Delay(cmd) => write_delay_payload(cmd, output),
            Self::Upsample(cmd) => write_upsample_payload(cmd, output),
            Self::DownMix6chTo2ch(cmd) => write_downmix_6ch_to_2ch_payload(cmd, output),
            Self::Aux(cmd) => write_aux_payload(cmd, output),
            Self::DeviceSink(cmd) => write_device_payload(cmd, output),
            Self::CircularBufferSink(cmd) => write_circular_buffer_payload(cmd, output),
            Self::Reverb(cmd) => write_reverb_payload(cmd, output),
            Self::I3dl2Reverb(cmd) => write_i3dl2_reverb_payload(cmd, output),
            Self::Performance(cmd) => write_performance_payload(cmd, output),
            Self::ClearMixBuffer(cmd) => write_clear_mix_buffer_payload(cmd, output),
            Self::CopyMixBuffer(cmd) => write_copy_mix_buffer_payload(cmd, output),
            Self::LightLimiterVersion1(cmd) => write_light_limiter_v1_payload(cmd, output),
            Self::LightLimiterVersion2(cmd) => write_light_limiter_v2_payload(cmd, output),
            Self::MultiTapBiquadFilter(cmd) => write_multi_tap_biquad_filter_payload(cmd, output),
            Self::Capture(cmd) => write_capture_payload(cmd, output),
            Self::Compressor(cmd) => write_compressor_payload(cmd, output),
        }
    }
}

pub fn dump_command(
    header: &CommandHeader,
    payload_addr: CpuAddr,
    target_sample_rate: u32,
    dump: &mut String,
) {
    if dump_data_source_command(header.type_, payload_addr, target_sample_rate, dump)
        || dump_effect_command(header.type_, payload_addr, dump)
        || dump_mix_family_command(header.type_, payload_addr, dump)
        || dump_resample_command(header.type_, payload_addr, dump)
        || dump_sink_command(header.type_, payload_addr, dump)
        || dump_performance_family_command(header.type_, payload_addr, dump)
    {
        return;
    }

    CommandListProcessor::append_command_dump(dump, header);
}

pub fn verify_command(header: &CommandHeader, payload_addr: CpuAddr) -> bool {
    if let Some(result) = verify_data_source_command(header.type_, payload_addr) {
        return result;
    }
    if let Some(result) = verify_effect_command(header.type_, payload_addr) {
        return result;
    }
    if let Some(result) = verify_mix_family_command(header.type_, payload_addr) {
        return result;
    }
    if let Some(result) = verify_resample_command(header.type_, payload_addr) {
        return result;
    }
    if let Some(result) = verify_sink_command(header.type_, payload_addr) {
        return result;
    }
    if let Some(result) = verify_performance_family_command(header.type_, payload_addr) {
        return result;
    }

    false
}

pub fn process_command(
    processor: &mut CommandListProcessor,
    header: &CommandHeader,
    payload_addr: CpuAddr,
) {
    if process_data_source_command(processor, header.type_, payload_addr)
        || process_effect_command(processor, header.type_, payload_addr)
        || process_mix_family_command(processor, header.type_, payload_addr)
        || process_resample_command(processor, header.type_, payload_addr)
        || process_sink_command(processor, header.type_, payload_addr)
        || process_performance_family_command(processor, header.type_, payload_addr)
    {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::wave_buffer::{WaveBufferVersion1, WaveBufferVersion2};
    use crate::renderer::voice::voice_info::BiquadFilterParameter;

    #[test]
    fn upsample_payload_serializes_unk_20() {
        let command = Command::Upsample(UpsampleCommand {
            samples_buffer: 0x1111,
            inputs: 0x2222,
            buffer_count: 4,
            unk_20: 0x3344_5566,
            source_sample_count: 160,
            source_sample_rate: 32_000,
            upsampler_info: 0x3333,
        });
        let mut output = [0u8; std::mem::size_of::<UpsamplePayload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(
            u32::from_le_bytes(output[20..24].try_into().unwrap()),
            0x3344_5566
        );
    }

    #[test]
    fn device_payload_zeroes_padding_bytes() {
        let command = Command::DeviceSink(DeviceSinkCommand {
            name: [0; 0x100],
            session_id: 7,
            sample_buffer: 0x1234_5678,
            sample_count: 320,
            input_count: 2,
            inputs: [1, 2, 0, 0, 0, 0],
        });
        let mut output = [0xAAu8; std::mem::size_of::<DeviceSinkPayload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(std::mem::size_of::<DeviceSinkPayload>(), 296);
        assert_eq!(
            u32::from_le_bytes(output[0x104..0x108].try_into().unwrap()),
            0
        );
    }

    #[test]
    fn aux_payload_zeroes_tail_padding_bytes() {
        let command = Command::Aux(AuxCommand {
            input: 1,
            output: 2,
            send_buffer_info: 0x11,
            return_buffer_info: 0x22,
            send_buffer: 0x33,
            return_buffer: 0x44,
            count_max: 10,
            write_offset: 20,
            update_count: 30,
            effect_enabled: true,
        });
        let mut output = [0xAAu8; std::mem::size_of::<AuxPayload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(&output[written - 7..written], &[0; 7]);
    }

    #[test]
    fn reverb_payload_zeroes_tail_padding_bytes() {
        let command = Command::Reverb(ReverbCommand {
            inputs: [0; MAX_CHANNELS],
            outputs: [1; MAX_CHANNELS],
            parameter: reverb::ParameterVersion2::default(),
            state: 0x55,
            workbuffer: 0x66,
            effect_enabled: true,
            long_size_pre_delay_supported: true,
        });
        let mut output = [0xAAu8; std::mem::size_of::<ReverbPayload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(&output[written - 6..written], &[0; 6]);
    }

    #[test]
    fn capture_payload_zeroes_tail_padding_bytes() {
        let command = Command::Capture(CaptureCommand {
            input: 1,
            output: 2,
            send_buffer_info: 0x11,
            send_buffer: 0x22,
            count_max: 10,
            write_offset: 20,
            update_count: 30,
            effect_enabled: true,
        });
        let mut output = [0xAAu8; std::mem::size_of::<CapturePayload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(&output[written - 7..written], &[0; 7]);
    }

    #[test]
    fn biquad_payload_zeroes_tail_padding_bytes() {
        let command = Command::BiquadFilter(BiquadFilterCommand {
            input: 1,
            output: 2,
            biquad: BiquadFilterParameter::default(),
            state: 0x44,
            needs_init: true,
            use_float_processing: false,
        });
        let mut output = [0xAAu8; std::mem::size_of::<BiquadFilterPayload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(&output[written - 6..written], &[0; 6]);
    }

    #[test]
    fn pcm_data_source_payload_zeroes_reserved_bytes() {
        let command = Command::DataSourcePcmInt16Version2(DataSourceCommand {
            src_quality: SrcQuality::Medium,
            output_index: 3,
            flags: 5,
            sample_rate: 48_000,
            pitch: 1.0,
            channel_index: 1,
            channel_count: 2,
            wave_buffers: [WaveBufferVersion2::default(); 4],
            voice_state: 0x1234,
            data_address: 0,
            data_size: 0,
        });
        let mut output = [0xAAu8; std::mem::size_of::<PcmInt16DataSourceVersion2Payload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(output[1], 0);
        assert_eq!(&output[6..8], &[0; 2]);
        assert_eq!(&output[18..24], &[0; 6]);
    }

    #[test]
    fn light_limiter_v2_payload_zeroes_tail_padding_bytes() {
        let command = Command::LightLimiterVersion2(LightLimiterVersion2Command {
            inputs: [0; MAX_CHANNELS],
            outputs: [0; MAX_CHANNELS],
            parameter: light_limiter::ParameterVersion2::default(),
            state: 0x11,
            workbuffer: 0x22,
            result_state: 0x33,
            effect_enabled: true,
        });
        let mut output = [0xAAu8; std::mem::size_of::<LightLimiterVersion2Payload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(&output[written - 7..written], &[0; 7]);
    }

    #[test]
    fn wave_buffer_payload_sizes_match_cpp_layout() {
        assert_eq!(std::mem::size_of::<WaveBufferVersion1>(), 48);
        assert_eq!(std::mem::size_of::<WaveBufferVersion2>(), 56);
    }

    #[test]
    fn mix_ramp_grouped_payload_serializes_precision_before_buffer_count() {
        let command = Command::MixRampGrouped(MixRampGroupedCommand {
            buffer_count: 0x1122_3344,
            precision: 23,
            inputs: [0; MAX_MIX_BUFFERS as usize],
            outputs: [0; MAX_MIX_BUFFERS as usize],
            prev_volumes: [0.0; MAX_MIX_BUFFERS as usize],
            volumes: [0.0; MAX_MIX_BUFFERS as usize],
            previous_samples: 0x55,
        });
        let mut output = [0xAAu8; std::mem::size_of::<MixRampGroupedPayload>()];

        let written = command.write_payload(&mut output);

        assert_eq!(written, output.len());
        assert_eq!(output[0], 23);
        assert_eq!(&output[1..4], &[0; 3]);
        assert_eq!(
            u32::from_le_bytes(output[4..8].try_into().unwrap()),
            0x1122_3344
        );
    }
}
