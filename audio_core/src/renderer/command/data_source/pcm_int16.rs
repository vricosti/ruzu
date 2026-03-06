use crate::common::common::{CpuAddr, SampleFormat, SrcQuality};
use crate::common::wave_buffer::WaveBufferVersion2;
use crate::renderer::command::data_source::decode::{
    decode_from_wave_buffers, mix_buffer_range, read_voice_state_mut, DataSourceCommand,
    DecodeFromWaveBuffersArgs,
};
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PcmInt16DataSourceVersion1Payload {
    pub src_quality: SrcQuality,
    pub _padding0: u8,
    pub output_index: i16,
    pub flags: u16,
    pub _padding1: [u8; 2],
    pub sample_rate: u32,
    pub pitch: f32,
    pub channel_index: i8,
    pub channel_count: i8,
    pub _padding2: [u8; 6],
    pub wave_buffers: [WaveBufferVersion2; 4],
    pub voice_state: CpuAddr,
}

pub type PcmInt16DataSourceVersion2Payload = PcmInt16DataSourceVersion1Payload;

impl PcmInt16DataSourceVersion1Payload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize, target_sample_rate: u32) {
        process_pcm_int16_data_source_version1_command(
            self,
            mix_buffers,
            sample_count,
            target_sample_rate,
        );
    }

    pub fn verify(&self) -> bool {
        verify_pcm_int16_data_source_version1_command(self)
    }

    pub fn dump(&self, target_sample_rate: u32, dump: &mut String) {
        dump_pcm_int16_data_source_version1_command(self, target_sample_rate, dump);
    }
}

impl PcmInt16DataSourceVersion2Payload {
    pub fn process_v2(
        &self,
        mix_buffers: &mut [i32],
        sample_count: usize,
        target_sample_rate: u32,
    ) {
        process_pcm_int16_data_source_version2_command(
            self,
            mix_buffers,
            sample_count,
            target_sample_rate,
        );
    }

    pub fn verify_v2(&self) -> bool {
        verify_pcm_int16_data_source_version2_command(self)
    }

    pub fn dump_v2(&self, target_sample_rate: u32, dump: &mut String) {
        dump_pcm_int16_data_source_version2_command(self, target_sample_rate, dump);
    }
}

pub fn write_pcm_int16_data_source_version1_payload(
    command: &DataSourceCommand,
    output: &mut [u8],
) -> usize {
    write_pcm_int16_data_source_payload(command, output)
}

pub fn write_pcm_int16_data_source_version2_payload(
    command: &DataSourceCommand,
    output: &mut [u8],
) -> usize {
    write_pcm_int16_data_source_payload(command, output)
}

pub fn process_pcm_int16_data_source_version1_command(
    payload: &PcmInt16DataSourceVersion1Payload,
    mix_buffers: &mut [i32],
    sample_count: usize,
    target_sample_rate: u32,
) {
    process_pcm_int16_data_source(payload, mix_buffers, sample_count, target_sample_rate, true);
}

pub fn process_pcm_int16_data_source_version2_command(
    payload: &PcmInt16DataSourceVersion2Payload,
    mix_buffers: &mut [i32],
    sample_count: usize,
    target_sample_rate: u32,
) {
    process_pcm_int16_data_source(
        payload,
        mix_buffers,
        sample_count,
        target_sample_rate,
        false,
    );
}

pub fn verify_pcm_int16_data_source_version1_command(
    _payload: &PcmInt16DataSourceVersion1Payload,
) -> bool {
    true
}

pub fn verify_pcm_int16_data_source_version2_command(
    _payload: &PcmInt16DataSourceVersion2Payload,
) -> bool {
    true
}

pub fn dump_pcm_int16_data_source_version1_command(
    payload: &PcmInt16DataSourceVersion1Payload,
    target_sample_rate: u32,
    dump: &mut String,
) {
    let _ = writeln!(
        dump,
        "PcmInt16DataSourceVersion1Command\n\toutput_index {:02X} channel {} channel count {} source sample rate {} target sample rate {} src quality {:?}",
        payload.output_index,
        payload.channel_index,
        payload.channel_count,
        payload.sample_rate,
        target_sample_rate,
        payload.src_quality
    );
}

pub fn dump_pcm_int16_data_source_version2_command(
    payload: &PcmInt16DataSourceVersion2Payload,
    target_sample_rate: u32,
    dump: &mut String,
) {
    let _ = writeln!(
        dump,
        "PcmInt16DataSourceVersion2Command\n\toutput_index {:02X} channel {} channel count {} source sample rate {} target sample rate {} src quality {:?}",
        payload.output_index,
        payload.channel_index,
        payload.channel_count,
        payload.sample_rate,
        target_sample_rate,
        payload.src_quality
    );
}

fn write_pcm_int16_data_source_payload(command: &DataSourceCommand, output: &mut [u8]) -> usize {
    let mut payload: PcmInt16DataSourceVersion1Payload = unsafe { std::mem::zeroed() };
    payload.src_quality = command.src_quality;
    payload._padding0 = 0;
    payload.output_index = command.output_index;
    payload.flags = command.flags;
    payload._padding1 = [0; 2];
    payload.sample_rate = command.sample_rate;
    payload.pitch = command.pitch;
    payload.channel_index = command.channel_index;
    payload.channel_count = command.channel_count;
    payload._padding2 = [0; 6];
    payload.wave_buffers = command.wave_buffers;
    payload.voice_state = command.voice_state;
    write_copy(&payload, output)
}

fn process_pcm_int16_data_source(
    payload: &PcmInt16DataSourceVersion1Payload,
    mix_buffers: &mut [i32],
    sample_count: usize,
    target_sample_rate: u32,
    normalize_loop_bounds: bool,
) {
    let Some(output_range) = mix_buffer_range(mix_buffers, payload.output_index, sample_count)
    else {
        return;
    };
    let Some(voice_state) = read_voice_state_mut(payload.voice_state) else {
        return;
    };

    let mut wave_buffers = payload.wave_buffers;
    if normalize_loop_bounds {
        for wave_buffer in &mut wave_buffers {
            wave_buffer.loop_start_offset = wave_buffer.start_offset;
            wave_buffer.loop_end_offset = wave_buffer.end_offset;
            wave_buffer.loop_count = if wave_buffer.looping { -1 } else { 0 };
        }
    }

    decode_from_wave_buffers(DecodeFromWaveBuffersArgs {
        sample_format: SampleFormat::PcmInt16,
        output: &mut mix_buffers[output_range],
        voice_state,
        wave_buffers: &wave_buffers,
        channel: payload.channel_index,
        channel_count: payload.channel_count,
        src_quality: payload.src_quality,
        pitch: payload.pitch,
        source_sample_rate: payload.sample_rate,
        target_sample_rate,
        sample_count: sample_count as u32,
        data_address: 0,
        data_size: 0,
        is_voice_played_sample_count_reset_at_loop_point_supported: (payload.flags & 1) != 0,
        is_voice_pitch_and_src_skipped_supported: (payload.flags & 2) != 0,
    });
}
