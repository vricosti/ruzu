use super::resample::src_process_frame;
use crate::common::common::CpuAddr;
use crate::renderer::command::util::write_copy;
use crate::renderer::upsampler::UpsamplerInfo;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct UpsamplePayload {
    pub samples_buffer: CpuAddr,
    pub inputs: CpuAddr,
    pub buffer_count: u32,
    pub unk_20: u32,
    pub source_sample_count: u32,
    pub source_sample_rate: u32,
    pub upsampler_info: CpuAddr,
}

#[derive(Debug, Clone, Copy)]
pub struct UpsampleCommand {
    pub samples_buffer: CpuAddr,
    pub inputs: CpuAddr,
    pub buffer_count: u32,
    pub unk_20: u32,
    pub source_sample_count: u32,
    pub source_sample_rate: u32,
    pub upsampler_info: CpuAddr,
}

pub fn write_upsample_payload(cmd: &UpsampleCommand, output: &mut [u8]) -> usize {
    let mut payload: UpsamplePayload = unsafe { std::mem::zeroed() };
    payload.samples_buffer = cmd.samples_buffer;
    payload.inputs = cmd.inputs;
    payload.buffer_count = cmd.buffer_count;
    payload.unk_20 = cmd.unk_20;
    payload.source_sample_count = cmd.source_sample_count;
    payload.source_sample_rate = cmd.source_sample_rate;
    payload.upsampler_info = cmd.upsampler_info;
    write_copy(&payload, output)
}

impl UpsamplePayload {
    pub fn process(self, mix_buffers: &[i32], buffer_count: i16, sample_count: usize) {
        if self.upsampler_info == 0 || self.inputs == 0 || self.samples_buffer == 0 {
            return;
        }

        let info = unsafe { &mut *(self.upsampler_info as *mut UpsamplerInfo) };
        let input_count = info.input_count.min(self.buffer_count) as usize;
        let inputs = unsafe { std::slice::from_raw_parts(self.inputs as *const i16, input_count) };

        for (channel_index, &channel) in inputs.iter().enumerate() {
            if channel < 0 || channel >= buffer_count {
                continue;
            }
            let buffer_index = channel as usize;
            let output_offset = info.sample_count as usize * buffer_index;
            let output = unsafe {
                std::slice::from_raw_parts_mut(
                    (self.samples_buffer as *mut i32).add(output_offset),
                    info.sample_count as usize,
                )
            };
            let input =
                &mix_buffers[buffer_index * sample_count..(buffer_index + 1) * sample_count];
            src_process_frame(
                output,
                input,
                info.sample_count,
                self.source_sample_count,
                &mut info.states[channel_index],
            );
        }
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(
            dump,
            "UpsampleCommand\n\tsource_sample_count {} source_sample_rate {}",
            self.source_sample_count, self.source_sample_rate
        );
    }
}

pub fn process_upsample_command(
    payload: &UpsamplePayload,
    mix_buffers: &[i32],
    buffer_count: i16,
    sample_count: usize,
) {
    if payload.upsampler_info == 0 || payload.inputs == 0 || payload.samples_buffer == 0 {
        return;
    }

    let info = unsafe { &mut *(payload.upsampler_info as *mut UpsamplerInfo) };
    let input_count = info.input_count.min(payload.buffer_count) as usize;
    let inputs = unsafe { std::slice::from_raw_parts(payload.inputs as *const i16, input_count) };

    for (channel_index, &channel) in inputs.iter().enumerate() {
        if channel < 0 || channel >= buffer_count {
            continue;
        }
        let buffer_index = channel as usize;
        let output_offset = info.sample_count as usize * buffer_index;
        let output = unsafe {
            std::slice::from_raw_parts_mut(
                (payload.samples_buffer as *mut i32).add(output_offset),
                info.sample_count as usize,
            )
        };
        let input = &mix_buffers[buffer_index * sample_count..(buffer_index + 1) * sample_count];
        src_process_frame(
            output,
            input,
            info.sample_count,
            payload.source_sample_count,
            &mut info.states[channel_index],
        );
    }
}

pub fn verify_upsample_command(_payload: &UpsamplePayload) -> bool {
    true
}

pub fn dump_upsample_command(payload: &UpsamplePayload, dump: &mut String) {
    let _ = writeln!(
        dump,
        "UpsampleCommand\n\tsource_sample_count {} source_sample_rate {}",
        payload.source_sample_count, payload.source_sample_rate
    );
}
