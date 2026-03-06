use crate::common::common::CpuAddr;
use crate::renderer::command::effect::aux_::{reset_aux_info, write_aux_buffer};
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CapturePayload {
    pub input: i16,
    pub output: i16,
    pub send_buffer_info: CpuAddr,
    pub send_buffer: CpuAddr,
    pub count_max: u32,
    pub write_offset: u32,
    pub update_count: u32,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct CaptureCommand {
    pub input: i16,
    pub output: i16,
    pub send_buffer_info: CpuAddr,
    pub send_buffer: CpuAddr,
    pub count_max: u32,
    pub write_offset: u32,
    pub update_count: u32,
    pub effect_enabled: bool,
}

impl CapturePayload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_capture_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_capture_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_capture_command(self, dump);
    }
}

pub fn write_capture_payload(cmd: &CaptureCommand, output: &mut [u8]) -> usize {
    let mut payload: CapturePayload = unsafe { std::mem::zeroed() };
    payload.input = cmd.input;
    payload.output = cmd.output;
    payload.send_buffer_info = cmd.send_buffer_info;
    payload.send_buffer = cmd.send_buffer;
    payload.count_max = cmd.count_max;
    payload.write_offset = cmd.write_offset;
    payload.update_count = cmd.update_count;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn process_capture_command(
    payload: &CapturePayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let Some(input_range) = mix_buffer_range(mix_buffers, payload.input, sample_count) else {
        return;
    };
    if payload.effect_enabled {
        let input = mix_buffers[input_range].to_vec();
        let _ = write_aux_buffer(
            payload.send_buffer_info,
            payload.send_buffer,
            payload.count_max,
            &input,
            sample_count as u32,
            payload.write_offset,
            payload.update_count,
        );
    } else {
        reset_aux_info(payload.send_buffer_info);
    }
}

pub fn verify_capture_command(_payload: &CapturePayload) -> bool {
    true
}

pub fn dump_capture_command(payload: &CapturePayload, dump: &mut String) {
    let _ = writeln!(
        dump,
        "CaptureCommand\n\tenabled {} input {:02X} output {:02X}",
        payload.effect_enabled, payload.input, payload.output
    );
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
