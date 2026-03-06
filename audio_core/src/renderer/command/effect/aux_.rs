use crate::common::common::CpuAddr;
use crate::renderer::command::mix::copy_mix_buffer;
use crate::renderer::command::util::write_copy;
use crate::renderer::effect::aux_::AuxInfoDsp;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct AuxPayload {
    pub input: i16,
    pub output: i16,
    pub send_buffer_info: CpuAddr,
    pub return_buffer_info: CpuAddr,
    pub send_buffer: CpuAddr,
    pub return_buffer: CpuAddr,
    pub count_max: u32,
    pub write_offset: u32,
    pub update_count: u32,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct AuxCommand {
    pub input: i16,
    pub output: i16,
    pub send_buffer_info: CpuAddr,
    pub return_buffer_info: CpuAddr,
    pub send_buffer: CpuAddr,
    pub return_buffer: CpuAddr,
    pub count_max: u32,
    pub write_offset: u32,
    pub update_count: u32,
    pub effect_enabled: bool,
}

impl AuxPayload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_aux_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_aux_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_aux_command(self, dump);
    }
}

pub fn write_aux_payload(cmd: &AuxCommand, output: &mut [u8]) -> usize {
    let mut payload: AuxPayload = unsafe { std::mem::zeroed() };
    payload.input = cmd.input;
    payload.output = cmd.output;
    payload.send_buffer_info = cmd.send_buffer_info;
    payload.return_buffer_info = cmd.return_buffer_info;
    payload.send_buffer = cmd.send_buffer;
    payload.return_buffer = cmd.return_buffer;
    payload.count_max = cmd.count_max;
    payload.write_offset = cmd.write_offset;
    payload.update_count = cmd.update_count;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn process_aux_command(payload: &AuxPayload, mix_buffers: &mut [i32], sample_count: usize) {
    let Some(input_range) = mix_buffer_range(mix_buffers, payload.input, sample_count) else {
        return;
    };
    let Some(output_range) = mix_buffer_range(mix_buffers, payload.output, sample_count) else {
        return;
    };
    if payload.effect_enabled {
        let input = mix_buffers[input_range.clone()].to_vec();
        let written = write_aux_buffer(
            payload.send_buffer_info,
            payload.send_buffer,
            payload.count_max,
            &input,
            sample_count as u32,
            payload.write_offset,
            payload.update_count,
        );
        let read = read_aux_buffer(
            payload.return_buffer_info,
            payload.return_buffer,
            payload.count_max,
            &mut mix_buffers[output_range.clone()],
            sample_count as u32,
            payload.write_offset,
            payload.update_count,
        );
        if written == 0 || read < sample_count as u32 {
            for sample in &mut mix_buffers[output_range.start + read as usize..output_range.end] {
                *sample = 0;
            }
        }
    } else {
        reset_aux_info(payload.send_buffer_info);
        reset_aux_info(payload.return_buffer_info);
        copy_mix_buffer(mix_buffers, sample_count, payload.output, payload.input);
    }
}

pub fn verify_aux_command(_payload: &AuxPayload) -> bool {
    true
}

pub fn dump_aux_command(payload: &AuxPayload, dump: &mut String) {
    let _ = writeln!(
        dump,
        "AuxCommand\n\tenabled {} input {:02X} output {:02X}",
        payload.effect_enabled, payload.input, payload.output
    );
}

pub(crate) fn reset_aux_info(addr: CpuAddr) {
    let Some(info) = read_aux_info_mut(addr) else {
        return;
    };
    info.read_offset = 0;
    info.write_offset = 0;
    info.total_sample_count = 0;
}

pub(crate) fn write_aux_buffer(
    info_addr: CpuAddr,
    buffer_addr: CpuAddr,
    count_max: u32,
    input: &[i32],
    write_count: u32,
    write_offset: u32,
    update_count: u32,
) -> u32 {
    if info_addr == 0 || buffer_addr == 0 || count_max == 0 || input.is_empty() || write_count == 0
    {
        return 0;
    }
    if write_count > count_max {
        return 0;
    }

    let Some(info) = read_aux_info_mut(info_addr) else {
        return 0;
    };

    let mut target_write_offset = info.write_offset.saturating_add(write_offset);
    if target_write_offset > count_max {
        return 0;
    }

    let buffer_len = count_max as usize;
    let buffer = unsafe { std::slice::from_raw_parts_mut(buffer_addr as *mut i32, buffer_len) };
    let mut remaining = write_count as usize;
    let mut read_pos = 0usize;
    while remaining > 0 {
        let available = buffer_len.saturating_sub(target_write_offset as usize);
        let to_write = available.min(remaining);
        if to_write == 0 {
            break;
        }
        let end = read_pos.saturating_add(to_write).min(input.len());
        if end <= read_pos {
            break;
        }
        buffer[target_write_offset as usize..target_write_offset as usize + (end - read_pos)]
            .copy_from_slice(&input[read_pos..end]);
        target_write_offset = (target_write_offset + (end - read_pos) as u32) % count_max;
        remaining -= end - read_pos;
        read_pos = end;
    }

    if update_count != 0 {
        info.write_offset = (info.write_offset + update_count) % count_max;
        info.total_sample_count = info.total_sample_count.saturating_add(update_count);
    }

    write_count.saturating_sub(remaining as u32)
}

pub(crate) fn read_aux_buffer(
    info_addr: CpuAddr,
    buffer_addr: CpuAddr,
    count_max: u32,
    output: &mut [i32],
    read_count: u32,
    read_offset: u32,
    update_count: u32,
) -> u32 {
    if info_addr == 0 || buffer_addr == 0 || count_max == 0 || output.is_empty() || read_count == 0
    {
        return 0;
    }
    if read_count > count_max {
        return 0;
    }

    let Some(info) = read_aux_info_mut(info_addr) else {
        return 0;
    };

    let mut target_read_offset = info.read_offset.saturating_add(read_offset);
    if target_read_offset > count_max {
        return 0;
    }

    let buffer_len = count_max as usize;
    let buffer = unsafe { std::slice::from_raw_parts(buffer_addr as *const i32, buffer_len) };
    let mut remaining = read_count as usize;
    let mut write_pos = 0usize;
    while remaining > 0 && write_pos < output.len() {
        let available = buffer_len.saturating_sub(target_read_offset as usize);
        let to_read = available.min(remaining).min(output.len() - write_pos);
        if to_read == 0 {
            break;
        }
        output[write_pos..write_pos + to_read].copy_from_slice(
            &buffer[target_read_offset as usize..target_read_offset as usize + to_read],
        );
        target_read_offset = (target_read_offset + to_read as u32) % count_max;
        remaining -= to_read;
        write_pos += to_read;
    }

    if update_count != 0 {
        info.read_offset = (info.read_offset + update_count) % count_max;
    }

    read_count.saturating_sub(remaining as u32)
}

fn read_aux_info_mut(addr: CpuAddr) -> Option<&'static mut AuxInfoDsp> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { &mut *(addr as *mut AuxInfoDsp) })
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
