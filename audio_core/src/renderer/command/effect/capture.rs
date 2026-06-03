use crate::common::common::CpuAddr;
use crate::renderer::effect::aux_::{AuxBufferInfo, AuxInfoDsp};
use crate::renderer::command::util::write_copy;
use crate::{guest_read_block, guest_write_block};
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
        let _ = write_capture_buffer(
            payload.send_buffer_info,
            payload.send_buffer,
            payload.count_max,
            &input,
            sample_count as u32,
            payload.write_offset,
            payload.update_count,
        );
    } else {
        reset_capture_info(payload.send_buffer_info);
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

fn reset_capture_info(addr: CpuAddr) {
    let Some(mut info) = read_aux_info(addr) else {
        return;
    };
    info.read_offset = 0;
    info.write_offset = 0;
    info.total_sample_count = 0;
    let _ = write_aux_info(addr, &info);
}

fn write_capture_buffer(
    info_addr: CpuAddr,
    buffer_addr: CpuAddr,
    count_max: u32,
    input: &[i32],
    write_count: u32,
    write_offset: u32,
    update_count: u32,
) -> u32 {
    if write_count > count_max || info_addr == 0 || input.is_empty() || buffer_addr == 0 {
        return 0;
    }
    if count_max == 0 {
        return 0;
    }

    let Some(mut info) = read_aux_buffer_info(info_addr) else {
        return 0;
    };

    let mut target_write_offset = info.dsp_info.write_offset + write_offset;
    if target_write_offset > count_max || write_count == 0 {
        return 0;
    }

    let mut remaining = write_count as usize;
    let mut write_pos = 0usize;
    while remaining > 0 {
        let to_write = (count_max - target_write_offset).min(remaining as u32) as usize;
        if to_write > 0 {
            let write_addr =
                buffer_addr + target_write_offset as usize * std::mem::size_of::<i32>();
            let end = write_pos + to_write;
            if !guest_write_block(write_addr as u64, i32_slice_as_bytes(&input[write_pos..end])) {
                return write_count.saturating_sub(remaining as u32);
            }
        }
        target_write_offset = (target_write_offset + to_write as u32) % count_max;
        remaining -= to_write;
        write_pos += to_write;
    }

    if update_count != 0 {
        let count_diff = info
            .dsp_info
            .total_sample_count
            .wrapping_sub(info.cpu_info.total_sample_count);
        if count_diff >= count_max {
            let mut dsp_lost_count = info.dsp_info.lost_sample_count.wrapping_add(update_count);
            if dsp_lost_count.wrapping_sub(info.cpu_info.lost_sample_count)
                < info
                    .dsp_info
                    .lost_sample_count
                    .wrapping_sub(info.cpu_info.lost_sample_count)
            {
                dsp_lost_count = info.cpu_info.lost_sample_count.wrapping_sub(1);
            }
            info.dsp_info.lost_sample_count = dsp_lost_count;
        }

        info.dsp_info.write_offset =
            (info.dsp_info.write_offset + update_count + count_max) % count_max;

        let mut new_sample_count = info.dsp_info.total_sample_count.wrapping_add(update_count);
        if new_sample_count.wrapping_sub(info.cpu_info.total_sample_count) < count_diff {
            new_sample_count = info.cpu_info.total_sample_count.wrapping_sub(1);
        }
        info.dsp_info.total_sample_count = new_sample_count;
    }

    let _ = write_aux_buffer_info(info_addr, &info);
    write_count
}

fn read_aux_info(addr: CpuAddr) -> Option<AuxInfoDsp> {
    if addr == 0 {
        return None;
    }
    let mut info = AuxInfoDsp::default();
    guest_read_block(addr as u64, aux_info_as_bytes_mut(&mut info)).then_some(info)
}

fn write_aux_info(addr: CpuAddr, info: &AuxInfoDsp) -> bool {
    if addr == 0 {
        return false;
    }
    guest_write_block(addr as u64, aux_info_as_bytes(info))
}

fn read_aux_buffer_info(addr: CpuAddr) -> Option<AuxBufferInfo> {
    if addr == 0 {
        return None;
    }
    let mut info = AuxBufferInfo::default();
    guest_read_block(addr as u64, aux_buffer_info_as_bytes_mut(&mut info)).then_some(info)
}

fn write_aux_buffer_info(addr: CpuAddr, info: &AuxBufferInfo) -> bool {
    if addr == 0 {
        return false;
    }
    guest_write_block(addr as u64, aux_buffer_info_as_bytes(info))
}

fn aux_info_as_bytes(info: &AuxInfoDsp) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            info as *const AuxInfoDsp as *const u8,
            std::mem::size_of::<AuxInfoDsp>(),
        )
    }
}

fn aux_info_as_bytes_mut(info: &mut AuxInfoDsp) -> &mut [u8] {
    unsafe {
        std::slice::from_raw_parts_mut(
            info as *mut AuxInfoDsp as *mut u8,
            std::mem::size_of::<AuxInfoDsp>(),
        )
    }
}

fn aux_buffer_info_as_bytes(info: &AuxBufferInfo) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            info as *const AuxBufferInfo as *const u8,
            std::mem::size_of::<AuxBufferInfo>(),
        )
    }
}

fn aux_buffer_info_as_bytes_mut(info: &mut AuxBufferInfo) -> &mut [u8] {
    unsafe {
        std::slice::from_raw_parts_mut(
            info as *mut AuxBufferInfo as *mut u8,
            std::mem::size_of::<AuxBufferInfo>(),
        )
    }
}

fn i32_slice_as_bytes(samples: &[i32]) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            samples.as_ptr() as *const u8,
            std::mem::size_of_val(samples),
        )
    }
}
