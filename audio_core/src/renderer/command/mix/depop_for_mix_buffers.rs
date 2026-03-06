use crate::common::common::CpuAddr;
use crate::renderer::command::mix::apply_depop_mix;
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
pub struct DepopForMixBuffersCommand {
    pub input: u32,
    pub count: u32,
    pub decay: i64,
    pub depop_buffer: CpuAddr,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DepopForMixBuffersPayload {
    pub input: u32,
    pub count: u32,
    pub decay: i64,
    pub depop_buffer: CpuAddr,
}

pub fn write_depop_for_mix_buffers_payload(
    cmd: &DepopForMixBuffersCommand,
    output: &mut [u8],
) -> usize {
    let mut payload: DepopForMixBuffersPayload = unsafe { std::mem::zeroed() };
    payload.input = cmd.input;
    payload.count = cmd.count;
    payload.decay = cmd.decay;
    payload.depop_buffer = cmd.depop_buffer;
    write_copy(&payload, output)
}

impl DepopForMixBuffersPayload {
    pub fn process(self, mix_buffers: &mut [i32], sample_count: usize, buffer_count: usize) {
        let buffer_end = (self.input.saturating_add(self.count)).min(buffer_count as u32) as usize;
        let Some(depop_buffer) = read_i32_slice_mut(self.depop_buffer, buffer_end) else {
            return;
        };

        for buffer_index in self.input as usize..buffer_end {
            let depop_sample = depop_buffer[buffer_index];
            if depop_sample == 0 {
                continue;
            }
            depop_buffer[buffer_index] = apply_depop_mix(
                mix_buffers,
                sample_count,
                buffer_index,
                depop_sample,
                self.decay,
            );
        }
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(dump, "DepopForMixBuffersCommand");
        let _ = writeln!(dump, "\tinput {:02X}", self.input);
        let _ = writeln!(dump, "\tcount {}", self.count);
    }
}

pub fn process_depop_for_mix_buffers_command(
    payload: &DepopForMixBuffersPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
    buffer_count: usize,
) {
    let buffer_end =
        (payload.input.saturating_add(payload.count)).min(buffer_count as u32) as usize;
    let Some(depop_buffer) = read_i32_slice_mut(payload.depop_buffer, buffer_end) else {
        return;
    };

    for buffer_index in payload.input as usize..buffer_end {
        let depop_sample = depop_buffer[buffer_index];
        if depop_sample == 0 {
            continue;
        }
        depop_buffer[buffer_index] = apply_depop_mix(
            mix_buffers,
            sample_count,
            buffer_index,
            depop_sample,
            payload.decay,
        );
    }
}

pub fn verify_depop_for_mix_buffers_command(_payload: &DepopForMixBuffersPayload) -> bool {
    true
}

pub fn dump_depop_for_mix_buffers_command(payload: &DepopForMixBuffersPayload, dump: &mut String) {
    let _ = writeln!(dump, "DepopForMixBuffersCommand");
    let _ = writeln!(dump, "\tinput {:02X}", payload.input);
    let _ = writeln!(dump, "\tcount {}", payload.count);
}

fn read_i32_slice_mut(addr: CpuAddr, len: usize) -> Option<&'static mut [i32]> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts_mut(addr as *mut i32, len) })
}
