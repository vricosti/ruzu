use crate::common::common::{CpuAddr, MAX_MIX_BUFFERS};
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DepopPreparePayload {
    pub inputs: [i16; MAX_MIX_BUFFERS as usize],
    pub previous_samples: CpuAddr,
    pub buffer_count: u32,
    pub depop_buffer: CpuAddr,
}

#[derive(Debug, Clone, Copy)]
pub struct DepopPrepareCommand {
    pub inputs: [i16; MAX_MIX_BUFFERS as usize],
    pub previous_samples: CpuAddr,
    pub buffer_count: u32,
    pub depop_buffer: CpuAddr,
}

pub fn write_depop_prepare_payload(cmd: &DepopPrepareCommand, output: &mut [u8]) -> usize {
    let mut payload: DepopPreparePayload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.previous_samples = cmd.previous_samples;
    payload.buffer_count = cmd.buffer_count;
    payload.depop_buffer = cmd.depop_buffer;
    write_copy(&payload, output)
}

impl DepopPreparePayload {
    pub fn process(self, buffer_count: usize) {
        let Some(previous_samples) =
            read_i32_slice_mut(self.previous_samples, self.buffer_count as usize)
        else {
            return;
        };
        let Some(depop_buffer) = read_i32_slice_mut(self.depop_buffer, buffer_count) else {
            return;
        };

        for i in 0..(self.buffer_count as usize).min(self.inputs.len()) {
            let buffer_index = self.inputs[i];
            if buffer_index < 0 {
                continue;
            }
            let buffer_index = buffer_index as usize;
            if buffer_index >= depop_buffer.len() {
                continue;
            }
            let sample = previous_samples[i];
            if sample != 0 {
                depop_buffer[buffer_index] = depop_buffer[buffer_index].saturating_add(sample);
                previous_samples[i] = 0;
            }
        }
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(dump, "DepopPrepareCommand");
        let _ = write!(dump, "\tinputs: ");
        for input in self.inputs.iter().take(self.buffer_count as usize) {
            let _ = write!(dump, "{:02X}, ", input);
        }
        let _ = writeln!(dump);
    }
}

pub fn process_depop_prepare_command(payload: &DepopPreparePayload, buffer_count: usize) {
    let Some(previous_samples) =
        read_i32_slice_mut(payload.previous_samples, payload.buffer_count as usize)
    else {
        return;
    };
    let Some(depop_buffer) = read_i32_slice_mut(payload.depop_buffer, buffer_count) else {
        return;
    };

    for i in 0..(payload.buffer_count as usize).min(payload.inputs.len()) {
        let buffer_index = payload.inputs[i];
        if buffer_index < 0 {
            continue;
        }
        let buffer_index = buffer_index as usize;
        if buffer_index >= depop_buffer.len() {
            continue;
        }
        let sample = previous_samples[i];
        if sample != 0 {
            depop_buffer[buffer_index] = depop_buffer[buffer_index].saturating_add(sample);
            previous_samples[i] = 0;
        }
    }
}

pub fn verify_depop_prepare_command(_payload: &DepopPreparePayload) -> bool {
    true
}

pub fn dump_depop_prepare_command(payload: &DepopPreparePayload, dump: &mut String) {
    let _ = writeln!(dump, "DepopPrepareCommand");
    let _ = write!(dump, "\tinputs: ");
    for input in payload.inputs.iter().take(payload.buffer_count as usize) {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = writeln!(dump);
}

fn read_i32_slice_mut(addr: CpuAddr, len: usize) -> Option<&'static mut [i32]> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts_mut(addr as *mut i32, len) })
}
