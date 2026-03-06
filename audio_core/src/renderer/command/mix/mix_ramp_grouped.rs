use crate::common::common::{CpuAddr, MAX_MIX_BUFFERS};
use crate::renderer::command::mix::apply_mix_ramp;
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MixRampGroupedPayload {
    pub precision: u8,
    pub _padding0: [u8; 3],
    pub buffer_count: u32,
    pub inputs: [i16; MAX_MIX_BUFFERS as usize],
    pub outputs: [i16; MAX_MIX_BUFFERS as usize],
    pub prev_volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub previous_samples: CpuAddr,
}

#[derive(Debug, Clone, Copy)]
pub struct MixRampGroupedCommand {
    pub buffer_count: u32,
    pub precision: u8,
    pub inputs: [i16; MAX_MIX_BUFFERS as usize],
    pub outputs: [i16; MAX_MIX_BUFFERS as usize],
    pub prev_volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub previous_samples: CpuAddr,
}

pub fn write_mix_ramp_grouped_payload(cmd: &MixRampGroupedCommand, output: &mut [u8]) -> usize {
    let mut payload: MixRampGroupedPayload = unsafe { std::mem::zeroed() };
    payload.precision = cmd.precision;
    payload._padding0 = [0; 3];
    payload.buffer_count = cmd.buffer_count;
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.prev_volumes = cmd.prev_volumes;
    payload.volumes = cmd.volumes;
    payload.previous_samples = cmd.previous_samples;
    write_copy(&payload, output)
}

impl MixRampGroupedPayload {
    pub fn process(self, mix_buffers: &mut [i32], sample_count: usize) {
        let Some(previous_samples) =
            read_i32_slice_mut(self.previous_samples, self.prev_volumes.len())
        else {
            return;
        };

        for i in 0..(self.buffer_count as usize).min(self.inputs.len()) {
            previous_samples[i] = apply_mix_ramp(
                mix_buffers,
                sample_count,
                self.outputs[i],
                self.inputs[i],
                self.prev_volumes[i],
                self.volumes[i],
                self.precision,
            );
        }
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(dump, "MixRampGroupedCommand");
        let _ = write!(dump, "\tinputs:  ");
        for input in self.inputs.iter().take(self.buffer_count as usize) {
            let _ = write!(dump, "{:02X}, ", input);
        }
        let _ = write!(dump, "\n\toutputs: ");
        for output in self.outputs.iter().take(self.buffer_count as usize) {
            let _ = write!(dump, "{:02X}, ", output);
        }
        let _ = writeln!(dump);
    }
}

pub fn process_mix_ramp_grouped_command(
    payload: &MixRampGroupedPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let Some(previous_samples) =
        read_i32_slice_mut(payload.previous_samples, payload.prev_volumes.len())
    else {
        return;
    };

    for i in 0..(payload.buffer_count as usize).min(payload.inputs.len()) {
        previous_samples[i] = apply_mix_ramp(
            mix_buffers,
            sample_count,
            payload.outputs[i],
            payload.inputs[i],
            payload.prev_volumes[i],
            payload.volumes[i],
            payload.precision,
        );
    }
}

pub fn verify_mix_ramp_grouped_command(_payload: &MixRampGroupedPayload) -> bool {
    true
}

pub fn dump_mix_ramp_grouped_command(payload: &MixRampGroupedPayload, dump: &mut String) {
    let _ = writeln!(dump, "MixRampGroupedCommand");
    let _ = write!(dump, "\tinputs:  ");
    for input in payload.inputs.iter().take(payload.buffer_count as usize) {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in payload.outputs.iter().take(payload.buffer_count as usize) {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
}

fn read_i32_slice_mut(addr: CpuAddr, len: usize) -> Option<&'static mut [i32]> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts_mut(addr as *mut i32, len) })
}
