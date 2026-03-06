use crate::common::common::CpuAddr;
use crate::renderer::command::mix::apply_mix_ramp;
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MixRampPayload {
    pub precision: u8,
    pub _padding0: u8,
    pub input_index: i16,
    pub output_index: i16,
    pub _padding1: [u8; 2],
    pub prev_volume: f32,
    pub volume: f32,
    pub previous_sample: CpuAddr,
}

#[derive(Debug, Clone, Copy)]
pub struct MixRampCommand {
    pub precision: u8,
    pub input_index: i16,
    pub output_index: i16,
    pub prev_volume: f32,
    pub volume: f32,
    pub previous_sample: CpuAddr,
}

pub fn write_mix_ramp_payload(cmd: &MixRampCommand, output: &mut [u8]) -> usize {
    let mut payload: MixRampPayload = unsafe { std::mem::zeroed() };
    payload.precision = cmd.precision;
    payload._padding0 = 0;
    payload.input_index = cmd.input_index;
    payload.output_index = cmd.output_index;
    payload._padding1 = [0; 2];
    payload.prev_volume = cmd.prev_volume;
    payload.volume = cmd.volume;
    payload.previous_sample = cmd.previous_sample;
    write_copy(&payload, output)
}

impl MixRampPayload {
    pub fn process(self, mix_buffers: &mut [i32], sample_count: usize) {
        let Some(previous_sample) = read_i32_mut(self.previous_sample) else {
            return;
        };
        *previous_sample = apply_mix_ramp(
            mix_buffers,
            sample_count,
            self.output_index,
            self.input_index,
            self.prev_volume,
            self.volume,
            self.precision,
        );
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(dump, "MixRampCommand");
        let _ = writeln!(dump, "\tinput {:02X}", self.input_index);
        let _ = writeln!(dump, "\toutput {:02X}", self.output_index);
        let _ = writeln!(dump, "\tprev_volume {:.8}", self.prev_volume);
        let _ = writeln!(dump, "\tvolume {:.8}", self.volume);
    }
}

pub fn process_mix_ramp_command(
    payload: &MixRampPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let Some(previous_sample) = read_i32_mut(payload.previous_sample) else {
        return;
    };
    *previous_sample = apply_mix_ramp(
        mix_buffers,
        sample_count,
        payload.output_index,
        payload.input_index,
        payload.prev_volume,
        payload.volume,
        payload.precision,
    );
}

pub fn verify_mix_ramp_command(_payload: &MixRampPayload) -> bool {
    true
}

pub fn dump_mix_ramp_command(payload: &MixRampPayload, dump: &mut String) {
    let _ = writeln!(dump, "MixRampCommand");
    let _ = writeln!(dump, "\tinput {:02X}", payload.input_index);
    let _ = writeln!(dump, "\toutput {:02X}", payload.output_index);
    let _ = writeln!(dump, "\tprev_volume {:.8}", payload.prev_volume);
    let _ = writeln!(dump, "\tvolume {:.8}", payload.volume);
}

fn read_i32_mut(addr: CpuAddr) -> Option<&'static mut i32> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { &mut *(addr as *mut i32) })
}
