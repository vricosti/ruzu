use crate::renderer::command::mix::apply_mix;
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MixPayload {
    pub precision: u8,
    pub _padding0: u8,
    pub input_index: i16,
    pub output_index: i16,
    pub _padding1: [u8; 2],
    pub volume: f32,
}

#[derive(Debug, Clone, Copy)]
pub struct MixCommand {
    pub precision: u8,
    pub input_index: i16,
    pub output_index: i16,
    pub volume: f32,
}

pub fn write_mix_payload(cmd: &MixCommand, output: &mut [u8]) -> usize {
    let mut payload: MixPayload = unsafe { std::mem::zeroed() };
    payload.precision = cmd.precision;
    payload._padding0 = 0;
    payload.input_index = cmd.input_index;
    payload.output_index = cmd.output_index;
    payload._padding1 = [0; 2];
    payload.volume = cmd.volume;
    write_copy(&payload, output)
}

impl MixPayload {
    pub fn process(self, mix_buffers: &mut [i32], sample_count: usize) {
        apply_mix(
            mix_buffers,
            sample_count,
            self.output_index,
            self.input_index,
            self.volume,
            self.precision,
        );
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(dump, "MixCommand");
        let _ = writeln!(dump, "\tinput {:02X}", self.input_index);
        let _ = writeln!(dump, "\toutput {:02X}", self.output_index);
        let _ = writeln!(dump, "\tvolume {:.8}", self.volume);
    }
}

pub fn process_mix_command(payload: &MixPayload, mix_buffers: &mut [i32], sample_count: usize) {
    apply_mix(
        mix_buffers,
        sample_count,
        payload.output_index,
        payload.input_index,
        payload.volume,
        payload.precision,
    );
}

pub fn verify_mix_command(_payload: &MixPayload) -> bool {
    true
}

pub fn dump_mix_command(payload: &MixPayload, dump: &mut String) {
    let _ = writeln!(dump, "MixCommand");
    let _ = writeln!(dump, "\tinput {:02X}", payload.input_index);
    let _ = writeln!(dump, "\toutput {:02X}", payload.output_index);
    let _ = writeln!(dump, "\tvolume {:.8}", payload.volume);
}
