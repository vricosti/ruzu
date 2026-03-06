use crate::renderer::command::mix::apply_linear_envelope_gain;
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct VolumeRampPayload {
    pub precision: u8,
    pub _padding0: u8,
    pub input_index: i16,
    pub output_index: i16,
    pub _padding1: [u8; 2],
    pub prev_volume: f32,
    pub volume: f32,
}

#[derive(Debug, Clone, Copy)]
pub struct VolumeRampCommand {
    pub precision: u8,
    pub input_index: i16,
    pub output_index: i16,
    pub prev_volume: f32,
    pub volume: f32,
}

pub fn write_volume_ramp_payload(cmd: &VolumeRampCommand, output: &mut [u8]) -> usize {
    let mut payload: VolumeRampPayload = unsafe { std::mem::zeroed() };
    payload.precision = cmd.precision;
    payload._padding0 = 0;
    payload.input_index = cmd.input_index;
    payload.output_index = cmd.output_index;
    payload._padding1 = [0; 2];
    payload.prev_volume = cmd.prev_volume;
    payload.volume = cmd.volume;
    write_copy(&payload, output)
}

impl VolumeRampPayload {
    pub fn process(self, mix_buffers: &mut [i32], sample_count: usize) {
        apply_linear_envelope_gain(
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
        let _ = writeln!(dump, "VolumeRampCommand");
        let _ = writeln!(dump, "\tinput {:02X}", self.input_index);
        let _ = writeln!(dump, "\toutput {:02X}", self.output_index);
        let _ = writeln!(dump, "\tprev_volume {:.8}", self.prev_volume);
        let _ = writeln!(dump, "\tvolume {:.8}", self.volume);
    }
}

pub fn process_volume_ramp_command(
    payload: &VolumeRampPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    apply_linear_envelope_gain(
        mix_buffers,
        sample_count,
        payload.output_index,
        payload.input_index,
        payload.prev_volume,
        payload.volume,
        payload.precision,
    );
}

pub fn verify_volume_ramp_command(_payload: &VolumeRampPayload) -> bool {
    true
}

pub fn dump_volume_ramp_command(payload: &VolumeRampPayload, dump: &mut String) {
    let _ = writeln!(dump, "VolumeRampCommand");
    let _ = writeln!(dump, "\tinput {:02X}", payload.input_index);
    let _ = writeln!(dump, "\toutput {:02X}", payload.output_index);
    let _ = writeln!(dump, "\tprev_volume {:.8}", payload.prev_volume);
    let _ = writeln!(dump, "\tvolume {:.8}", payload.volume);
}
