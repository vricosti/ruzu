use crate::renderer::command::mix::copy_mix_buffer;
use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CopyMixBufferPayload {
    pub input_index: i16,
    pub output_index: i16,
}

#[derive(Debug, Clone, Copy)]
pub struct CopyMixBufferCommand {
    pub input_index: i16,
    pub output_index: i16,
}

pub fn write_copy_mix_buffer_payload(cmd: &CopyMixBufferCommand, output: &mut [u8]) -> usize {
    let mut payload: CopyMixBufferPayload = unsafe { std::mem::zeroed() };
    payload.input_index = cmd.input_index;
    payload.output_index = cmd.output_index;
    write_copy(&payload, output)
}

impl CopyMixBufferPayload {
    pub fn process(self, mix_buffers: &mut [i32], sample_count: usize) {
        copy_mix_buffer(
            mix_buffers,
            sample_count,
            self.output_index,
            self.input_index,
        );
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(dump, "CopyMixBufferCommand");
        let _ = writeln!(dump, "\tinput {:02X}", self.input_index);
        let _ = writeln!(dump, "\toutput {:02X}", self.output_index);
    }
}

pub fn process_copy_mix_buffer_command(
    payload: &CopyMixBufferPayload,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    copy_mix_buffer(
        mix_buffers,
        sample_count,
        payload.output_index,
        payload.input_index,
    );
}

pub fn verify_copy_mix_buffer_command(_payload: &CopyMixBufferPayload) -> bool {
    true
}

pub fn dump_copy_mix_buffer_command(payload: &CopyMixBufferPayload, dump: &mut String) {
    let _ = writeln!(dump, "CopyMixBufferCommand");
    let _ = writeln!(dump, "\tinput {:02X}", payload.input_index);
    let _ = writeln!(dump, "\toutput {:02X}", payload.output_index);
}
