use crate::renderer::command::util::write_copy;
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ClearMixBufferPayload {
    pub buffer_count: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct ClearMixBufferCommand {
    pub buffer_count: u32,
}

pub fn write_clear_mix_buffer_payload(cmd: &ClearMixBufferCommand, output: &mut [u8]) -> usize {
    let mut payload: ClearMixBufferPayload = unsafe { std::mem::zeroed() };
    payload.buffer_count = cmd.buffer_count;
    write_copy(&payload, output)
}

impl ClearMixBufferPayload {
    pub fn process(self, mix_buffers: &mut [i32], sample_count: usize) {
        let total_samples = sample_count.saturating_mul(self.buffer_count as usize);
        let len = total_samples.min(mix_buffers.len());
        mix_buffers[..len].fill(0);
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(dump, "ClearMixBufferCommand");
    }
}

pub fn verify_clear_mix_buffer_command(_buffer_count: u32) -> bool {
    true
}

pub fn dump_clear_mix_buffer_command(dump: &mut String) {
    let _ = writeln!(dump, "ClearMixBufferCommand");
}
