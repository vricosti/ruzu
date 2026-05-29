use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::guest_write_block;
use crate::renderer::command::util::write_copy;
use std::fmt::Write;
use std::mem::size_of;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CircularBufferSinkPayload {
    pub input_count: u32,
    pub inputs: [i16; MAX_CHANNELS],
    pub address: CpuAddr,
    pub size: u32,
    pub pos: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct CircularBufferSinkCommand {
    pub input_count: u32,
    pub inputs: [i16; MAX_CHANNELS],
    pub address: CpuAddr,
    pub size: u32,
    pub pos: u32,
}

pub fn write_circular_buffer_payload(cmd: &CircularBufferSinkCommand, output: &mut [u8]) -> usize {
    let mut payload: CircularBufferSinkPayload = unsafe { std::mem::zeroed() };
    payload.input_count = cmd.input_count;
    payload.inputs = cmd.inputs;
    payload.address = cmd.address;
    payload.size = cmd.size;
    payload.pos = cmd.pos;
    write_copy(&payload, output)
}

impl CircularBufferSinkPayload {
    /// Port of upstream `CircularBufferSinkCommand::Process`.
    pub fn process(
        mut self,
        payload_addr: CpuAddr,
        sample_count: usize,
        _buffer_count: i16,
        mix_buffers: &[i32],
    ) {
        let mut output = vec![0i16; sample_count];

        for channel in 0..self.input_count as usize {
            let input_idx = self.inputs[channel] as usize;
            let input_samples =
                &mix_buffers[input_idx * sample_count..(input_idx + 1) * sample_count];
            for (dst, &sample) in output.iter_mut().zip(input_samples.iter()) {
                *dst = sample.clamp(i16::MIN as i32, i16::MAX as i32) as i16;
            }

            let output_bytes = unsafe {
                std::slice::from_raw_parts(
                    output.as_ptr() as *const u8,
                    sample_count * size_of::<i16>(),
                )
            };
            let _ = guest_write_block((self.address + self.pos as usize) as u64, output_bytes);
            self.pos += (sample_count * size_of::<i16>()) as u32;
            if self.pos >= self.size {
                self.pos = 0;
            }
        }

        // Write updated payload back to the command list (work buffer, host-mapped).
        crate::raw_write_trace::maybe_trace_write_at(
            "circular_buffer:payload",
            payload_addr as usize,
            std::mem::size_of::<CircularBufferSinkPayload>(),
        );
        unsafe { std::ptr::write_unaligned(payload_addr as *mut CircularBufferSinkPayload, self) };
    }

    /// Port of upstream `CircularBufferSinkCommand::Verify`.
    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = write!(
            dump,
            "CircularBufferSinkCommand\n\tinput_count {} ring size {:04X} ring pos {:04X}\n\tinputs: ",
            self.input_count, self.size, self.pos
        );
        for input in self.inputs.iter().take(self.input_count as usize) {
            let _ = write!(dump, "{:02X}, ", input);
        }
        let _ = writeln!(dump);
    }
}
