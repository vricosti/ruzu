use crate::common::common::{CpuAddr, MAX_CHANNELS};
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
    pub fn process(
        mut self,
        payload_addr: CpuAddr,
        sample_count: usize,
        buffer_count: i16,
        mix_buffers: &[i32],
    ) {
        if sample_count == 0 || self.address == 0 || self.size == 0 {
            return;
        }

        let input_count = self.input_count.max(1).min(buffer_count.max(1) as u32) as usize;
        if self.inputs.iter().take(input_count).any(|&input| input < 0) {
            return;
        }
        let ring_size = self.size as usize;
        let bytes_per_channel = sample_count.saturating_mul(size_of::<i16>());
        if bytes_per_channel == 0 {
            return;
        }

        let mut pos = (self.pos as usize) % ring_size;
        let ring = unsafe { std::slice::from_raw_parts_mut(self.address as *mut u8, ring_size) };
        let mut output = vec![0i16; sample_count];

        for &input in self.inputs.iter().take(input_count) {
            if input >= buffer_count {
                continue;
            }

            let input_index = input as usize;
            let input_samples =
                &mix_buffers[input_index * sample_count..(input_index + 1) * sample_count];
            for (dst, &sample) in output.iter_mut().zip(input_samples.iter()) {
                *dst = sample.clamp(i16::MIN as i32, i16::MAX as i32) as i16;
            }

            let output_bytes = unsafe {
                std::slice::from_raw_parts(output.as_ptr() as *const u8, bytes_per_channel)
            };
            let first_write = bytes_per_channel.min(ring_size.saturating_sub(pos));
            ring[pos..pos + first_write].copy_from_slice(&output_bytes[..first_write]);
            if first_write < bytes_per_channel {
                let remaining = bytes_per_channel - first_write;
                ring[..remaining].copy_from_slice(&output_bytes[first_write..]);
            }

            pos = pos.saturating_add(bytes_per_channel);
            if pos >= ring_size {
                pos = 0;
            }
        }

        self.pos = pos as u32;
        unsafe { std::ptr::write_unaligned(payload_addr as *mut CircularBufferSinkPayload, self) };
    }

    pub fn verify(self) -> bool {
        !self
            .inputs
            .iter()
            .take(self.input_count as usize)
            .any(|&input| input < 0)
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

pub fn process_circular_buffer_command(
    payload_addr: CpuAddr,
    sample_count: usize,
    buffer_count: i16,
    mix_buffers: &[i32],
) {
    let Some(mut payload) = read_pod::<CircularBufferSinkPayload>(payload_addr) else {
        return;
    };
    if sample_count == 0 || payload.address == 0 || payload.size == 0 {
        return;
    }

    let input_count = payload.input_count.max(1).min(buffer_count.max(1) as u32) as usize;
    if payload
        .inputs
        .iter()
        .take(input_count)
        .any(|&input| input < 0)
    {
        return;
    }
    let ring_size = payload.size as usize;
    let bytes_per_channel = sample_count.saturating_mul(size_of::<i16>());
    if bytes_per_channel == 0 {
        return;
    }

    let mut pos = (payload.pos as usize) % ring_size;
    let ring = unsafe { std::slice::from_raw_parts_mut(payload.address as *mut u8, ring_size) };
    let mut output = vec![0i16; sample_count];

    for &input in payload.inputs.iter().take(input_count) {
        if input >= buffer_count {
            continue;
        }

        let input_index = input as usize;
        let input_samples =
            &mix_buffers[input_index * sample_count..(input_index + 1) * sample_count];
        for (dst, &sample) in output.iter_mut().zip(input_samples.iter()) {
            *dst = sample.clamp(i16::MIN as i32, i16::MAX as i32) as i16;
        }

        let output_bytes =
            unsafe { std::slice::from_raw_parts(output.as_ptr() as *const u8, bytes_per_channel) };
        let first_write = bytes_per_channel.min(ring_size.saturating_sub(pos));
        ring[pos..pos + first_write].copy_from_slice(&output_bytes[..first_write]);
        if first_write < bytes_per_channel {
            let remaining = bytes_per_channel - first_write;
            ring[..remaining].copy_from_slice(&output_bytes[first_write..]);
        }

        pos = pos.saturating_add(bytes_per_channel);
        if pos >= ring_size {
            pos = 0;
        }
    }

    payload.pos = pos as u32;
    unsafe { std::ptr::write_unaligned(payload_addr as *mut CircularBufferSinkPayload, payload) };
}

pub fn verify_circular_buffer_command(payload: &CircularBufferSinkPayload) -> bool {
    !payload
        .inputs
        .iter()
        .take(payload.input_count as usize)
        .any(|&input| input < 0)
}

pub fn dump_circular_buffer_command(payload: &CircularBufferSinkPayload, dump: &mut String) {
    let _ = write!(
        dump,
        "CircularBufferSinkCommand\n\tinput_count {} ring size {:04X} ring pos {:04X}\n\tinputs: ",
        payload.input_count, payload.size, payload.pos
    );
    for input in payload.inputs.iter().take(payload.input_count as usize) {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = writeln!(dump);
}

fn read_pod<T: Copy>(addr: CpuAddr) -> Option<T> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { (addr as *const T).read_unaligned() })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_rejects_negative_input_indices() {
        let payload = CircularBufferSinkPayload {
            input_count: 2,
            inputs: [-1, 1, 0, 0, 0, 0],
            ..unsafe { std::mem::zeroed() }
        };

        assert!(!payload.verify());
        assert!(!verify_circular_buffer_command(&payload));
    }
}
