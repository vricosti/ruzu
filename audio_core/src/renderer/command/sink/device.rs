use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::command::util::write_copy;
use crate::sink::sink_stream::{SinkBuffer, SinkStreamHandle};
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DeviceSinkPayload {
    pub name: [u8; 0x100],
    pub session_id: i32,
    pub _padding0: u32,
    pub sample_buffer: CpuAddr,
    pub sample_count: u64,
    pub input_count: u32,
    pub inputs: [i16; MAX_CHANNELS],
}

#[derive(Debug, Clone, Copy)]
pub struct DeviceSinkCommand {
    pub name: [u8; 0x100],
    pub session_id: i32,
    pub sample_buffer: CpuAddr,
    pub sample_count: u64,
    pub input_count: u32,
    pub inputs: [i16; MAX_CHANNELS],
}

pub fn write_device_payload(cmd: &DeviceSinkCommand, output: &mut [u8]) -> usize {
    let mut payload: DeviceSinkPayload = unsafe { std::mem::zeroed() };
    payload.name = cmd.name;
    payload.session_id = cmd.session_id;
    payload._padding0 = 0;
    payload.sample_buffer = cmd.sample_buffer;
    payload.sample_count = cmd.sample_count;
    payload.input_count = cmd.input_count;
    payload.inputs = cmd.inputs;
    write_copy(&payload, output)
}

impl DeviceSinkPayload {
    pub fn process(self, stream: &SinkStreamHandle, buffer_count: i16) {
        let input_count = self.input_count.max(1).min(buffer_count.max(1) as u32) as usize;
        if self.inputs.iter().take(input_count).any(|&input| input < 0) {
            return;
        }
        let max_input = self
            .inputs
            .iter()
            .take(input_count)
            .copied()
            .max()
            .unwrap_or(0) as usize;
        let planar_buffer_count = max_input.saturating_add(1).max(1);
        let sample_count = self.sample_count as usize;
        let frames = (sample_count / planar_buffer_count).max(1);
        let src =
            unsafe { std::slice::from_raw_parts(self.sample_buffer as *const i32, sample_count) };
        let mut samples = Vec::with_capacity(frames.saturating_mul(input_count));
        for frame in 0..frames {
            for &input in self.inputs.iter().take(input_count) {
                let buffer_index = input as usize;
                let sample = src[buffer_index * frames + frame];
                samples.push(sample.clamp(i16::MIN as i32, i16::MAX as i32) as i16);
            }
        }
        let mut stream = stream.lock();
        stream.set_system_channels(input_count as u32);
        stream.append_buffer(
            SinkBuffer {
                frames: frames as u64,
                frames_played: 0,
                tag: 0,
                consumed: false,
            },
            &samples,
        );
        if stream.is_paused() {
            stream.start(false);
        }
    }

    pub fn verify(self) -> bool {
        !self
            .inputs
            .iter()
            .take(self.input_count as usize)
            .any(|&input| input < 0)
    }

    pub fn dump(self, dump: &mut String) {
        let end = self
            .name
            .iter()
            .position(|&byte| byte == 0)
            .unwrap_or(self.name.len());
        let name = String::from_utf8_lossy(&self.name[..end]);
        let _ = write!(
            dump,
            "DeviceSinkCommand\n\t{} session {} input_count {}\n\tinputs: ",
            name, self.session_id, self.input_count
        );
        for input in self.inputs.iter().take(self.input_count as usize) {
            let _ = write!(dump, "{:02X}, ", input);
        }
        let _ = writeln!(dump);
    }
}

pub fn process_device_command(
    payload: &DeviceSinkPayload,
    stream: &SinkStreamHandle,
    buffer_count: i16,
) {
    let input_count = payload.input_count.max(1).min(buffer_count.max(1) as u32) as usize;
    if payload
        .inputs
        .iter()
        .take(input_count)
        .any(|&input| input < 0)
    {
        return;
    }
    let max_input = payload
        .inputs
        .iter()
        .take(input_count)
        .copied()
        .max()
        .unwrap_or(0) as usize;
    let planar_buffer_count = max_input.saturating_add(1).max(1);
    let sample_count = payload.sample_count as usize;
    let frames = (sample_count / planar_buffer_count).max(1);
    let src =
        unsafe { std::slice::from_raw_parts(payload.sample_buffer as *const i32, sample_count) };
    let mut samples = Vec::with_capacity(frames.saturating_mul(input_count));
    for frame in 0..frames {
        for &input in payload.inputs.iter().take(input_count) {
            let buffer_index = input as usize;
            let sample = src[buffer_index * frames + frame];
            samples.push(sample.clamp(i16::MIN as i32, i16::MAX as i32) as i16);
        }
    }
    let mut stream = stream.lock();
    stream.set_system_channels(input_count as u32);
    stream.append_buffer(
        SinkBuffer {
            frames: frames as u64,
            frames_played: 0,
            tag: 0,
            consumed: false,
        },
        &samples,
    );
    if stream.is_paused() {
        stream.start(false);
    }
}

pub fn verify_device_command(payload: &DeviceSinkPayload) -> bool {
    !payload
        .inputs
        .iter()
        .take(payload.input_count as usize)
        .any(|&input| input < 0)
}

pub fn dump_device_command(payload: &DeviceSinkPayload, dump: &mut String) {
    let end = payload
        .name
        .iter()
        .position(|&byte| byte == 0)
        .unwrap_or(payload.name.len());
    let name = String::from_utf8_lossy(&payload.name[..end]);
    let _ = write!(
        dump,
        "DeviceSinkCommand\n\t{} session {} input_count {}\n\tinputs: ",
        name, payload.session_id, payload.input_count
    );
    for input in payload.inputs.iter().take(payload.input_count as usize) {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = writeln!(dump);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_rejects_negative_input_indices() {
        let payload = DeviceSinkPayload {
            input_count: 2,
            inputs: [-1, 1, 0, 0, 0, 0],
            ..unsafe { std::mem::zeroed() }
        };

        assert!(!payload.verify());
        assert!(!verify_device_command(&payload));
    }
}
