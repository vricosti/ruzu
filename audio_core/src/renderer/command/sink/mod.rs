use crate::adsp::apps::audio_renderer::command_list_processor::CommandListProcessor;
use crate::common::common::CpuAddr;
use crate::renderer::command::icommand::CommandId;

pub mod circular_buffer;
pub mod device;

pub use circular_buffer::{
    dump_circular_buffer_command, process_circular_buffer_command, verify_circular_buffer_command,
    write_circular_buffer_payload, CircularBufferSinkCommand, CircularBufferSinkPayload,
};
pub use device::{
    dump_device_command, process_device_command, verify_device_command, write_device_payload,
    DeviceSinkCommand, DeviceSinkPayload,
};

fn read_pod<T: Copy>(addr: CpuAddr) -> Option<T> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { (addr as *const T).read_unaligned() })
}

pub fn dump_sink_command(command_id: CommandId, payload_addr: CpuAddr, dump: &mut String) -> bool {
    match command_id {
        CommandId::DeviceSink => read_pod::<DeviceSinkPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        CommandId::CircularBufferSink => read_pod::<CircularBufferSinkPayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        _ => false,
    }
}

pub fn verify_sink_command(command_id: CommandId, payload_addr: CpuAddr) -> Option<bool> {
    match command_id {
        CommandId::DeviceSink => Some(
            read_pod::<DeviceSinkPayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        CommandId::CircularBufferSink => Some(
            read_pod::<CircularBufferSinkPayload>(payload_addr)
                .is_some_and(|payload| payload.verify()),
        ),
        _ => None,
    }
}

pub fn process_sink_command(
    processor: &mut CommandListProcessor,
    command_id: CommandId,
    payload_addr: CpuAddr,
) -> bool {
    match command_id {
        CommandId::DeviceSink => {
            let Some(payload) = read_pod::<DeviceSinkPayload>(payload_addr) else {
                return true;
            };
            let Some(stream) = processor.get_output_sink_stream() else {
                return true;
            };
            payload.process(&stream, processor.get_buffer_count_raw());
            true
        }
        CommandId::CircularBufferSink => {
            let Some(payload) = read_pod::<CircularBufferSinkPayload>(payload_addr) else {
                return true;
            };
            let buffer_count = processor.get_buffer_count_raw();
            let _ = processor.with_mix_buffers(|mix_buffers, sample_count, _| {
                payload.process(payload_addr, sample_count, buffer_count, mix_buffers);
            });
            true
        }
        _ => false,
    }
}
