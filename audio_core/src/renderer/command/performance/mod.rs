use crate::adsp::apps::audio_renderer::command_list_processor::CommandListProcessor;
use crate::common::common::CpuAddr;
use crate::renderer::command::icommand::CommandId;

mod performance;

pub use performance::{
    dump_performance_command, process_performance_command, verify_performance_command,
    write_performance_payload, PerformanceCommand, PerformancePayload,
};

fn read_pod<T: Copy>(addr: CpuAddr) -> Option<T> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { (addr as *const T).read_unaligned() })
}

pub fn dump_performance_family_command(
    command_id: CommandId,
    payload_addr: CpuAddr,
    dump: &mut String,
) -> bool {
    match command_id {
        CommandId::Performance => read_pod::<PerformancePayload>(payload_addr)
            .map(|payload| payload.dump(dump))
            .is_some(),
        _ => false,
    }
}

pub fn verify_performance_family_command(
    command_id: CommandId,
    payload_addr: CpuAddr,
) -> Option<bool> {
    match command_id {
        CommandId::Performance => Some(
            read_pod::<PerformancePayload>(payload_addr).is_some_and(|payload| payload.verify()),
        ),
        _ => None,
    }
}

pub fn process_performance_family_command(
    processor: &mut CommandListProcessor,
    command_id: CommandId,
    payload_addr: CpuAddr,
) -> bool {
    match command_id {
        CommandId::Performance => {
            let Some(payload) = read_pod::<PerformancePayload>(payload_addr) else {
                return true;
            };
            payload.process(processor.current_process_time_offset());
            true
        }
        _ => false,
    }
}
