use crate::common::common::CpuAddr;
use crate::renderer::command::util::write_copy;
use crate::renderer::performance::{PerformanceEntryAddresses, PerformanceState};
use std::fmt::Write;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PerformancePayload {
    pub state: u32,
    pub _padding: u32,
    pub translated_address: u64,
    pub entry_start_time_offset: u64,
    pub header_entry_count_offset: u64,
    pub entry_processed_time_offset: u64,
}

#[derive(Debug, Clone, Copy)]
pub struct PerformanceCommand {
    pub state: PerformanceState,
    pub entry_addresses: PerformanceEntryAddresses,
}

pub fn write_performance_payload(cmd: &PerformanceCommand, output: &mut [u8]) -> usize {
    let mut payload: PerformancePayload = unsafe { std::mem::zeroed() };
    payload.state = cmd.state as u32;
    payload._padding = 0;
    payload.translated_address = cmd.entry_addresses.translated_address as u64;
    payload.entry_start_time_offset = cmd.entry_addresses.entry_start_time_offset as u64;
    payload.header_entry_count_offset = cmd.entry_addresses.header_entry_count_offset as u64;
    payload.entry_processed_time_offset = cmd.entry_addresses.entry_processed_time_offset as u64;
    write_copy(&payload, output)
}

impl PerformancePayload {
    pub fn process(self, now: u64) {
        if self.translated_address == 0 {
            return;
        }

        let base = self.translated_address as CpuAddr;
        let elapsed = now as u32;
        match self.state {
            1 => write_u32(base, self.entry_start_time_offset, elapsed),
            2 => {
                write_u32(base, self.entry_processed_time_offset, elapsed);
                let current = read_u32(base, self.header_entry_count_offset);
                write_u32(
                    base,
                    self.header_entry_count_offset,
                    current.saturating_add(1),
                );
            }
            _ => {}
        }
    }

    pub fn verify(self) -> bool {
        true
    }

    pub fn dump(self, dump: &mut String) {
        let _ = writeln!(dump, "PerformanceCommand\n\tstate {}", self.state);
    }
}

pub fn process_performance_command(payload: &PerformancePayload, now: u64) {
    if payload.translated_address == 0 {
        return;
    }

    let base = payload.translated_address as CpuAddr;
    let elapsed = now as u32;
    match payload.state {
        1 => write_u32(base, payload.entry_start_time_offset, elapsed),
        2 => {
            write_u32(base, payload.entry_processed_time_offset, elapsed);
            let current = read_u32(base, payload.header_entry_count_offset);
            write_u32(
                base,
                payload.header_entry_count_offset,
                current.saturating_add(1),
            );
        }
        _ => {}
    }
}

pub fn verify_performance_command(_payload: &PerformancePayload) -> bool {
    true
}

pub fn dump_performance_command(payload: &PerformancePayload, dump: &mut String) {
    let _ = writeln!(dump, "PerformanceCommand\n\tstate {}", payload.state);
}

fn read_u32(base: CpuAddr, offset: u64) -> u32 {
    let addr = base.saturating_add(offset as CpuAddr) as *const u32;
    unsafe { addr.read_unaligned() }
}

fn write_u32(base: CpuAddr, offset: u64, value: u32) {
    let addr = base.saturating_add(offset as CpuAddr) as *mut u32;
    unsafe { addr.write_unaligned(value) };
}
