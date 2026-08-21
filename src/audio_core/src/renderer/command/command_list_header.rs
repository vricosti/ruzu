use crate::common::common::CpuAddr;

pub const COMMAND_LIST_HEADER_SIZE: usize = 0x20;

#[derive(Debug, Clone, Copy, Default)]
pub struct CommandListHeader {
    pub buffer_size: u64,
    pub command_count: u32,
    pub samples_buffer: CpuAddr,
    pub buffer_count: i16,
    pub sample_count: u32,
    pub sample_rate: u32,
}
