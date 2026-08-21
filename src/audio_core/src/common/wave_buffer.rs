use crate::common::common::CpuAddr;

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct WaveBufferVersion1 {
    pub buffer: CpuAddr,
    pub buffer_size: u64,
    pub start_offset: u32,
    pub end_offset: u32,
    pub looping: bool,
    pub stream_ended: bool,
    pub _padding0: [u8; 6],
    pub context: CpuAddr,
    pub context_size: u64,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct WaveBufferVersion2 {
    pub buffer: CpuAddr,
    pub context: CpuAddr,
    pub buffer_size: u64,
    pub context_size: u64,
    pub start_offset: u32,
    pub end_offset: u32,
    pub loop_start_offset: u32,
    pub loop_end_offset: u32,
    pub loop_count: i32,
    pub looping: bool,
    pub stream_ended: bool,
    pub _padding0: [u8; 2],
}
