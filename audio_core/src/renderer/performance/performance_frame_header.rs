use crate::common::common::make_magic;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PerformanceFrameHeaderVersion1 {
    pub magic: u32,
    pub entry_count: u32,
    pub detail_count: u32,
    pub next_offset: u32,
    pub total_processing_time: u32,
    pub frame_index: u32,
}

impl PerformanceFrameHeaderVersion1 {
    pub fn new() -> Self {
        Self {
            magic: make_magic('P', 'E', 'R', 'F'),
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PerformanceFrameHeaderVersion2 {
    pub magic: u32,
    pub entry_count: u32,
    pub detail_count: u32,
    pub next_offset: u32,
    pub total_processing_time: u32,
    pub voices_dropped: u32,
    pub start_time: u64,
    pub frame_index: u32,
    pub render_time_exceeded: bool,
    pub unk25: [u8; 0xB],
}

impl Default for PerformanceFrameHeaderVersion2 {
    fn default() -> Self {
        Self {
            magic: make_magic('P', 'E', 'R', 'F'),
            entry_count: 0,
            detail_count: 0,
            next_offset: 0,
            total_processing_time: 0,
            voices_dropped: 0,
            start_time: 0,
            frame_index: 0,
            render_time_exceeded: false,
            unk25: [0; 0xB],
        }
    }
}
