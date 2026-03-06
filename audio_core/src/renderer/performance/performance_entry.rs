#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PerformanceEntryType {
    Invalid = 0,
    Voice = 1,
    SubMix = 2,
    FinalMix = 3,
    Sink = 4,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PerformanceEntryVersion1 {
    pub node_id: u32,
    pub start_time: u32,
    pub processed_time: u32,
    pub entry_type: PerformanceEntryType,
}

impl Default for PerformanceEntryVersion1 {
    fn default() -> Self {
        Self {
            node_id: 0,
            start_time: 0,
            processed_time: 0,
            entry_type: PerformanceEntryType::Invalid,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PerformanceEntryVersion2 {
    pub node_id: u32,
    pub start_time: u32,
    pub processed_time: u32,
    pub entry_type: PerformanceEntryType,
    pub unk0d: [u8; 0xB],
}

impl Default for PerformanceEntryVersion2 {
    fn default() -> Self {
        Self {
            node_id: 0,
            start_time: 0,
            processed_time: 0,
            entry_type: PerformanceEntryType::Invalid,
            unk0d: [0; 0xB],
        }
    }
}
