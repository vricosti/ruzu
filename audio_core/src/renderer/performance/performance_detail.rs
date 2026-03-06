use super::performance_entry::PerformanceEntryType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PerformanceDetailType {
    Invalid = 0,
    Unk1 = 1,
    Unk2 = 2,
    Unk3 = 3,
    Unk4 = 4,
    Unk5 = 5,
    Unk6 = 6,
    Unk7 = 7,
    Unk8 = 8,
    Unk9 = 9,
    Unk10 = 10,
    Unk11 = 11,
    Unk12 = 12,
    Unk13 = 13,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PerformanceDetailVersion1 {
    pub node_id: u32,
    pub start_time: u32,
    pub processed_time: u32,
    pub detail_type: PerformanceDetailType,
    pub entry_type: PerformanceEntryType,
}

impl Default for PerformanceDetailVersion1 {
    fn default() -> Self {
        Self {
            node_id: 0,
            start_time: 0,
            processed_time: 0,
            detail_type: PerformanceDetailType::Invalid,
            entry_type: PerformanceEntryType::Invalid,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PerformanceDetailVersion2 {
    pub node_id: u32,
    pub start_time: u32,
    pub processed_time: u32,
    pub detail_type: PerformanceDetailType,
    pub entry_type: PerformanceEntryType,
    pub unk_10: u32,
    pub unk14: [u8; 0x4],
}

impl Default for PerformanceDetailVersion2 {
    fn default() -> Self {
        Self {
            node_id: 0,
            start_time: 0,
            processed_time: 0,
            detail_type: PerformanceDetailType::Invalid,
            entry_type: PerformanceEntryType::Invalid,
            unk_10: 0,
            unk14: [0; 0x4],
        }
    }
}
