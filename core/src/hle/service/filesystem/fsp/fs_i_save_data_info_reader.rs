//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_save_data_info_reader.h and .cpp
//!
//! ISaveDataInfoReader service.

/// Port of ISaveDataInfoReader::SaveDataInfo
/// sizeof = 0x60
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SaveDataInfo {
    pub save_id_unknown: u64,
    pub space: u8,
    pub save_type: u8,
    pub _padding0: [u8; 6],
    pub user_id: [u8; 0x10],
    pub save_id: u64,
    pub title_id: u64,
    pub save_image_size: u64,
    pub index: u16,
    pub rank: u8,
    pub _padding1: [u8; 0x25],
}

const _: () = assert!(std::mem::size_of::<SaveDataInfo>() == 0x60);

impl Default for SaveDataInfo {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// IPC command table for ISaveDataInfoReader:
///
/// | Cmd | Name              |
/// |-----|-------------------|
/// | 0   | ReadSaveDataInfo  |
pub struct ISaveDataInfoReader {
    _info: Vec<SaveDataInfo>,
    _next_entry_index: u64,
}

impl ISaveDataInfoReader {
    pub fn new() -> Self {
        Self {
            _info: Vec::new(),
            _next_entry_index: 0,
        }
    }
}
