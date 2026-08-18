//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_save_data_info_reader.h and .cpp
//!
//! ISaveDataInfoReader service.

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::file_sys::fs_save_data_types::{SaveDataRank, SaveDataSpaceId, SaveDataType};
use crate::file_sys::vfs::vfs_types::VirtualDir;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::CmifResponse;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::super::save_data_controller::SaveDataController;

/// Port of ISaveDataInfoReader::SaveDataInfo
/// sizeof = 0x60
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SaveDataInfo {
    pub save_id_unknown: u64,
    pub space: SaveDataSpaceId,
    pub save_type: SaveDataType,
    pub _padding0: [u8; 6],
    pub user_id: [u8; 0x10],
    pub save_id: u64,
    pub title_id: u64,
    pub save_image_size: u64,
    pub index: u16,
    pub rank: SaveDataRank,
    pub _padding1: [u8; 0x25],
}

const _: () = assert!(std::mem::size_of::<SaveDataInfo>() == 0x60);
const _: () = assert!(std::mem::offset_of!(SaveDataInfo, space) == 0x08);
const _: () = assert!(std::mem::offset_of!(SaveDataInfo, user_id) == 0x10);
const _: () = assert!(std::mem::offset_of!(SaveDataInfo, save_id) == 0x20);
const _: () = assert!(std::mem::offset_of!(SaveDataInfo, title_id) == 0x28);
const _: () = assert!(std::mem::offset_of!(SaveDataInfo, save_image_size) == 0x30);
const _: () = assert!(std::mem::offset_of!(SaveDataInfo, index) == 0x38);
const _: () = assert!(std::mem::offset_of!(SaveDataInfo, rank) == 0x3A);

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
    save_data_controller: SaveDataController,
    info: Vec<SaveDataInfo>,
    next_entry_index: Mutex<u64>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISaveDataInfoReader {
    pub fn new(save_data_controller: SaveDataController, space: SaveDataSpaceId) -> Self {
        let mut reader = Self {
            save_data_controller,
            info: Vec::new(),
            next_entry_index: Mutex::new(0),
            handlers: build_handler_map(&[(
                0,
                Some(Self::read_save_data_info_handler),
                "ReadSaveDataInfo",
            )]),
            handlers_tipc: BTreeMap::new(),
        };
        reader.find_all_saves(space);
        reader
    }

    /// Port of upstream `ISaveDataInfoReader::ReadSaveDataInfo`.
    fn read_save_data_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const ISaveDataInfoReader) };
        log::debug!("ISaveDataInfoReader::ReadSaveDataInfo called");

        let buffer_entries = ctx.get_write_buffer_size(0) / core::mem::size_of::<SaveDataInfo>();
        let mut next_entry_index = service.next_entry_index.lock().unwrap();
        let remaining = service.info.len() - *next_entry_index as usize;
        let actual_entries = buffer_entries.min(remaining);
        let begin = *next_entry_index as usize;
        let end = begin + actual_entries;
        if actual_entries != 0 {
            let entries = &service.info[begin..end];
            let bytes = unsafe {
                core::slice::from_raw_parts(
                    entries.as_ptr().cast::<u8>(),
                    actual_entries * core::mem::size_of::<SaveDataInfo>(),
                )
            };
            ctx.write_buffer(bytes, 0);
        }
        *next_entry_index += actual_entries as u64;

        let mut response = CmifResponse::new(ctx, 4, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_u64(actual_entries as u64);
    }

    /// Port of upstream `ISaveDataInfoReader::FindAllSaves`.
    fn find_all_saves(&mut self, space: SaveDataSpaceId) {
        let Some(save_root) = self.save_data_controller.open_save_data_space(space) else {
            log::error!(
                "The save root for the space_id={:02X} was invalid!",
                space as u8
            );
            return;
        };

        for save_type in save_root.get_subdirectories() {
            if save_type.get_name() == "save" {
                self.find_normal_saves(space, &save_type);
            } else if space == SaveDataSpaceId::Temporary {
                self.find_temporary_storage_saves(space, &save_type);
            }
        }
    }

    /// Port of upstream `ISaveDataInfoReader::FindNormalSaves`.
    fn find_normal_saves(&mut self, space: SaveDataSpaceId, save_type: &VirtualDir) {
        for save_id in save_type.get_subdirectories() {
            for user_id in save_id.get_subdirectories() {
                let user_id_name = user_id.get_name();
                if user_id_name.len() != 0x20 {
                    continue;
                }

                let save_id_numeric = stoull_be(&save_id.get_name());
                let mut user_id_numeric =
                    common::hex_util::hex_string_to_array::<0x10>(&user_id_name);
                user_id_numeric.reverse();

                if save_id_numeric != 0 {
                    self.info.push(SaveDataInfo {
                        save_id_unknown: 0,
                        space,
                        save_type: SaveDataType::System,
                        _padding0: [0; 6],
                        user_id: user_id_numeric,
                        save_id: save_id_numeric,
                        title_id: 0,
                        save_image_size: user_id.get_size() as u64,
                        index: 0,
                        rank: SaveDataRank::Primary,
                        _padding1: [0; 0x25],
                    });
                    continue;
                }

                for title_id in user_id.get_subdirectories() {
                    let is_device = user_id_numeric.iter().all(|&value| value == 0);
                    self.info.push(SaveDataInfo {
                        save_id_unknown: 0,
                        space,
                        save_type: if is_device {
                            SaveDataType::Device
                        } else {
                            SaveDataType::Account
                        },
                        _padding0: [0; 6],
                        user_id: user_id_numeric,
                        save_id: save_id_numeric,
                        title_id: stoull_be(&title_id.get_name()),
                        save_image_size: title_id.get_size() as u64,
                        index: 0,
                        rank: SaveDataRank::Primary,
                        _padding1: [0; 0x25],
                    });
                }
            }
        }
    }

    /// Port of upstream `ISaveDataInfoReader::FindTemporaryStorageSaves`.
    fn find_temporary_storage_saves(&mut self, space: SaveDataSpaceId, save_type: &VirtualDir) {
        for user_id in save_type.get_subdirectories() {
            let user_id_name = user_id.get_name();
            if user_id_name.len() != 0x20 {
                continue;
            }
            for title_id in user_id.get_subdirectories() {
                if title_id.get_files().is_empty() && title_id.get_subdirectories().is_empty() {
                    continue;
                }

                let mut user_id_numeric =
                    common::hex_util::hex_string_to_array::<0x10>(&user_id_name);
                user_id_numeric.reverse();
                self.info.push(SaveDataInfo {
                    save_id_unknown: 0,
                    space,
                    save_type: SaveDataType::Temporary,
                    _padding0: [0; 6],
                    user_id: user_id_numeric,
                    save_id: stoull_be(&save_type.get_name()),
                    title_id: stoull_be(&title_id.get_name()),
                    save_image_size: title_id.get_size() as u64,
                    index: 0,
                    rank: SaveDataRank::Primary,
                    _padding1: [0; 0x25],
                });
            }
        }
    }
}

/// Port of upstream file-local `stoull_be`.
fn stoull_be(value: &str) -> u64 {
    if value.len() != 16 {
        return 0;
    }
    u64::from_str_radix(value, 16).unwrap_or(0)
}

impl SessionRequestHandler for ISaveDataInfoReader {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "fsp::ISaveDataInfoReader"
    }
}

impl ServiceFramework for ISaveDataInfoReader {
    fn get_service_name(&self) -> &str {
        "fsp::ISaveDataInfoReader"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::{VectorVfsDirectory, VectorVfsFile};
    use std::sync::Arc;

    fn directory(name: &str, dirs: Vec<VirtualDir>) -> VirtualDir {
        Arc::new(VectorVfsDirectory::new(
            Vec::new(),
            dirs,
            name.to_owned(),
            None,
        ))
    }

    #[test]
    fn save_data_info_layout_matches_upstream() {
        assert_eq!(core::mem::size_of::<SaveDataInfo>(), 0x60);
        assert_eq!(core::mem::offset_of!(SaveDataInfo, user_id), 0x10);
        assert_eq!(core::mem::offset_of!(SaveDataInfo, rank), 0x3A);
    }

    #[test]
    fn stoull_be_matches_upstream_filename_conversion() {
        assert_eq!(stoull_be("0100F2C0115B6000"), 0x0100_F2C0_115B_6000);
        assert_eq!(stoull_be("too-short"), 0);
        assert_eq!(stoull_be("not-hexadecimal!"), 0);
    }

    #[test]
    fn find_normal_saves_preserves_upstream_field_mapping() {
        let title = directory("0100F2C0115B6000", Vec::new());
        let user = directory("00112233445566778899AABBCCDDEEFF", vec![title]);
        let save_id = directory("0000000000000000", vec![user]);
        let save_type = directory("save", vec![save_id]);

        let mut reader = ISaveDataInfoReader::new(SaveDataController::new(), SaveDataSpaceId::User);
        reader.find_normal_saves(SaveDataSpaceId::User, &save_type);

        assert_eq!(reader.info.len(), 1);
        let info = reader.info[0];
        assert_eq!(info.space, SaveDataSpaceId::User);
        assert_eq!(info.save_type, SaveDataType::Account);
        assert_eq!(
            info.user_id,
            [
                0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, 0x99, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22,
                0x11, 0x00,
            ]
        );
        assert_eq!(info.save_id, 0);
        assert_eq!(info.title_id, 0x0100_F2C0_115B_6000);
        assert_eq!(info.rank, SaveDataRank::Primary);
    }

    #[test]
    fn find_temporary_saves_skips_empty_titles() {
        let empty_title = directory("0100000000000001", Vec::new());
        let nonempty_title: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![Arc::new(VectorVfsFile::new(
                vec![1],
                "marker".to_owned(),
                None,
            ))],
            Vec::new(),
            "0100000000000002".to_owned(),
            None,
        ));
        let user = directory(
            "00000000000000000000000000000000",
            vec![empty_title, nonempty_title],
        );
        let save_type = directory("0000000000001234", vec![user]);

        let mut reader =
            ISaveDataInfoReader::new(SaveDataController::new(), SaveDataSpaceId::Temporary);
        reader.find_temporary_storage_saves(SaveDataSpaceId::Temporary, &save_type);

        assert_eq!(reader.info.len(), 1);
        assert_eq!(reader.info[0].save_id, 0x1234);
        assert_eq!(reader.info[0].title_id, 0x0100_0000_0000_0002);
        assert_eq!(reader.info[0].save_type, SaveDataType::Temporary);
    }
}
