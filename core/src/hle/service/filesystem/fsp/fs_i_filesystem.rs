//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_filesystem.h and .cpp
//!
//! IFileSystem service.

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::file_sys::fs_filesystem::{
    CreateOption, FileSystemAttribute, OpenDirectoryMode, OpenMode,
};
use crate::file_sys::fsa::fs_i_filesystem::IFileSystem as FsaIFileSystem;
use crate::file_sys::fssrv::fssrv_sf_path::Path as SfPath;
use crate::file_sys::vfs::vfs_types::{FileTimeStampRaw, VirtualDir};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::fs_i_directory::IDirectory;
use super::fs_i_file::IFile;
use super::fsp_types::SizeGetter;

/// IPC command table for IFileSystem:
///
/// | Cmd | Name                        |
/// |-----|-----------------------------|
/// | 0   | CreateFile                  |
/// | 1   | DeleteFile                  |
/// | 2   | CreateDirectory             |
/// | 3   | DeleteDirectory             |
/// | 4   | DeleteDirectoryRecursively  |
/// | 5   | RenameFile                  |
/// | 6   | RenameDirectory             |
/// | 7   | GetEntryType                |
/// | 8   | OpenFile                    |
/// | 9   | OpenDirectory               |
/// | 10  | Commit                      |
/// | 11  | GetFreeSpaceSize            |
/// | 12  | GetTotalSpaceSize           |
/// | 13  | CleanDirectoryRecursively   |
/// | 14  | GetFileTimeStampRaw         |
/// | 15  | QueryEntry                  |
/// | 16  | GetFileSystemAttribute      |
pub struct IFileSystem {
    backend: FsaIFileSystem,
    size_getter: SizeGetter,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IFileSystem {
    pub fn new(dir: VirtualDir, size_getter: SizeGetter) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::create_file_handler), "CreateFile"),
            (1, Some(Self::delete_file_handler), "DeleteFile"),
            (2, Some(Self::create_directory_handler), "CreateDirectory"),
            (3, Some(Self::delete_directory_handler), "DeleteDirectory"),
            (
                4,
                Some(Self::delete_directory_recursively_handler),
                "DeleteDirectoryRecursively",
            ),
            (5, Some(Self::rename_file_handler), "RenameFile"),
            (7, Some(Self::get_entry_type_handler), "GetEntryType"),
            (8, Some(Self::open_file_handler), "OpenFile"),
            (9, Some(Self::open_directory_handler), "OpenDirectory"),
            (10, Some(Self::commit_handler), "Commit"),
            (
                11,
                Some(Self::get_free_space_size_handler),
                "GetFreeSpaceSize",
            ),
            (
                12,
                Some(Self::get_total_space_size_handler),
                "GetTotalSpaceSize",
            ),
            (
                13,
                Some(Self::clean_directory_recursively_handler),
                "CleanDirectoryRecursively",
            ),
            (
                14,
                Some(Self::get_file_time_stamp_raw_handler),
                "GetFileTimeStampRaw",
            ),
            (
                16,
                Some(Self::get_file_system_attribute_handler),
                "GetFileSystemAttribute",
            ),
        ]);

        Self {
            backend: FsaIFileSystem::new(dir),
            size_getter,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn read_path_from_buffer(ctx: &HLERequestContext) -> Result<String, ResultCode> {
        let path_bytes = ctx.read_buffer(0);
        if path_bytes.len() < core::mem::size_of::<SfPath>() {
            return Err(ResultCode::new(u32::MAX));
        }

        let mut path = SfPath::default();
        unsafe {
            core::ptr::copy_nonoverlapping(
                path_bytes.as_ptr(),
                &mut path as *mut SfPath as *mut u8,
                core::mem::size_of::<SfPath>(),
            );
        }
        Ok(path.as_str().to_string())
    }

    fn read_path_from_second_buffer(ctx: &HLERequestContext) -> Result<String, ResultCode> {
        let path_bytes = ctx.read_buffer(1);
        if path_bytes.len() < core::mem::size_of::<SfPath>() {
            return Err(ResultCode::new(u32::MAX));
        }

        let mut path = SfPath::default();
        unsafe {
            core::ptr::copy_nonoverlapping(
                path_bytes.as_ptr(),
                &mut path as *mut SfPath as *mut u8,
                core::mem::size_of::<SfPath>(),
            );
        }
        Ok(path.as_str().to_string())
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(object.clone()).unwrap_or(0)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(object);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    fn push_result_only(ctx: &mut HLERequestContext, result: ResultCode) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn get_entry_type_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!("IFileSystem::GetEntryType called. file={}", path);

        match service.backend.get_entry_type(&path) {
            Ok(entry_type) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u32(entry_type as u32);
            }
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn create_file_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let mut rp = RequestParser::new(ctx);
        let option_raw = rp.pop_i32();
        let size = rp.pop_i64();
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!(
            "IFileSystem::CreateFile called. file={}, option=0x{:X}, size=0x{:08X}",
            path,
            option_raw,
            size
        );

        let option = match option_raw {
            1 => CreateOption::BigFile,
            _ => CreateOption::None,
        };

        match service.backend.create_file(&path, size, option) {
            Ok(()) => Self::push_result_only(ctx, RESULT_SUCCESS),
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn delete_file_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!("IFileSystem::DeleteFile called. file={}", path);

        match service.backend.delete_file(&path) {
            Ok(()) => Self::push_result_only(ctx, RESULT_SUCCESS),
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn create_directory_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!("IFileSystem::CreateDirectory called. directory={}", path);

        match service.backend.create_directory(&path) {
            Ok(()) => Self::push_result_only(ctx, RESULT_SUCCESS),
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn delete_directory_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!("IFileSystem::DeleteDirectory called. directory={}", path);

        match service.backend.delete_directory(&path) {
            Ok(()) => Self::push_result_only(ctx, RESULT_SUCCESS),
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn delete_directory_recursively_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!(
            "IFileSystem::DeleteDirectoryRecursively called. directory={}",
            path
        );

        match service.backend.delete_directory_recursively(&path) {
            Ok(()) => Self::push_result_only(ctx, RESULT_SUCCESS),
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn rename_file_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let old_path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };
        let new_path = match Self::read_path_from_second_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!(
            "IFileSystem::RenameFile called. file '{}' to file '{}'",
            old_path,
            new_path
        );

        match service.backend.rename_file(&old_path, &new_path) {
            Ok(()) => Self::push_result_only(ctx, RESULT_SUCCESS),
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn open_file_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let mut rp = RequestParser::new(ctx);
        let mode_raw = rp.pop_u32();
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!(
            "IFileSystem::OpenFile called. file={}, mode={}",
            path,
            mode_raw
        );

        let mode = OpenMode::from_bits_truncate(mode_raw);
        match service.backend.open_file(&path, mode) {
            Ok(vfs_file) => Self::push_interface_response(ctx, Arc::new(IFile::new(vfs_file))),
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn open_directory_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let mut rp = RequestParser::new(ctx);
        let mode_raw = rp.pop_u32() as u64;
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!(
            "IFileSystem::OpenDirectory called. directory={}, mode={}",
            path,
            mode_raw
        );

        let mode = OpenDirectoryMode::from_bits_truncate(mode_raw);
        match service.backend.open_directory(&path, mode) {
            Ok(vfs_dir) => {
                Self::push_interface_response(ctx, Arc::new(IDirectory::new(vfs_dir, mode)))
            }
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn commit_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_free_space_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64((service.size_getter.get_free_size)());
    }

    fn get_total_space_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64((service.size_getter.get_total_size)());
    }

    fn clean_directory_recursively_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::debug!(
            "IFileSystem::CleanDirectoryRecursively called. directory={}",
            path
        );

        match service.backend.clean_directory_recursively(&path) {
            Ok(()) => Self::push_result_only(ctx, RESULT_SUCCESS),
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn get_file_time_stamp_raw_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFileSystem) };
        let path = match Self::read_path_from_buffer(ctx) {
            Ok(path) => path,
            Err(rc) => {
                Self::push_result_only(ctx, rc);
                return;
            }
        };

        log::warn!("IFileSystem::GetFileTimeStampRaw called. file={}", path);

        match service.backend.get_file_time_stamp_raw(&path) {
            Ok(timestamp) => {
                let mut rb = ResponseBuilder::new(
                    ctx,
                    2 + (core::mem::size_of::<FileTimeStampRaw>() as u32 / 4),
                    0,
                    0,
                );
                rb.push_result(RESULT_SUCCESS);
                rb.push_raw(&timestamp);
            }
            Err(rc) => Self::push_result_only(ctx, ResultCode::new(rc.0)),
        }
    }

    fn get_file_system_attribute_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::warn!("IFileSystem::GetFileSystemAttribute called");

        let attribute = FileSystemAttribute {
            dir_entry_name_length_max_defined: 1,
            file_entry_name_length_max_defined: 1,
            dir_path_name_length_max_defined: 0,
            file_path_name_length_max_defined: 0,
            _padding0: [0; 0x5],
            utf16_dir_entry_name_length_max_defined: 0,
            utf16_file_entry_name_length_max_defined: 0,
            utf16_dir_path_name_length_max_defined: 0,
            utf16_file_path_name_length_max_defined: 0,
            _padding1: [0; 0x18],
            dir_entry_name_length_max: 0x40,
            file_entry_name_length_max: 0x40,
            dir_path_name_length_max: 0,
            file_path_name_length_max: 0,
            _padding2: [0; 0x5],
            utf16_dir_entry_name_length_max: 0,
            utf16_file_entry_name_length_max: 0,
            utf16_dir_path_name_length_max: 0,
            utf16_file_path_name_length_max: 0,
            _padding3: [0; 0x18],
            _padding4: [0; 1],
        };

        let mut rb = ResponseBuilder::new(
            ctx,
            2 + (core::mem::size_of::<FileSystemAttribute>() as u32 / 4),
            0,
            0,
        );
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&attribute);
    }
}

impl SessionRequestHandler for IFileSystem {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "IFileSystem"
    }
}

impl ServiceFramework for IFileSystem {
    fn get_service_name(&self) -> &str {
        "IFileSystem"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }

    fn invoke_request(&self, ctx: &mut HLERequestContext)
    where
        Self: Sized,
    {
        let cmd = ctx.get_command();
        if let Some(fi) = self.handlers().get(&cmd) {
            if let Some(callback) = fi.handler_callback {
                log::trace!("Service::{}: {}", self.get_service_name(), fi.name);
                callback(self, ctx);
                return;
            }
        }

        log::warn!(
            "IFileSystem: unimplemented command '{}' returned stub success",
            cmd
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}
