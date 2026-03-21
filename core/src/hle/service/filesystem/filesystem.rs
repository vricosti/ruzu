//! Port of zuyu/src/core/hle/service/filesystem/filesystem.h and filesystem.cpp
//!
//! FileSystemController and VfsDirectoryServiceWrapper.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use common::fs::path_util::{self, DirectorySeparator};
use common::ResultCode;

use crate::file_sys::errors;
use crate::file_sys::fs_filesystem::{DirectoryEntryType, OpenMode};
use crate::file_sys::romfs_factory::RomFSFactory;
use crate::file_sys::savedata_factory::SaveDataFactory;
use crate::file_sys::vfs::vfs_offset::OffsetVfsFile;
use crate::file_sys::vfs::vfs_types::{FileTimeStampRaw, VirtualDir, VirtualFile};
use crate::hle::result::RESULT_SUCCESS;
use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
use crate::hle::service::sm::sm::ServiceManager;

/// Port of upstream `ResultUnknown` (result.h:250) — `common::ResultCode(UINT32_MAX)`.
/// Used as a fallback error code in VfsDirectoryServiceWrapper methods.
const RESULT_UNKNOWN: ResultCode = ResultCode(u32::MAX);

/// Port of Service::FileSystem::ContentStorageId
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ContentStorageId {
    System = 0,
    User = 1,
    SdCard = 2,
}

/// Port of Service::FileSystem::ImageDirectoryId
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ImageDirectoryId {
    Nand = 0,
    SdCard = 1,
}

/// Port of Service::FileSystem::ProcessId
pub type ProcessId = u64;

/// Port of Service::FileSystem::ProgramId
pub type ProgramId = u64;

/// Port of upstream `FileSystemController::Registration` (filesystem.h:129-133).
struct Registration {
    program_id: ProgramId,
    romfs_factory: Option<Arc<RomFSFactory>>,
    save_data_factory: Option<Arc<Mutex<SaveDataFactory>>>,
}

/// Port of Service::FileSystem::FileSystemController
///
/// Manages filesystem factories and process registrations.
/// Upstream: filesystem.h:64-146, filesystem.cpp.
pub struct FileSystemController {
    registrations: Mutex<BTreeMap<ProcessId, Registration>>,
    /// BIS factory for NAND system/user content.
    /// Upstream: `std::unique_ptr<FileSys::BISFactory> bis_factory`.
    bis_factory: Option<crate::file_sys::bis_factory::BisFactory>,
    /// Virtual filesystem reference for creating factories.
    /// Upstream: `Core::System::GetFilesystem()`.
    vfs: Option<Arc<crate::file_sys::vfs::vfs_real::RealVfsFilesystem>>,
    // sdmc_factory: Option<SDMCFactory>,
    // gamecard: Option<XCI>,
    // gamecard_registered: Option<RegisteredCache>,
    // gamecard_placeholder: Option<PlaceholderCache>,
}

impl FileSystemController {
    pub fn new() -> Self {
        Self {
            registrations: Mutex::new(BTreeMap::new()),
            bis_factory: None,
            vfs: None,
        }
    }

    /// Set the virtual filesystem reference.
    pub fn set_filesystem(&mut self, vfs: Arc<crate::file_sys::vfs::vfs_real::RealVfsFilesystem>) {
        self.vfs = Some(vfs);
    }

    /// Set the BIS factory (called during system initialization).
    pub fn set_bis_factory(&mut self, factory: crate::file_sys::bis_factory::BisFactory) {
        self.bis_factory = Some(factory);
    }

    /// Get the System NAND RegisteredCache.
    /// Upstream: `FileSystemController::GetSystemNANDContents()`.
    pub fn get_system_nand_contents(&self) -> Option<&crate::file_sys::registered_cache::RegisteredCache> {
        self.bis_factory.as_ref()?.get_system_nand_contents()
    }

    /// Get the User NAND RegisteredCache.
    /// Upstream: `FileSystemController::GetUserNANDContents()`.
    pub fn get_user_nand_contents(&self) -> Option<&crate::file_sys::registered_cache::RegisteredCache> {
        self.bis_factory.as_ref()?.get_user_nand_contents()
    }

    /// Get the System NAND content directory.
    /// Upstream: `FileSystemController::GetSystemNANDContentDirectory()`.
    pub fn get_system_nand_content_directory(&self) -> Option<crate::file_sys::vfs::vfs_types::VirtualDir> {
        self.bis_factory.as_ref()?.get_system_nand_content_directory()
    }

    /// Get the User NAND content directory.
    /// Upstream: `FileSystemController::GetUserNANDContentDirectory()`.
    pub fn get_user_nand_content_directory(&self) -> Option<crate::file_sys::vfs::vfs_types::VirtualDir> {
        self.bis_factory.as_ref()?.get_user_nand_content_directory()
    }

    /// Port of upstream `FileSystemController::RegisterProcess` (filesystem.cpp:298-311).
    ///
    /// Called by the NCA/NRO loader after loading a process. Stores the
    /// process_id → program_id mapping and associated RomFS/SaveData factories
    /// so that FSP_SRV::SetCurrentProcess can resolve controllers.
    pub fn register_process(
        &self,
        process_id: ProcessId,
        program_id: ProgramId,
        romfs_factory: Option<Arc<RomFSFactory>>,
    ) -> u32 {
        let save_data_factory = self.create_save_data_factory(program_id);
        let mut registrations = self.registrations.lock().unwrap();
        registrations.insert(process_id, Registration {
            program_id,
            romfs_factory,
            save_data_factory,
        });
        log::debug!(
            "FileSystemController::RegisterProcess: process_id={:#x}, program_id={:#018x}",
            process_id, program_id,
        );
        RESULT_SUCCESS.get_inner_value()
    }

    /// Port of upstream `FileSystemController::OpenProcess` (filesystem.cpp:313-328).
    ///
    /// Looks up the registration for `process_id` and returns the associated
    /// program_id, SaveDataController, and RomFsController.
    pub fn open_process(
        &self,
        process_id: ProcessId,
    ) -> Option<(
        ProgramId,
        super::save_data_controller::SaveDataController,
        super::romfs_controller::RomFsController,
    )> {
        let registrations = self.registrations.lock().unwrap();
        let reg = registrations.get(&process_id)?;
        let save_data_controller = match &reg.save_data_factory {
            Some(factory) => {
                super::save_data_controller::SaveDataController::with_factory(factory.clone())
            }
            None => super::save_data_controller::SaveDataController::new(),
        };
        let romfs_controller = match &reg.romfs_factory {
            Some(factory) => {
                super::romfs_controller::RomFsController::with_factory(reg.program_id, factory.clone())
            }
            None => super::romfs_controller::RomFsController::new(reg.program_id),
        };
        Some((reg.program_id, save_data_controller, romfs_controller))
    }

    /// Port of upstream `FileSystemController::CreateSaveDataFactory` (filesystem.cpp:347-357).
    ///
    /// Creates a SaveDataFactory for the given program_id using the NAND save directory.
    fn create_save_data_factory(&self, program_id: ProgramId) -> Option<Arc<Mutex<SaveDataFactory>>> {
        use crate::file_sys::fs_filesystem::OpenMode;
        use common::fs::path_util::{get_ruzu_path_string, RuzuPath};

        // Upstream: auto vfs = system.GetFilesystem();
        //           auto nand_directory = vfs->OpenDirectory(NANDDir, ReadWrite);
        let vfs = self.vfs.as_ref()?;
        let nand_path = get_ruzu_path_string(RuzuPath::NANDDir);
        let nand_directory: VirtualDir = Arc::new(
            crate::file_sys::vfs::vfs_real::RealVfsDirectory::new(
                vfs.clone(),
                nand_path,
                OpenMode::READ_WRITE,
            ),
        );
        Some(Arc::new(Mutex::new(SaveDataFactory::new(program_id, nand_directory))))
    }

    /// Get modification load root for a given title.
    /// Upstream: `FileSystemController::GetModificationLoadRoot`.
    pub fn get_modification_load_root(&self, title_id: u64) -> Option<crate::file_sys::vfs::vfs_types::VirtualDir> {
        log::trace!("Opening mod load root for tid={:016X}", title_id);
        self.bis_factory.as_ref()?.get_modification_load_root(title_id)
    }

    /// Get SDMC modification load root for a given title.
    /// Upstream: `FileSystemController::GetSDMCModificationLoadRoot`.
    pub fn get_sdmc_modification_load_root(&self, _title_id: u64) -> Option<crate::file_sys::vfs::vfs_types::VirtualDir> {
        // Upstream delegates to sdmc_factory which is not yet ported.
        None
    }

    /// Get modification dump root for a given title.
    /// Upstream: `FileSystemController::GetModificationDumpRoot`.
    pub fn get_modification_dump_root(&self, title_id: u64) -> Option<crate::file_sys::vfs::vfs_types::VirtualDir> {
        log::trace!("Opening mod dump root for tid={:016X}", title_id);
        self.bis_factory.as_ref()?.get_modification_dump_root(title_id)
    }

    /// Get BCAT directory for a given title.
    /// Upstream: `FileSystemController::GetBCATDirectory`.
    pub fn get_bcat_directory(&self, title_id: u64) -> Option<crate::file_sys::vfs::vfs_types::VirtualDir> {
        log::trace!("Opening BCAT root for tid={:016X}", title_id);
        self.bis_factory.as_ref()?.get_bcat_directory(title_id)
    }

    pub fn create_factories(&mut self) {
        // Upstream creates SDMCFactory and BISFactory using the VfsFilesystem.
        // BISFactory is created via set_bis_factory(); SDMCFactory is not yet ported.
        if self.bis_factory.is_none() {
            log::warn!("FileSystemController::create_factories: BISFactory not yet set");
        }
        // SDMCFactory creation would go here once ported.
    }

    pub fn reset(&mut self) {
        self.registrations.lock().unwrap().clear();
    }
}

/// Port of upstream static `GetDirectoryRelativeWrapped` (filesystem.cpp:33-39).
///
/// Resolves a directory path relative to `base`, treating empty / "." / "/" / "\\"
/// as the base directory itself.
fn get_directory_relative_wrapped(base: &VirtualDir, dir_name: &str) -> Option<VirtualDir> {
    let dir_name = path_util::sanitize_path(dir_name, DirectorySeparator::ForwardSlash);
    if dir_name.is_empty() || dir_name == "." || dir_name == "/" || dir_name == "\\" {
        return Some(Arc::clone(base));
    }
    base.get_directory_relative(&dir_name)
}

/// Port of Service::FileSystem::VfsDirectoryServiceWrapper
///
/// Wraps a VfsDirectory with Result-returning methods for use with Switch services.
pub struct VfsDirectoryServiceWrapper {
    backing: VirtualDir,
}

impl VfsDirectoryServiceWrapper {
    pub fn new(backing: VirtualDir) -> Self {
        Self { backing }
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::GetName` (filesystem.cpp:47-49).
    pub fn get_name(&self) -> String {
        self.backing.get_name()
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::CreateFile` (filesystem.cpp:51-73).
    pub fn create_file(&self, path: &str, size: u64) -> Result<(), ResultCode> {
        let path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        let dir = get_directory_relative_wrapped(&self.backing, &path_util::get_parent_path(&path))
            .ok_or(errors::RESULT_PATH_NOT_FOUND)?;

        if self.get_entry_type(&path).is_ok() {
            return Err(errors::RESULT_PATH_ALREADY_EXISTS);
        }

        let filename = path_util::get_filename(&path);
        let file = dir
            .create_file(filename)
            // Upstream TODO(DarkLordZach): Find a better error code for this
            .ok_or(RESULT_UNKNOWN)?;
        if !file.resize(size as usize) {
            // Upstream TODO(DarkLordZach): Find a better error code for this
            return Err(RESULT_UNKNOWN);
        }
        Ok(())
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::DeleteFile` (filesystem.cpp:75-92).
    pub fn delete_file(&self, path: &str) -> Result<(), ResultCode> {
        let path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        if path.is_empty() {
            // Upstream TODO(DarkLordZach): Why do games call this and what should it do?
            // Works as is but...
            return Ok(());
        }

        let dir = get_directory_relative_wrapped(&self.backing, &path_util::get_parent_path(&path))
            .ok_or(errors::RESULT_PATH_NOT_FOUND)?;
        let filename = path_util::get_filename(&path);
        if dir.get_file(filename).is_none() {
            return Err(errors::RESULT_PATH_NOT_FOUND);
        }
        if !dir.delete_file(filename) {
            // Upstream TODO(DarkLordZach): Find a better error code for this
            return Err(RESULT_UNKNOWN);
        }
        Ok(())
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::CreateDirectory` (filesystem.cpp:94-112).
    ///
    /// NOTE: This is inaccurate behavior. CreateDirectory is not recursive.
    /// CreateDirectory should return PathNotFound if the parent directory does not exist.
    /// This is here temporarily in order to have UMM "work" in the meantime.
    /// Upstream TODO (Morph): Remove this when a hardware test verifies the correct behavior.
    pub fn create_directory(&self, path: &str) -> Result<(), ResultCode> {
        let path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        let components = path_util::split_path_components_copy(&path);
        let mut relative_path = String::new();
        for component in &components {
            relative_path = path_util::sanitize_path(
                &format!("{}/{}", relative_path, component),
                DirectorySeparator::ForwardSlash,
            );
            if self.backing.create_subdirectory(&relative_path).is_none() {
                // Upstream TODO(DarkLordZach): Find a better error code for this
                return Err(RESULT_UNKNOWN);
            }
        }
        Ok(())
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::DeleteDirectory` (filesystem.cpp:114-122).
    pub fn delete_directory(&self, path: &str) -> Result<(), ResultCode> {
        let path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        let dir = get_directory_relative_wrapped(&self.backing, &path_util::get_parent_path(&path))
            .ok_or(RESULT_UNKNOWN)?;
        let filename = path_util::get_filename(&path);
        if !dir.delete_subdirectory(filename) {
            // Upstream TODO(DarkLordZach): Find a better error code for this
            return Err(RESULT_UNKNOWN);
        }
        Ok(())
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::DeleteDirectoryRecursively`
    /// (filesystem.cpp:124-132).
    pub fn delete_directory_recursively(&self, path: &str) -> Result<(), ResultCode> {
        let path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        let dir = get_directory_relative_wrapped(&self.backing, &path_util::get_parent_path(&path))
            .ok_or(RESULT_UNKNOWN)?;
        let filename = path_util::get_filename(&path);
        if !dir.delete_subdirectory_recursive(filename) {
            // Upstream TODO(DarkLordZach): Find a better error code for this
            return Err(RESULT_UNKNOWN);
        }
        Ok(())
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::CleanDirectoryRecursively`
    /// (filesystem.cpp:134-144).
    pub fn clean_directory_recursively(&self, path: &str) -> Result<(), ResultCode> {
        let sanitized_path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        let dir = get_directory_relative_wrapped(
            &self.backing,
            &path_util::get_parent_path(&sanitized_path),
        )
        .ok_or(RESULT_UNKNOWN)?;
        let filename = path_util::get_filename(&sanitized_path);
        if !dir.clean_subdirectory_recursive(filename) {
            // Upstream TODO(DarkLordZach): Find a better error code for this
            return Err(RESULT_UNKNOWN);
        }
        Ok(())
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::RenameFile` (filesystem.cpp:146-187).
    pub fn rename_file(&self, src_path: &str, dest_path: &str) -> Result<(), ResultCode> {
        let src_path = path_util::sanitize_path(src_path, DirectorySeparator::ForwardSlash);
        let dest_path = path_util::sanitize_path(dest_path, DirectorySeparator::ForwardSlash);
        let src = self.backing.get_file_relative(&src_path);
        let dst = self.backing.get_file_relative(&dest_path);

        if path_util::get_parent_path(&src_path) == path_util::get_parent_path(&dest_path) {
            // Use more-optimized vfs implementation rename.
            let src = src.ok_or(errors::RESULT_PATH_NOT_FOUND)?;

            if let Some(ref dst) = dst {
                let full_path = dst.get_full_path();
                if std::path::Path::new(&full_path).exists() {
                    log::error!(
                        "File at new_path={} already exists",
                        full_path
                    );
                    return Err(errors::RESULT_PATH_ALREADY_EXISTS);
                }
            }

            let dest_filename = path_util::get_filename(&dest_path);
            if !src.rename(dest_filename) {
                // Upstream TODO(DarkLordZach): Find a better error code for this
                return Err(RESULT_UNKNOWN);
            }
            return Ok(());
        }

        // Move by hand -- Upstream TODO(DarkLordZach): Optimize
        let src = src.ok_or(errors::RESULT_PATH_NOT_FOUND)?;
        self.create_file(&dest_path, src.get_size() as u64)?;

        let dest = self
            .backing
            .get_file_relative(&dest_path)
            .expect("Newly created file with success cannot be found.");

        let bytes = src.read_all_bytes();
        assert_eq!(
            dest.write_bytes(&bytes, 0),
            src.get_size(),
            "Could not write all of the bytes but everything else has succeeded."
        );

        let src_filename = path_util::get_filename(&src_path);
        let src_dir = src
            .get_containing_directory()
            .ok_or(RESULT_UNKNOWN)?;
        if !src_dir.delete_file(src_filename) {
            // Upstream TODO(DarkLordZach): Find a better error code for this
            return Err(RESULT_UNKNOWN);
        }

        Ok(())
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::RenameDirectory` (filesystem.cpp:189-213).
    pub fn rename_directory(&self, src_path: &str, dest_path: &str) -> Result<(), ResultCode> {
        let src_path = path_util::sanitize_path(src_path, DirectorySeparator::ForwardSlash);
        let dest_path = path_util::sanitize_path(dest_path, DirectorySeparator::ForwardSlash);
        let src = get_directory_relative_wrapped(&self.backing, &src_path);

        if path_util::get_parent_path(&src_path) == path_util::get_parent_path(&dest_path) {
            // Use more-optimized vfs implementation rename.
            let src = src.ok_or(errors::RESULT_PATH_NOT_FOUND)?;
            let dest_filename = path_util::get_filename(&dest_path);
            if !src.rename(dest_filename) {
                // Upstream TODO(DarkLordZach): Find a better error code for this
                return Err(RESULT_UNKNOWN);
            }
            return Ok(());
        }

        // Upstream TODO(DarkLordZach): Implement renaming across the tree (move).
        panic!(
            "Could not rename directory with path \"{}\" to new path \"{}\" because parent dirs \
             don't match -- UNIMPLEMENTED",
            src_path, dest_path
        );
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::OpenFile` (filesystem.cpp:215-236).
    pub fn open_file(&self, path: &str, mode: OpenMode) -> Result<VirtualFile, ResultCode> {
        let path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        let npath = path.trim_start_matches(|c| c == '/' || c == '\\');

        let file = self
            .backing
            .get_file_relative(npath)
            .ok_or(errors::RESULT_PATH_NOT_FOUND)?;

        if mode == OpenMode::ALLOW_APPEND {
            let size = file.get_size();
            Ok(Arc::new(OffsetVfsFile::new(
                file,
                size,
                0,
                String::new(),
            )))
        } else {
            Ok(file)
        }
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::OpenDirectory` (filesystem.cpp:238-248).
    pub fn open_directory(&self, path: &str) -> Result<VirtualDir, ResultCode> {
        let path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        let dir = get_directory_relative_wrapped(&self.backing, &path)
            // Upstream TODO(DarkLordZach): Find a better error code for this
            .ok_or(errors::RESULT_PATH_NOT_FOUND)?;
        Ok(dir)
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::GetEntryType` (filesystem.cpp:250-276).
    pub fn get_entry_type(&self, path: &str) -> Result<DirectoryEntryType, ResultCode> {
        let path = path_util::sanitize_path(path, DirectorySeparator::ForwardSlash);
        let dir = get_directory_relative_wrapped(&self.backing, &path_util::get_parent_path(&path))
            .ok_or(errors::RESULT_PATH_NOT_FOUND)?;

        let filename = path_util::get_filename(&path);
        // Upstream TODO(Subv): Some games use the '/' path, find out what this means.
        if filename.is_empty() {
            return Ok(DirectoryEntryType::Directory);
        }

        if dir.get_file(filename).is_some() {
            return Ok(DirectoryEntryType::File);
        }

        if dir.get_subdirectory(filename).is_some() {
            return Ok(DirectoryEntryType::Directory);
        }

        Err(errors::RESULT_PATH_NOT_FOUND)
    }

    /// Port of upstream `VfsDirectoryServiceWrapper::GetFileTimeStampRaw`
    /// (filesystem.cpp:278-292).
    pub fn get_file_time_stamp_raw(&self, path: &str) -> Result<FileTimeStampRaw, ResultCode> {
        let dir = get_directory_relative_wrapped(&self.backing, &path_util::get_parent_path(path))
            .ok_or(errors::RESULT_PATH_NOT_FOUND)?;

        // Check that the entry exists
        self.get_entry_type(path)?;

        let filename = path_util::get_filename(path);
        Ok(dir.get_file_time_stamp(filename))
    }
}

/// Launches FileSystem services.
///
/// Matches upstream `void FileSystem::LoopProcess(Core::System& system)`:
/// Registers "fsp-ldr", "fsp:pr", "fsp-srv".
pub fn loop_process(
    service_manager: &Arc<Mutex<ServiceManager>>,
    fsc: Arc<Mutex<FileSystemController>>,
) {
    register_services(service_manager, fsc);
}

pub fn register_services(
    service_manager: &Arc<Mutex<ServiceManager>>,
    fsc: Arc<Mutex<FileSystemController>>,
) {
    let mut server_manager =
        crate::hle::service::server_manager::ServerManager::new(crate::core::SystemRef::null());

    server_manager.register_named_service(
        "fsp-ldr",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::fsp::fsp_ldr::FspLdr::new())
        }),
        64,
    );
    server_manager.register_named_service(
        "fsp:pr",
        Box::new(|| -> SessionRequestHandlerPtr {
            Arc::new(super::fsp::fsp_pr::FspPr::new())
        }),
        64,
    );
    let fsc_for_closure = fsc.clone();
    server_manager.register_named_service(
        "fsp-srv",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(super::fsp::fsp_srv::FspSrv::new_with_fsc(fsc_for_closure.clone()))
        }),
        64,
    );

    crate::hle::service::server_manager::ServerManager::run_server(server_manager);
}
