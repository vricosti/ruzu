//! Port of zuyu/src/core/hle/service/filesystem/romfs_controller.h and .cpp
//!
//! RomFsController - manages RomFS access for a process.

/// Port of Service::FileSystem::RomFsController
pub struct RomFsController {
    _program_id: u64,
    // factory: Arc<RomFSFactory>,
}

impl RomFsController {
    pub fn new(program_id: u64) -> Self {
        Self {
            _program_id: program_id,
        }
    }

    pub fn open_romfs_current_process(&self) {
        // return factory->OpenCurrentProcess(program_id)
        // TODO: Requires RomFSFactory (FileSys crate) to be ported.
        log::warn!("RomFsController::open_romfs_current_process: RomFSFactory not yet ported");
    }

    pub fn open_patched_romfs(&self, _title_id: u64) {
        // return factory->OpenPatchedRomFS(title_id, type)
        // TODO: Requires RomFSFactory (FileSys crate) to be ported.
        log::warn!("RomFsController::open_patched_romfs: RomFSFactory not yet ported");
    }

    pub fn open_patched_romfs_with_program_index(&self, _title_id: u64, _program_index: u8) {
        // return factory->OpenPatchedRomFSWithProgramIndex(title_id, program_index, type)
        // TODO: Requires RomFSFactory (FileSys crate) to be ported.
        log::warn!("RomFsController::open_patched_romfs_with_program_index: RomFSFactory not yet ported");
    }

    pub fn open_romfs(&self, _title_id: u64) {
        // return factory->Open(title_id, storage_id, type)
        // TODO: Requires RomFSFactory (FileSys crate) to be ported.
        log::warn!("RomFsController::open_romfs: RomFSFactory not yet ported");
    }

    pub fn open_base_nca(&self, _title_id: u64) {
        // return factory->GetEntry(title_id, storage_id, type)
        // TODO: Requires RomFSFactory (FileSys crate) to be ported.
        log::warn!("RomFsController::open_base_nca: RomFSFactory not yet ported");
    }
}
