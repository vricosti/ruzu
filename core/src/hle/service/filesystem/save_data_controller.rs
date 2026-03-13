//! Port of zuyu/src/core/hle/service/filesystem/save_data_controller.h and .cpp
//!
//! SaveDataController - manages save data creation and access.

/// Default size for normal/journal save data if application control metadata cannot be found.
/// ~4.2GB, matching upstream SufficientSaveDataSize.
const _SUFFICIENT_SAVE_DATA_SIZE: u64 = 0xF000_0000;

/// Port of Service::FileSystem::SaveDataController
pub struct SaveDataController {
    // factory: Arc<SaveDataFactory>,
}

impl SaveDataController {
    pub fn new() -> Self {
        Self {}
    }

    pub fn create_save_data(&self) {
        // Upstream: factory->Create(attribute, creation_info)
        // TODO: Requires SaveDataFactory (FileSys crate) to be ported.
        log::warn!("SaveDataController::create_save_data: SaveDataFactory not yet ported");
    }

    pub fn open_save_data(&self) {
        // Upstream: factory->Open(attribute)
        // TODO: Requires SaveDataFactory (FileSys crate) to be ported.
        log::warn!("SaveDataController::open_save_data: SaveDataFactory not yet ported");
    }

    pub fn open_save_data_space(&self) {
        // Upstream: factory->OpenSaveDataSpace(space)
        // TODO: Requires SaveDataFactory (FileSys crate) to be ported.
        log::warn!("SaveDataController::open_save_data_space: SaveDataFactory not yet ported");
    }

    pub fn set_auto_create(&mut self, _state: bool) {
        // factory->SetAutoCreate(state)
    }
}
