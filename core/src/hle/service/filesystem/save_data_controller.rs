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
        todo!("SaveDataController::create_save_data");
    }

    pub fn open_save_data(&self) {
        todo!("SaveDataController::open_save_data");
    }

    pub fn open_save_data_space(&self) {
        todo!("SaveDataController::open_save_data_space");
    }

    pub fn set_auto_create(&mut self, _state: bool) {
        // factory->SetAutoCreate(state)
    }
}
