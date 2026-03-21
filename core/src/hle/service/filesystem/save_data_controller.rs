//! Port of zuyu/src/core/hle/service/filesystem/save_data_controller.h and .cpp
//!
//! SaveDataController - manages save data creation and access.

use std::sync::{Arc, Mutex};

use crate::file_sys::fs_save_data_types::{SaveDataAttribute, SaveDataSpaceId};
use crate::file_sys::savedata_factory::SaveDataFactory;
use crate::file_sys::vfs::vfs_types::VirtualDir;

/// Default size for normal/journal save data if application control metadata cannot be found.
/// ~4.2GB, matching upstream SufficientSaveDataSize.
const _SUFFICIENT_SAVE_DATA_SIZE: u64 = 0xF000_0000;

/// Port of Service::FileSystem::SaveDataController
pub struct SaveDataController {
    factory: Option<Arc<Mutex<SaveDataFactory>>>,
}

impl SaveDataController {
    pub fn new() -> Self {
        Self { factory: None }
    }

    pub fn with_factory(factory: Arc<Mutex<SaveDataFactory>>) -> Self {
        Self {
            factory: Some(factory),
        }
    }

    pub fn set_factory(&mut self, factory: Arc<Mutex<SaveDataFactory>>) {
        self.factory = Some(factory);
    }

    pub fn create_save_data(
        &self,
        space: SaveDataSpaceId,
        attribute: &SaveDataAttribute,
    ) -> Option<VirtualDir> {
        let factory = self.factory.as_ref()?;
        factory.lock().unwrap().create(space, attribute)
    }

    pub fn open_save_data(
        &self,
        space: SaveDataSpaceId,
        attribute: &SaveDataAttribute,
    ) -> Option<VirtualDir> {
        let factory = self.factory.as_ref()?;
        factory.lock().unwrap().open(space, attribute)
    }

    pub fn open_save_data_space(&self, space: SaveDataSpaceId) -> Option<VirtualDir> {
        let factory = self.factory.as_ref()?;
        factory.lock().unwrap().get_save_data_space_directory(space)
    }

    pub fn set_auto_create(&mut self, state: bool) {
        if let Some(ref factory) = self.factory {
            factory.lock().unwrap().set_auto_create(state);
        }
    }
}

impl Default for SaveDataController {
    fn default() -> Self {
        Self::new()
    }
}
