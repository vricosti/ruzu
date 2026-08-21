// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of Eden `src/core/hle/service/am/process_creation.h` and `process_creation.cpp`.

use std::sync::{Arc, Mutex};

use crate::file_sys::content_archive::NCA;
use crate::file_sys::nca_metadata::ContentRecordType;
use crate::file_sys::partition_filesystem::ResultStatus as FileSysResultStatus;
use crate::file_sys::patch_manager::PatchManager;
use crate::file_sys::registered_cache::{
    get_update_title_id, ContentProvider, ContentProviderUnion, ContentProviderUnionSlot,
};
use crate::file_sys::romfs_factory::StorageId;
use crate::hle::service::filesystem::filesystem::FileSystemController;
use crate::hle::service::glue::glue_manager::ApplicationLaunchProperty;
use crate::hle::service::os::process::Process;
use crate::loader::loader::{get_loader, ResultStatus, System as LoaderSystem};

/// Port of upstream local `GetStorageIdForFrontendSlot`.
pub fn get_storage_id_for_frontend_slot(slot: Option<ContentProviderUnionSlot>) -> StorageId {
    match slot {
        Some(ContentProviderUnionSlot::UserNAND) => StorageId::NandUser,
        Some(ContentProviderUnionSlot::SysNAND) => StorageId::NandSystem,
        Some(ContentProviderUnionSlot::SDMC) => StorageId::SdCard,
        Some(ContentProviderUnionSlot::FrontendManual) => StorageId::Host,
        Some(ContentProviderUnionSlot::External) => StorageId::None,
        None => StorageId::None,
    }
}

/// Build the ARP launch property using the same version/storage sources as
/// upstream `CreateApplicationProcess`. The current Rust load path still calls
/// this from `System::load` until full process-creation ownership is active.
pub fn build_application_launch_property(
    title_id: u64,
    program_index: u8,
    filesystem_controller: &Arc<Mutex<FileSystemController>>,
    content_provider: &Arc<Mutex<ContentProviderUnion>>,
) -> ApplicationLaunchProperty {
    let fs_guard = filesystem_controller.lock().unwrap();
    let content_guard = content_provider.lock().unwrap();
    let patch_manager = PatchManager::new(title_id, &fs_guard, &*content_guard);

    ApplicationLaunchProperty {
        title_id,
        version: patch_manager.get_game_version().unwrap_or(0),
        base_game_storage_id: get_storage_id_for_frontend_slot(
            content_guard.get_slot_for_entry(title_id, ContentRecordType::Program),
        ) as u8,
        update_storage_id: get_storage_id_for_frontend_slot(
            content_guard
                .get_slot_for_entry(get_update_title_id(title_id), ContentRecordType::Program),
        ) as u8,
        program_index,
        reserved: 0,
    }
}

/// Port of CreateProcess
///
/// Creates a guest process from a program NCA in storage.
///
/// Upstream implementation:
/// 1. Retrieves the program NCA from ContentProviderUnion via GetEntryRaw(program_id, Program)
/// 2. Optionally validates NCA key generation against min/max bounds
/// 3. Obtains a loader via Loader::GetLoader(system, file, program_id, program_index)
/// 4. Creates a Process and calls process->Initialize(*loader, out_load_result)
/// 5. Returns the initialized Process
///
pub fn create_process(
    system: crate::core::SystemRef,
    program_id: u64,
    min_key_gen: u8,
    max_key_gen: u8,
) -> Option<Process> {
    let system_ref = system.get();
    let storage = system_ref.get_content_provider()?;
    let nca_raw = storage
        .lock()
        .unwrap()
        .get_entry_raw(program_id, ContentRecordType::Program)?;

    if min_key_gen > 0 {
        let nca = NCA::new(nca_raw.clone(), None);
        let key_generation = nca.get_key_generation();
        if nca.get_status() == FileSysResultStatus::Success
            && (key_generation < min_key_gen || key_generation > max_key_gen)
        {
            log::warn!("Skipping program {program_id:016X} with generation {key_generation}");
            return None;
        }
    }

    let mut loader_system = LoaderSystem {
        content_provider: Some(Arc::clone(storage)),
        filesystem_controller: Some(system_ref.get_filesystem_controller()),
    };
    let mut loader = get_loader(&mut loader_system, nca_raw, program_id, 0)?;
    let mut process = Process::new();
    let mut load_result = ResultStatus::ErrorNotInitialized;
    process
        .initialize(system, loader.as_mut(), &mut load_result)
        .then_some(process)
}

/// Port of CreateApplicationProcess
///
/// Creates an application process and registers its control data with the ARP manager.
///
/// Upstream implementation:
/// 1. Calls CreateProcessImpl to obtain a loader and initialized Process
/// 2. Reads NACP control data from the loader
/// 3. Builds ApplicationLaunchProperty with program_id, version (from PatchManager),
///    base_game_storage_id, and update_storage_id
/// 4. Registers the title with system.GetARPManager()
/// 5. Returns the Process along with control data, loader, and load result
///
/// Requires: ContentProviderUnion, NCA, Loader, PatchManager, and ARP manager
/// infrastructure -- none of which are fully wired yet.
pub fn create_application_process(_program_id: u64, _program_index: u64) {
    log::warn!("(STUBBED) create_application_process called -- requires ContentProviderUnion, NCA, Loader, and ARP infrastructure");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn storage_id_mapping_matches_upstream_frontend_slots() {
        assert_eq!(get_storage_id_for_frontend_slot(None), StorageId::None);
        assert_eq!(
            get_storage_id_for_frontend_slot(Some(ContentProviderUnionSlot::UserNAND)),
            StorageId::NandUser
        );
        assert_eq!(
            get_storage_id_for_frontend_slot(Some(ContentProviderUnionSlot::SysNAND)),
            StorageId::NandSystem
        );
        assert_eq!(
            get_storage_id_for_frontend_slot(Some(ContentProviderUnionSlot::SDMC)),
            StorageId::SdCard
        );
        assert_eq!(
            get_storage_id_for_frontend_slot(Some(ContentProviderUnionSlot::FrontendManual)),
            StorageId::Host
        );
    }
}
