// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of Eden `src/core/hle/service/am/process_creation.h` and `process_creation.cpp`.

use std::sync::{Arc, Mutex};

use crate::file_sys::content_archive::NCA;
use crate::file_sys::control_metadata::{RawNACP, NACP};
use crate::file_sys::nca_metadata::ContentRecordType;
use crate::file_sys::partition_filesystem::ResultStatus as FileSysResultStatus;
use crate::file_sys::patch_manager::PatchManager;
use crate::file_sys::registered_cache::{
    get_update_title_id, ContentProvider, ContentProviderUnion, ContentProviderUnionSlot,
};
use crate::file_sys::romfs_factory::StorageId;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::hle::service::filesystem::filesystem::FileSystemController;
use crate::hle::service::glue::glue_manager::ApplicationLaunchProperty;
use crate::hle::service::os::process::Process;
use crate::loader::loader::{get_loader, AppLoader, ResultStatus, System as LoaderSystem};

/// Port of upstream anonymous `CreateProcessImpl`.
fn create_process_impl(
    out_loader: &mut Option<Box<dyn AppLoader>>,
    out_load_result: &mut ResultStatus,
    system: crate::core::SystemRef,
    file: VirtualFile,
    program_id: u64,
    program_index: u64,
) -> Option<Process> {
    let system_ref = system.get();
    let mut loader_system = LoaderSystem::new(
        system_ref.get_content_provider().cloned(),
        Some(system_ref.get_filesystem_controller()),
    );
    *out_loader = get_loader(&mut loader_system, file, program_id, program_index as usize);

    let loader = out_loader.as_deref_mut()?;
    let mut process = Process::new();
    process
        .initialize(system, loader, out_load_result)
        .then_some(process)
}

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

    let mut load_result = ResultStatus::ErrorNotInitialized;
    let mut loader = None;
    create_process_impl(
        &mut loader,
        &mut load_result,
        system,
        nca_raw,
        program_id,
        0,
    )
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
pub fn create_application_process(
    out_control: &mut Vec<u8>,
    out_loader: &mut Option<Box<dyn AppLoader>>,
    out_load_result: &mut ResultStatus,
    system: crate::core::SystemRef,
    file: VirtualFile,
    program_id: u64,
    program_index: u64,
) -> Option<Process> {
    let process = create_process_impl(
        out_loader,
        out_load_result,
        system,
        file,
        program_id,
        program_index,
    )?;

    let mut nacp = NACP::new();
    if out_loader
        .as_deref()
        .is_some_and(|loader| loader.read_control_data(&mut nacp) == ResultStatus::Success)
    {
        *out_control = nacp.get_raw_bytes();
    } else {
        out_control.resize(std::mem::size_of::<RawNACP>(), 0);
        out_control.fill(0);
    }

    let system_ref = system.get();
    let storage = system_ref.get_content_provider()?;
    let launch = build_application_launch_property(
        process.get_program_id(),
        0,
        &system_ref.get_filesystem_controller(),
        storage,
    );
    let _ = system_ref.arp_manager().lock().unwrap().register(
        launch.title_id,
        launch,
        out_control.clone(),
    );

    Some(process)
}

/// Port of upstream `ReinitializeProcess`.
pub fn reinitialize_process(
    system: crate::core::SystemRef,
    process: &mut Process,
    program_id: u64,
) -> bool {
    let system_ref = system.get();
    let Some(storage) = system_ref.get_content_provider() else {
        return false;
    };
    let Some(nca_raw) = storage
        .lock()
        .unwrap()
        .get_entry_raw(program_id, ContentRecordType::Program)
    else {
        return false;
    };

    let mut loader_system = LoaderSystem::new(
        Some(Arc::clone(storage)),
        Some(system_ref.get_filesystem_controller()),
    );
    let Some(mut loader) = get_loader(&mut loader_system, nca_raw, program_id, 0) else {
        return false;
    };

    let mut status = ResultStatus::ErrorNotInitialized;
    process.initialize(system, loader.as_mut(), &mut status)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;

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

    #[test]
    fn create_process_impl_retains_loader_when_process_initialization_fails() {
        let system = crate::core::System::new();
        let system_ref = crate::core::SystemRef::from_ref(&system);
        let file: VirtualFile = Arc::new(VectorVfsFile::new(
            Vec::new(),
            "homebrew.nro".to_string(),
            None,
        ));
        let mut loader = None;
        let mut load_result = ResultStatus::Success;

        let process = create_process_impl(&mut loader, &mut load_result, system_ref, file, 0, 0);

        assert!(process.is_none());
        assert!(loader.is_some());
        assert_eq!(load_result, ResultStatus::ErrorNotInitialized);
    }

    #[test]
    fn reinitialize_process_fails_without_a_content_provider() {
        let system = crate::core::System::new();
        let mut process = Process::new();

        assert!(!reinitialize_process(
            crate::core::SystemRef::from_ref(&system),
            &mut process,
            0,
        ));
    }
}
