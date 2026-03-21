// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/process_creation.h
//! Port of zuyu/src/core/hle/service/am/process_creation.cpp

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
/// Requires: ContentProviderUnion, NCA parsing, Loader infrastructure, and
/// service-layer Process initialization -- none of which are fully wired yet.
pub fn create_process(_program_id: u64, _min_key_gen: u8, _max_key_gen: u8) {
    log::warn!("(STUBBED) create_process called -- requires ContentProviderUnion, NCA, and Loader infrastructure");
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
