// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/process_creation.h
//! Port of zuyu/src/core/hle/service/am/process_creation.cpp

/// Port of CreateProcess
///
/// Creates a guest process from a program NCA in storage.
/// Stubbed: requires filesystem, loader, and process infrastructure.
pub fn create_process(_program_id: u64, _min_key_gen: u8, _max_key_gen: u8) {
    // TODO: implement when loader/process infrastructure is available
    log::warn!("(STUBBED) create_process called");
}

/// Port of CreateApplicationProcess
///
/// Creates an application process and registers its control data.
/// Stubbed: requires filesystem, loader, and process infrastructure.
pub fn create_application_process(_program_id: u64, _program_index: u64) {
    // TODO: implement when loader/process infrastructure is available
    log::warn!("(STUBBED) create_application_process called");
}
