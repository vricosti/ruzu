// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/process_holder.h
//! Port of zuyu/src/core/hle/service/am/process_holder.cpp

/// Port of ProcessHolder
///
/// Holds a reference to an Applet and its Process for multi-wait observation.
/// Stubbed: requires MultiWaitHolder and Process infrastructure.
pub struct ProcessHolder {
    // TODO: Applet reference, Process reference, MultiWaitHolder base
}

impl ProcessHolder {
    pub fn new() -> Self {
        Self {}
    }
}
