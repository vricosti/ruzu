// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/nvmemp.h
//! Port of zuyu/src/core/hle/service/nvdrv/nvmemp.cpp

/// IPC command table for NVMEMP:
/// - 0: Open
/// - 1: GetAruid
pub struct Nvmemp {}

impl Nvmemp {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of NVMEMP::Open
    pub fn open(&self) {
        unimplemented!("NVMEMP::Open");
    }

    /// Port of NVMEMP::GetAruid
    pub fn get_aruid(&self) {
        unimplemented!("NVMEMP::GetAruid");
    }
}
