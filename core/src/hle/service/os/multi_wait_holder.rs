// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/multi_wait_holder.h
//! Port of zuyu/src/core/hle/service/os/multi_wait_holder.cpp
//!
//! MultiWaitHolder — holds a synchronization object for MultiWait.

/// MultiWaitHolder — holds a native synchronization object handle.
///
/// Corresponds to `MultiWaitHolder` in upstream multi_wait_holder.h / multi_wait_holder.cpp.
pub struct MultiWaitHolder {
    user_data: usize,
    // TODO: native_handle, list linkage, multi_wait reference
}

impl MultiWaitHolder {
    pub fn new() -> Self {
        Self { user_data: 0 }
    }

    /// Set user data associated with this holder.
    pub fn set_user_data(&mut self, data: usize) {
        self.user_data = data;
    }

    /// Get user data associated with this holder.
    pub fn get_user_data(&self) -> usize {
        self.user_data
    }

    /// Link this holder to a MultiWait.
    pub fn link_to_multi_wait(&mut self) {
        // TODO: implement list linkage
    }

    /// Unlink this holder from its MultiWait.
    pub fn unlink_from_multi_wait(&mut self) {
        // TODO: implement list unlinkage
    }
}
