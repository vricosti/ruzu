// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/glue_manager.h
//! Port of zuyu/src/core/hle/service/glue/glue_manager.cpp

use super::errors;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use std::collections::BTreeMap;

/// ApplicationLaunchProperty corresponds to upstream `ApplicationLaunchProperty` in `glue_manager.h`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ApplicationLaunchProperty {
    pub title_id: u64,
    pub version: u32,
    pub base_game_storage_id: u8,
    pub update_storage_id: u8,
    pub program_index: u8,
    pub reserved: u8,
}

const _: () = assert!(core::mem::size_of::<ApplicationLaunchProperty>() == 0x10);

struct MapEntry {
    launch: ApplicationLaunchProperty,
    control: Vec<u8>,
}

/// ARPManager manages registration of application launch and control properties.
///
/// Corresponds to `ARPManager` in upstream `glue_manager.h`.
pub struct ARPManager {
    entries: BTreeMap<u64, MapEntry>,
}

impl ARPManager {
    pub fn new() -> Self {
        Self {
            entries: BTreeMap::new(),
        }
    }

    pub fn get_launch_property(
        &self,
        title_id: u64,
    ) -> (ResultCode, Option<ApplicationLaunchProperty>) {
        if title_id == 0 {
            return (errors::RESULT_INVALID_PROCESS_ID, None);
        }
        match self.entries.get(&title_id) {
            Some(entry) => (RESULT_SUCCESS, Some(entry.launch)),
            None => (errors::RESULT_PROCESS_ID_NOT_REGISTERED, None),
        }
    }

    pub fn get_control_property(&self, title_id: u64) -> (ResultCode, Option<Vec<u8>>) {
        if title_id == 0 {
            return (errors::RESULT_INVALID_PROCESS_ID, None);
        }
        match self.entries.get(&title_id) {
            Some(entry) => (RESULT_SUCCESS, Some(entry.control.clone())),
            None => (errors::RESULT_PROCESS_ID_NOT_REGISTERED, None),
        }
    }

    pub fn register(
        &mut self,
        title_id: u64,
        launch: ApplicationLaunchProperty,
        control: Vec<u8>,
    ) -> ResultCode {
        if title_id == 0 {
            return errors::RESULT_INVALID_PROCESS_ID;
        }
        if self.entries.contains_key(&title_id) {
            return errors::RESULT_ALREADY_BOUND;
        }
        self.entries.insert(title_id, MapEntry { launch, control });
        RESULT_SUCCESS
    }

    pub fn unregister(&mut self, title_id: u64) -> ResultCode {
        if title_id == 0 {
            return errors::RESULT_INVALID_PROCESS_ID;
        }
        if self.entries.remove(&title_id).is_none() {
            return errors::RESULT_PROCESS_ID_NOT_REGISTERED;
        }
        RESULT_SUCCESS
    }

    pub fn reset_all(&mut self) {
        self.entries.clear();
    }
}
