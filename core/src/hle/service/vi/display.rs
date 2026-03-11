// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/display.h

use super::vi_types::DisplayName;

#[derive(Clone)]
pub struct Display {
    id: u64,
    display_name: DisplayName,
    is_initialized: bool,
}

impl Default for Display {
    fn default() -> Self {
        Self {
            id: 0,
            display_name: [0u8; 0x40],
            is_initialized: false,
        }
    }
}

impl Display {
    pub fn initialize(&mut self, id: u64, display_name: &DisplayName) {
        self.id = id;
        self.display_name = *display_name;
        self.is_initialized = true;
    }

    pub fn finalize(&mut self) {
        self.id = 0;
        self.display_name = [0u8; 0x40];
        self.is_initialized = false;
    }

    pub fn get_id(&self) -> u64 {
        self.id
    }

    pub fn get_display_name(&self) -> &DisplayName {
        &self.display_name
    }

    pub fn is_initialized(&self) -> bool {
        self.is_initialized
    }
}
