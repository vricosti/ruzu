// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/display_list.h

use super::display::Display;
use super::vi_types::DisplayName;

pub struct DisplayList {
    displays: [Display; 8],
    next_id: u64,
}

impl Default for DisplayList {
    fn default() -> Self {
        Self {
            displays: Default::default(),
            next_id: 0,
        }
    }
}

impl DisplayList {
    pub fn create_display(&mut self, name: &DisplayName) -> bool {
        let idx = self.displays.iter().position(|d| !d.is_initialized());
        if let Some(idx) = idx {
            let id = self.next_id;
            self.next_id += 1;
            self.displays[idx].initialize(id, name);
            true
        } else {
            false
        }
    }

    pub fn destroy_display(&mut self, display_id: u64) -> bool {
        if let Some(display) = self.get_display_by_id_mut(display_id) {
            display.finalize();
            true
        } else {
            false
        }
    }

    pub fn get_display_by_name(&self, name: &DisplayName) -> Option<&Display> {
        self.displays.iter().find(|d| {
            d.is_initialized()
                && d.get_display_name()[..] == name[..]
        })
    }

    pub fn get_display_by_id(&self, display_id: u64) -> Option<&Display> {
        self.displays
            .iter()
            .find(|d| d.is_initialized() && d.get_id() == display_id)
    }

    pub fn get_display_by_id_mut(&mut self, display_id: u64) -> Option<&mut Display> {
        self.displays
            .iter_mut()
            .find(|d| d.is_initialized() && d.get_id() == display_id)
    }

    pub fn for_each_display<F: FnMut(&Display)>(&self, mut cb: F) {
        for display in &self.displays {
            if display.is_initialized() {
                cb(display);
            }
        }
    }

    pub fn for_each_display_mut<F: FnMut(&mut Display)>(&mut self, mut cb: F) {
        for display in &mut self.displays {
            if display.is_initialized() {
                cb(display);
            }
        }
    }

    fn get_free_display(&mut self) -> Option<&mut Display> {
        self.displays.iter_mut().find(|d| !d.is_initialized())
    }
}
