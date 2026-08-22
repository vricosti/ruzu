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
        let Some(display) = Self::get_free_display(&mut self.displays) else {
            return false;
        };

        let id = self.next_id;
        self.next_id += 1;
        display.initialize(id, name);
        true
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
        self.displays
            .iter()
            .find(|d| d.is_initialized() && d.get_display_name()[..] == name[..])
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

    fn get_free_display(displays: &mut [Display; 8]) -> Option<&mut Display> {
        displays
            .iter_mut()
            .find(|display| !display.is_initialized())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn display_name(index: u8) -> DisplayName {
        let mut name = [0; 0x40];
        name[0] = index;
        name
    }

    #[test]
    fn create_display_uses_first_free_slot_without_advancing_id_when_full() {
        let mut displays = DisplayList::default();
        for index in 0..8 {
            assert!(displays.create_display(&display_name(index)));
            assert_eq!(
                displays
                    .get_display_by_name(&display_name(index))
                    .unwrap()
                    .get_id(),
                index as u64
            );
        }

        assert!(!displays.create_display(&display_name(8)));
        assert!(displays.destroy_display(3));
        assert!(displays.create_display(&display_name(9)));
        assert_eq!(
            displays
                .get_display_by_name(&display_name(9))
                .unwrap()
                .get_id(),
            8
        );
    }
}
