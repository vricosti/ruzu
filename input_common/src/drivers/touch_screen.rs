// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/touch_screen.h` and `input_common/drivers/touch_screen.cpp`.
//!
//! Touch screen input driver that receives touch events and forwards them to touch devices.

use crate::input_engine::{InputEngine, PadIdentifier};

/// Maximum number of simultaneous touch points.
/// Port of TouchScreen::MAX_FINGER_COUNT
const MAX_FINGER_COUNT: usize = 16;

/// Default pad identifier for touch screen.
fn default_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 0,
        pad: 0,
    }
}

/// Port of `TouchScreen::TouchStatus` struct from touch_screen.h
#[derive(Debug, Default, Clone)]
struct TouchStatus {
    finger_id: usize,
    is_enabled: bool,
    is_active: bool,
}

/// Port of `TouchScreen` class from touch_screen.h / touch_screen.cpp
pub struct TouchScreen {
    engine: InputEngine,
    fingers: [TouchStatus; MAX_FINGER_COUNT],
}

impl TouchScreen {
    /// Port of TouchScreen::TouchScreen
    pub fn new(input_engine: String) -> Self {
        let mut ts = Self {
            engine: InputEngine::new(input_engine),
            fingers: Default::default(),
        };
        // PreSetController(identifier) + ReleaseAllTouch()
        ts.release_all_touch();
        ts
    }

    /// Returns a reference to the underlying input engine.
    pub fn engine(&self) -> &InputEngine {
        &self.engine
    }

    /// Returns a mutable reference to the underlying input engine.
    pub fn engine_mut(&mut self) -> &mut InputEngine {
        &mut self.engine
    }

    /// Signals that touch has moved and marks this touch point as active.
    /// Port of TouchScreen::TouchMoved
    pub fn touch_moved(&mut self, x: f32, y: f32, finger_id: usize) {
        let index = match self.get_index_from_finger_id(finger_id) {
            Some(i) => i,
            None => {
                // Touch doesn't exist, handle it as a new one
                self.touch_pressed(x, y, finger_id);
                return;
            }
        };
        let identifier = default_identifier();
        self.fingers[index].is_active = true;
        self.engine
            .set_button(&identifier, index as i32, true);
        self.engine
            .set_axis(&identifier, (index * 2) as i32, x);
        self.engine
            .set_axis(&identifier, (index * 2 + 1) as i32, y);
    }

    /// Signals and creates a new touch point with this finger id.
    /// Port of TouchScreen::TouchPressed
    pub fn touch_pressed(&mut self, x: f32, y: f32, finger_id: usize) {
        if self.get_index_from_finger_id(finger_id).is_some() {
            // Touch already exists. Just update the data
            self.touch_moved(x, y, finger_id);
            return;
        }
        let index = match self.get_next_free_index() {
            Some(i) => i,
            None => {
                // No free entries. Ignore input
                return;
            }
        };
        self.fingers[index].is_enabled = true;
        self.fingers[index].finger_id = finger_id;
        self.touch_moved(x, y, finger_id);
    }

    /// Signals and resets the touch point related to this finger id.
    /// Port of TouchScreen::TouchReleased
    pub fn touch_released(&mut self, finger_id: usize) {
        let index = match self.get_index_from_finger_id(finger_id) {
            Some(i) => i,
            None => return,
        };
        let identifier = default_identifier();
        self.fingers[index].is_enabled = false;
        self.engine
            .set_button(&identifier, index as i32, false);
        self.engine
            .set_axis(&identifier, (index * 2) as i32, 0.0);
        self.engine
            .set_axis(&identifier, (index * 2 + 1) as i32, 0.0);
    }

    /// Resets the active flag for each touch point.
    /// Port of TouchScreen::ClearActiveFlag
    pub fn clear_active_flag(&mut self) {
        for finger in self.fingers.iter_mut() {
            finger.is_active = false;
        }
    }

    /// Releases all touch that haven't been marked as active.
    /// Port of TouchScreen::ReleaseInactiveTouch
    pub fn release_inactive_touch(&mut self) {
        // Collect finger_ids of inactive touches first to avoid borrow issues
        let inactive_ids: Vec<usize> = self
            .fingers
            .iter()
            .filter(|f| !f.is_active)
            .map(|f| f.finger_id)
            .collect();
        for finger_id in inactive_ids {
            self.touch_released(finger_id);
        }
    }

    /// Resets all inputs to their initial value.
    /// Port of TouchScreen::ReleaseAllTouch
    pub fn release_all_touch(&mut self) {
        let enabled_ids: Vec<usize> = self
            .fingers
            .iter()
            .filter(|f| f.is_enabled)
            .map(|f| f.finger_id)
            .collect();
        for finger_id in enabled_ids {
            self.touch_released(finger_id);
        }
    }

    // ---- Private methods ----

    /// Port of TouchScreen::GetIndexFromFingerId
    fn get_index_from_finger_id(&self, finger_id: usize) -> Option<usize> {
        for index in 0..MAX_FINGER_COUNT {
            let finger = &self.fingers[index];
            if !finger.is_enabled {
                continue;
            }
            if finger.finger_id == finger_id {
                return Some(index);
            }
        }
        None
    }

    /// Port of TouchScreen::GetNextFreeIndex
    fn get_next_free_index(&self) -> Option<usize> {
        for index in 0..MAX_FINGER_COUNT {
            if !self.fingers[index].is_enabled {
                return Some(index);
            }
        }
        None
    }
}
