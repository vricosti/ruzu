// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/touch_screen.h` and `input_common/drivers/touch_screen.cpp`.
//!
//! Touch screen input driver that receives touch events and forwards them to touch devices.

use crate::input_engine::InputEngine;

/// Maximum number of simultaneous touch points.
/// Port of TouchScreen::MAX_FINGER_COUNT
const MAX_FINGER_COUNT: usize = 16;

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
        Self {
            engine: InputEngine::new(input_engine),
            fingers: Default::default(),
        }
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
    pub fn touch_moved(&mut self, _x: f32, _y: f32, _finger_id: usize) {
        todo!()
    }

    /// Signals and creates a new touch point with this finger id.
    /// Port of TouchScreen::TouchPressed
    pub fn touch_pressed(&mut self, _x: f32, _y: f32, _finger_id: usize) {
        todo!()
    }

    /// Signals and resets the touch point related to this finger id.
    /// Port of TouchScreen::TouchReleased
    pub fn touch_released(&mut self, _finger_id: usize) {
        todo!()
    }

    /// Resets the active flag for each touch point.
    /// Port of TouchScreen::ClearActiveFlag
    pub fn clear_active_flag(&mut self) {
        todo!()
    }

    /// Releases all touch that haven't been marked as active.
    /// Port of TouchScreen::ReleaseInactiveTouch
    pub fn release_inactive_touch(&mut self) {
        todo!()
    }

    /// Resets all inputs to their initial value.
    /// Port of TouchScreen::ReleaseAllTouch
    pub fn release_all_touch(&mut self) {
        todo!()
    }

    // ---- Private methods ----

    /// Port of TouchScreen::GetIndexFromFingerId
    fn get_index_from_finger_id(&self, _finger_id: usize) -> Option<usize> {
        todo!()
    }

    /// Port of TouchScreen::GetNextFreeIndex
    fn get_next_free_index(&self) -> Option<usize> {
        todo!()
    }
}
