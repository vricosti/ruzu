// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/touch_screen/gesture_handler.h and gesture_handler.cpp

use super::touch_types::*;

fn square(num: i32) -> f32 {
    (num as f32) * (num as f32)
}

/// Properties computed from current touch state for gesture detection.
#[derive(Debug, Clone, Default)]
pub struct GestureProperties {
    pub active_points: usize,
    pub mid_point: [i32; 2],
    pub points: [[i32; 2]; MAX_POINTS],
    pub average_distance: f32,
    pub angle: f32,
    pub detection_count: i64,
}

/// Processes raw touch input into gesture events (tap, pan, swipe, pinch,
/// rotate, etc.), matching upstream gesture_handler.cpp logic.
pub struct GestureHandler {
    gesture: GestureProperties,
    last_gesture: GestureProperties,
    last_gesture_state: GestureState,
    last_update_timestamp: i64,
    last_tap_timestamp: i64,
    last_pan_time_difference: f32,
    time_difference: f32,
    force_update: bool,
    enable_press_and_tap: bool,
}

impl GestureHandler {
    pub fn new() -> Self {
        Self {
            gesture: GestureProperties::default(),
            last_gesture: GestureProperties::default(),
            last_gesture_state: GestureState::default(),
            last_update_timestamp: 0,
            last_tap_timestamp: 0,
            last_pan_time_difference: 0.0,
            time_difference: 0.0,
            force_update: true,
            enable_press_and_tap: false,
        }
    }

    /// Port of GestureHandler::SetTouchState.
    pub fn set_touch_state(&mut self, touch_state: &[TouchState], count: u32, mut timestamp: i64) {
        self.gesture = GestureProperties::default();
        self.gesture.active_points = (count as usize).min(MAX_POINTS);

        for id in 0..self.gesture.active_points {
            let active_x = touch_state[id].position_x as i32;
            let active_y = touch_state[id].position_y as i32;
            self.gesture.points[id] = [active_x, active_y];

            self.gesture.mid_point[0] +=
                self.gesture.points[id][0] / self.gesture.active_points as i32;
            self.gesture.mid_point[1] +=
                self.gesture.points[id][1] / self.gesture.active_points as i32;
        }

        for id in 0..self.gesture.active_points {
            let distance = (square(self.gesture.mid_point[0] - self.gesture.points[id][0])
                + square(self.gesture.mid_point[1] - self.gesture.points[id][1]))
            .sqrt();
            self.gesture.average_distance += distance / self.gesture.active_points as f32;
        }

        self.gesture.angle = ((self.gesture.mid_point[1] - self.gesture.points[0][1]) as f32)
            .atan2((self.gesture.mid_point[0] - self.gesture.points[0][0]) as f32);

        self.gesture.detection_count = self.last_gesture.detection_count;

        if self.last_update_timestamp > timestamp {
            timestamp = self.last_tap_timestamp;
        }

        self.time_difference =
            (timestamp - self.last_update_timestamp) as f32 / (1000.0 * 1000.0 * 1000.0);
    }

    /// Port of GestureHandler::NeedsUpdate.
    pub fn needs_update(&mut self) -> bool {
        if self.force_update {
            self.force_update = false;
            return true;
        }

        for id in 0..MAX_POINTS {
            if self.gesture.points[id] != self.last_gesture.points[id] {
                return true;
            }
        }

        // Update on press and hold event after 0.5 seconds
        if self.last_gesture_state.gesture_type == GestureType::Touch
            && self.last_gesture_state.point_count == 1
            && self.time_difference > PRESS_DELAY
        {
            return self.enable_press_and_tap;
        }

        false
    }

    /// Port of GestureHandler::UpdateGestureState.
    pub fn update_gesture_state(&mut self, next_state: &mut GestureState, timestamp: i64) {
        self.last_update_timestamp = timestamp;

        let mut gesture_type = GestureType::Idle;
        let mut attributes = GestureAttribute { raw: 0 };

        next_state.sampling_number = self.last_gesture_state.sampling_number + 1;
        next_state.delta_x = 0;
        next_state.delta_y = 0;
        next_state.vel_x = 0.0;
        next_state.vel_y = 0.0;
        next_state.direction = GestureDirection::None;
        next_state.rotation_angle = 0.0;
        next_state.scale = 0.0;

        if self.gesture.active_points > 0 {
            if self.last_gesture.active_points == 0 {
                self.new_gesture(&mut gesture_type, &mut attributes);
            } else {
                self.update_existing_gesture(next_state, &mut gesture_type);
            }
        } else {
            self.end_gesture(next_state, &mut gesture_type, &mut attributes);
        }

        next_state.detection_count = self.gesture.detection_count;
        next_state.gesture_type = gesture_type;
        next_state.attributes = attributes;
        next_state.pos_x = self.gesture.mid_point[0];
        next_state.pos_y = self.gesture.mid_point[1];
        next_state.point_count = self.gesture.active_points as i32;
        next_state.points = self.gesture.points;
        self.last_gesture = self.gesture.clone();
        self.last_gesture_state = *next_state;
    }

    fn new_gesture(&mut self, gesture_type: &mut GestureType, attributes: &mut GestureAttribute) {
        self.gesture.detection_count += 1;
        *gesture_type = GestureType::Touch;

        if self.last_gesture_state.gesture_type != GestureType::Cancel {
            attributes.raw |= 1 << 4; // is_new_touch
            self.enable_press_and_tap = true;
        }
    }

    fn update_existing_gesture(
        &mut self,
        next_state: &mut GestureState,
        gesture_type: &mut GestureType,
    ) {
        for id in 0..MAX_POINTS {
            if self.gesture.points[id] != self.last_gesture.points[id] {
                *gesture_type = GestureType::Pan;
                break;
            }
        }

        // Number of fingers changed: cancel
        if self.gesture.active_points != self.last_gesture.active_points {
            *gesture_type = GestureType::Cancel;
            self.enable_press_and_tap = false;
            self.gesture.active_points = 0;
            self.gesture.mid_point = [0, 0];
            self.gesture.points = [[0; 2]; MAX_POINTS];
            return;
        }

        if *gesture_type == GestureType::Pan {
            self.update_pan_event(next_state, gesture_type);
            return;
        }

        if self.last_gesture_state.gesture_type == GestureType::Touch {
            *gesture_type = GestureType::Press;
        }
    }

    fn end_gesture(
        &mut self,
        next_state: &mut GestureState,
        gesture_type: &mut GestureType,
        attributes: &mut GestureAttribute,
    ) {
        if self.last_gesture.active_points != 0 {
            match self.last_gesture_state.gesture_type {
                GestureType::Touch => {
                    if self.enable_press_and_tap {
                        self.set_tap_event(gesture_type, attributes);
                        return;
                    }
                    *gesture_type = GestureType::Cancel;
                    self.force_update = true;
                }
                GestureType::Press
                | GestureType::Tap
                | GestureType::Swipe
                | GestureType::Pinch
                | GestureType::Rotate => {
                    *gesture_type = GestureType::Complete;
                    self.force_update = true;
                }
                GestureType::Pan => {
                    self.end_pan_event(next_state, gesture_type);
                }
                _ => {}
            }
            return;
        }
        if self.last_gesture_state.gesture_type == GestureType::Complete
            || self.last_gesture_state.gesture_type == GestureType::Cancel
        {
            self.gesture.detection_count += 1;
        }
    }

    fn set_tap_event(&mut self, gesture_type: &mut GestureType, attributes: &mut GestureAttribute) {
        *gesture_type = GestureType::Tap;
        self.gesture = self.last_gesture.clone();
        self.force_update = true;
        let tap_time_difference = (self.last_update_timestamp - self.last_tap_timestamp) as f32
            / (1000.0 * 1000.0 * 1000.0);
        self.last_tap_timestamp = self.last_update_timestamp;
        if tap_time_difference < DOUBLE_TAP_DELAY {
            attributes.raw |= 1 << 8; // is_double_tap
        }
    }

    fn update_pan_event(&mut self, next_state: &mut GestureState, gesture_type: &mut GestureType) {
        next_state.delta_x = self.gesture.mid_point[0] - self.last_gesture_state.pos_x;
        next_state.delta_y = self.gesture.mid_point[1] - self.last_gesture_state.pos_y;
        next_state.vel_x = next_state.delta_x as f32 / self.time_difference;
        next_state.vel_y = next_state.delta_y as f32 / self.time_difference;
        self.last_pan_time_difference = self.time_difference;

        // Promote to pinch
        if (self.gesture.average_distance - self.last_gesture.average_distance).abs()
            > PINCH_THRESHOLD
        {
            *gesture_type = GestureType::Pinch;
            next_state.scale = self.gesture.average_distance / self.last_gesture.average_distance;
        }

        let angle_between_two_lines = ((self.gesture.angle - self.last_gesture.angle)
            / (1.0 + (self.gesture.angle * self.last_gesture.angle)))
            .atan();
        // Promote to rotate
        if angle_between_two_lines.abs() > ANGLE_THRESHOLD {
            *gesture_type = GestureType::Rotate;
            next_state.scale = 0.0;
            next_state.rotation_angle = angle_between_two_lines * 180.0 / std::f32::consts::PI;
        }
    }

    fn end_pan_event(&mut self, next_state: &mut GestureState, gesture_type: &mut GestureType) {
        next_state.vel_x = self.last_gesture_state.delta_x as f32
            / (self.last_pan_time_difference + self.time_difference);
        next_state.vel_y = self.last_gesture_state.delta_y as f32
            / (self.last_pan_time_difference + self.time_difference);
        let curr_vel =
            ((next_state.vel_x * next_state.vel_x) + (next_state.vel_y * next_state.vel_y)).sqrt();

        if curr_vel > SWIPE_THRESHOLD {
            self.set_swipe_event(next_state, gesture_type);
            return;
        }

        *gesture_type = GestureType::Complete;
        next_state.vel_x = 0.0;
        next_state.vel_y = 0.0;
        self.force_update = true;
    }

    fn set_swipe_event(&mut self, next_state: &mut GestureState, gesture_type: &mut GestureType) {
        *gesture_type = GestureType::Swipe;
        self.gesture = self.last_gesture.clone();
        self.force_update = true;
        next_state.delta_x = self.last_gesture_state.delta_x;
        next_state.delta_y = self.last_gesture_state.delta_y;

        if next_state.delta_x.abs() > next_state.delta_y.abs() {
            if next_state.delta_x > 0 {
                next_state.direction = GestureDirection::Right;
            } else {
                next_state.direction = GestureDirection::Left;
            }
        } else if next_state.delta_y > 0 {
            next_state.direction = GestureDirection::Down;
        } else {
            next_state.direction = GestureDirection::Up;
        }
    }
}

impl Default for GestureHandler {
    fn default() -> Self {
        Self::new()
    }
}
