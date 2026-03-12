// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/touch_screen/touch_types.h and touch_types.cpp

use crate::hid_types::{TouchAttribute, TouchScreenModeForNx};

pub const MAX_FINGERS: usize = 16;
pub const MAX_POINTS: usize = 4;
pub const TOUCH_SENSOR_WIDTH: u32 = 1280;
pub const TOUCH_SENSOR_HEIGHT: u32 = 720;
pub const MAX_ROTATION_ANGLE: i32 = 270;
pub const MAX_TOUCH_DIAMETER: u32 = 30;
pub const TOUCH_BORDERS: u32 = 15;

pub const SWIPE_THRESHOLD: f32 = 400.0;
pub const ANGLE_THRESHOLD: f32 = 0.015;
pub const PINCH_THRESHOLD: f32 = 0.5;
pub const PRESS_DELAY: f32 = 0.5;
pub const DOUBLE_TAP_DELAY: f32 = 0.35;

/// This is nn::hid::GestureType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum GestureType {
    #[default]
    Idle = 0,
    Complete = 1,
    Cancel = 2,
    Touch = 3,
    Press = 4,
    Tap = 5,
    Pan = 6,
    Swipe = 7,
    Pinch = 8,
    Rotate = 9,
}

/// This is nn::hid::GestureDirection
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum GestureDirection {
    #[default]
    None = 0,
    Left = 1,
    Up = 2,
    Right = 3,
    Down = 4,
}

/// This is nn::hid::GestureAttribute
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct GestureAttribute {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<GestureAttribute>() == 4);

/// This is nn::hid::GestureState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct GestureState {
    pub sampling_number: i64,
    pub detection_count: i64,
    pub gesture_type: GestureType,
    pub direction: GestureDirection,
    pub pos_x: i32,
    pub pos_y: i32,
    pub delta_x: i32,
    pub delta_y: i32,
    pub vel_x: f32,
    pub vel_y: f32,
    pub attributes: GestureAttribute,
    pub scale: f32,
    pub rotation_angle: f32,
    pub point_count: i32,
    pub points: [[i32; 2]; 4],
}
const _: () = assert!(std::mem::size_of::<GestureState>() == 0x60);

/// This is nn::hid::TouchState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct TouchState {
    pub delta_time: u64,
    pub attribute: TouchAttribute,
    pub finger: u32,
    pub position_x: u32,
    pub position_y: u32,
    pub diameter_x: u32,
    pub diameter_y: u32,
    pub rotation_angle: i32,
}
const _: () = assert!(std::mem::size_of::<TouchState>() == 0x28);

/// This is nn::hid::TouchScreenState
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct TouchScreenState {
    pub sampling_number: i64,
    pub entry_count: i32,
    pub _reserved: [u8; 4],
    pub states: [TouchState; MAX_FINGERS],
}
const _: () = assert!(std::mem::size_of::<TouchScreenState>() == 0x290);

/// Upstream TouchFingerMap tracks finger mapping per-aruid for touch screen processing.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TouchFingerMap {
    pub finger_count: i32,
    pub touch_mode: TouchScreenModeForNx,
    pub _padding: [u8; 3],
    pub finger_ids: [u32; MAX_FINGERS],
}
const _: () = assert!(std::mem::size_of::<TouchFingerMap>() == 0x48);

impl Default for TouchFingerMap {
    fn default() -> Self {
        Self {
            finger_count: 0,
            touch_mode: TouchScreenModeForNx::UseSystemSetting,
            _padding: [0; 3],
            finger_ids: [0; MAX_FINGERS],
        }
    }
}

/// Per-aruid touch data tracked by TouchResource.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TouchAruidData {
    pub aruid: u64,
    pub basic_gesture_id: u32,
    pub used_1: u64,
    pub used_2: u64,
    pub used_3: u64,
    pub used_4: u64,
    pub gesture_type: GestureType,
    pub resolution_width: u16,
    pub resolution_height: u16,
    pub finger_map: TouchFingerMap,
}
const _: () = assert!(std::mem::size_of::<TouchAruidData>() == 0x80);

impl Default for TouchAruidData {
    fn default() -> Self {
        Self {
            aruid: 0,
            basic_gesture_id: 0,
            used_1: 0,
            used_2: 0,
            used_3: 0,
            used_4: 0,
            gesture_type: GestureType::Idle,
            resolution_width: 0,
            resolution_height: 0,
            finger_map: TouchFingerMap::default(),
        }
    }
}

/// State for touch screen auto-pilot injection.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct AutoPilotState {
    pub count: u64,
    pub state: [TouchState; MAX_FINGERS],
}
const _: () = assert!(std::mem::size_of::<AutoPilotState>() == 0x288);

impl Default for AutoPilotState {
    fn default() -> Self {
        Self {
            count: 0,
            state: [TouchState::default(); MAX_FINGERS],
        }
    }
}
