// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/frontend/input_converter.h and input_converter.cpp
//!
//! Provides conversion functions from raw input data into valid HID status types.

// TODO: Port all conversion functions. These depend on common::input types.
// Functions to port:
// - transform_to_battery()
// - transform_to_button()
// - transform_to_motion()
// - transform_to_stick()
// - transform_to_touch()
// - transform_to_trigger()
// - transform_to_analog()
// - transform_to_camera()
// - transform_to_nfc()
// - transform_to_color()
// - sanitize_analog()
// - sanitize_stick()
