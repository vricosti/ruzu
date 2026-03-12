// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/web_service/
//!
//! Web service client for room announcements, telemetry, user login
//! verification, and JWT-based user verification.

pub mod announce_room_json;
pub mod telemetry_json;
pub mod verify_login;
pub mod verify_user_jwt;
pub mod web_backend;
pub mod web_result;
