// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_error.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_error.cpp

/// Port of ErrorAppletMode
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorAppletMode {
    ShowError = 0,
    ShowSystemError = 1,
    ShowApplicationError = 2,
    ShowEula = 3,
    ShowErrorPctl = 4,
    ShowErrorRecord = 5,
    ShowUpdateEula = 8,
}

/// Error frontend applet stub.
pub struct Error {
    complete: bool,
    mode: ErrorAppletMode,
}

impl Error {
    pub fn new() -> Self {
        Self {
            complete: false,
            mode: ErrorAppletMode::ShowError,
        }
    }
}
