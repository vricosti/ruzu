// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/error.h and error.cpp
//! Error display applet interface.

use super::applet::Applet;

/// Result code type.
///
/// Corresponds to upstream `Result` (core/hle/result.h).
/// TODO: Import from hle::result when available.
#[derive(Debug, Clone, Copy)]
pub struct ResultCode {
    pub raw: u32,
}

impl ResultCode {
    pub fn get_module(&self) -> u32 {
        self.raw & 0x1FF
    }

    pub fn get_description(&self) -> u32 {
        (self.raw >> 9) & 0x1FFF
    }
}

/// Callback type for when error display is finished.
///
/// Corresponds to upstream `ErrorApplet::FinishedCallback`.
pub type FinishedCallback = Box<dyn FnOnce() + Send>;

/// Error applet trait.
///
/// Corresponds to upstream `Core::Frontend::ErrorApplet`.
pub trait ErrorApplet: Applet {
    fn show_error(&self, error: ResultCode, finished: FinishedCallback);

    fn show_error_with_timestamp(
        &self,
        error: ResultCode,
        time_seconds: i64,
        finished: FinishedCallback,
    );

    fn show_custom_error_text(
        &self,
        error: ResultCode,
        dialog_text: String,
        fullscreen_text: String,
        finished: FinishedCallback,
    );
}

/// Default (stub) error applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultErrorApplet`.
pub struct DefaultErrorApplet;

impl Applet for DefaultErrorApplet {
    fn close(&self) {}
}

impl ErrorApplet for DefaultErrorApplet {
    fn show_error(&self, error: ResultCode, _finished: FinishedCallback) {
        log::error!(
            "Application requested error display: {:04}-{:04} (raw={:08X})",
            error.get_module(),
            error.get_description(),
            error.raw
        );
    }

    fn show_error_with_timestamp(
        &self,
        error: ResultCode,
        time: i64,
        _finished: FinishedCallback,
    ) {
        log::error!(
            "Application requested error display: {:04X}-{:04X} (raw={:08X}) with timestamp={:016X}",
            error.get_module(),
            error.get_description(),
            error.raw,
            time
        );
    }

    fn show_custom_error_text(
        &self,
        error: ResultCode,
        main_text: String,
        detail_text: String,
        _finished: FinishedCallback,
    ) {
        log::error!(
            "Application requested custom error with error_code={:04X}-{:04X} (raw={:08X})",
            error.get_module(),
            error.get_description(),
            error.raw
        );
        log::error!("    Main Text: {}", main_text);
        log::error!("    Detail Text: {}", detail_text);
    }
}
