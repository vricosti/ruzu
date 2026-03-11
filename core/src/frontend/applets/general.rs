// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/general.h and general.cpp
//! Parental controls and photo viewer applet interfaces.

use super::applet::Applet;

// ---------------------------------------------------------------------------
// Parental Controls Applet
// ---------------------------------------------------------------------------

/// Parental controls applet trait.
///
/// Corresponds to upstream `Core::Frontend::ParentalControlsApplet`.
pub trait ParentalControlsApplet: Applet {
    /// Prompts the user to enter a PIN and calls the callback with whether or not it matches the
    /// correct PIN. If the bool is passed, and the PIN was recently entered correctly, the frontend
    /// should not prompt and simply return true.
    fn verify_pin(
        &mut self,
        finished: Box<dyn FnOnce(bool) + Send>,
        suspend_future_verification_temporarily: bool,
    );

    /// Prompts the user to enter a PIN and calls the callback for correctness. Frontends can
    /// optionally alert the user that this is to change parental controls settings.
    fn verify_pin_for_settings(&mut self, finished: Box<dyn FnOnce(bool) + Send>);

    /// Prompts the user to create a new PIN for pctl and stores it with the service.
    fn register_pin(&mut self, finished: Box<dyn FnOnce() + Send>);

    /// Prompts the user to verify the current PIN and then store a new one into pctl.
    fn change_pin(&mut self, finished: Box<dyn FnOnce() + Send>);
}

/// Default (stub) parental controls applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultParentalControlsApplet`.
pub struct DefaultParentalControlsApplet;

impl Applet for DefaultParentalControlsApplet {
    fn close(&self) {}
}

impl ParentalControlsApplet for DefaultParentalControlsApplet {
    fn verify_pin(
        &mut self,
        finished: Box<dyn FnOnce(bool) + Send>,
        suspend_future_verification_temporarily: bool,
    ) {
        log::info!(
            "Application requested frontend to verify PIN (normal), \
             suspend_future_verification_temporarily={}, verifying as correct.",
            suspend_future_verification_temporarily
        );
        finished(true);
    }

    fn verify_pin_for_settings(&mut self, finished: Box<dyn FnOnce(bool) + Send>) {
        log::info!(
            "Application requested frontend to verify PIN (settings), verifying as correct."
        );
        finished(true);
    }

    fn register_pin(&mut self, finished: Box<dyn FnOnce() + Send>) {
        log::info!("Application requested frontend to register new PIN");
        finished();
    }

    fn change_pin(&mut self, finished: Box<dyn FnOnce() + Send>) {
        log::info!("Application requested frontend to change PIN to new value");
        finished();
    }
}

// ---------------------------------------------------------------------------
// Photo Viewer Applet
// ---------------------------------------------------------------------------

/// Photo viewer applet trait.
///
/// Corresponds to upstream `Core::Frontend::PhotoViewerApplet`.
pub trait PhotoViewerApplet: Applet {
    fn show_photos_for_application(
        &self,
        title_id: u64,
        finished: Box<dyn FnOnce() + Send>,
    );

    fn show_all_photos(&self, finished: Box<dyn FnOnce() + Send>);
}

/// Default (stub) photo viewer applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultPhotoViewerApplet`.
pub struct DefaultPhotoViewerApplet;

impl Applet for DefaultPhotoViewerApplet {
    fn close(&self) {}
}

impl PhotoViewerApplet for DefaultPhotoViewerApplet {
    fn show_photos_for_application(
        &self,
        title_id: u64,
        finished: Box<dyn FnOnce() + Send>,
    ) {
        log::info!(
            "Application requested frontend to display stored photos for title_id={:016X}",
            title_id
        );
        finished();
    }

    fn show_all_photos(&self, finished: Box<dyn FnOnce() + Send>) {
        log::info!("Application requested frontend to display all stored photos.");
        finished();
    }
}
