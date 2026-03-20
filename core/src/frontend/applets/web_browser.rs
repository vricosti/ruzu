// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/web_browser.h and web_browser.cpp
//! Web browser applet interface.

use super::applet::Applet;

/// Corresponds to upstream `Service::AM::Frontend::WebExitReason`.
/// Local definition until hle::service::am::frontend types are ported.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum WebExitReason {
    EndButtonPressed = 0,
    BackCrossBtnPressed = 1,
    Exited = 2,
    Aborted = 3,
    LastUrl = 4,
    ErrorDialog = 7,
    WindowClosed = 8,
}

/// Callback for ROMFS extraction.
///
/// Corresponds to upstream `WebBrowserApplet::ExtractROMFSCallback`.
pub type ExtractRomfsCallback = Box<dyn FnOnce() + Send>;

/// Callback for web page results.
///
/// Corresponds to upstream `WebBrowserApplet::OpenWebPageCallback`.
pub type OpenWebPageCallback = Box<dyn FnOnce(WebExitReason, String) + Send>;

/// Web browser applet trait.
///
/// Corresponds to upstream `Core::Frontend::WebBrowserApplet`.
pub trait WebBrowserApplet: Applet {
    fn open_local_web_page(
        &self,
        local_url: &str,
        extract_romfs_callback: ExtractRomfsCallback,
        callback: OpenWebPageCallback,
    );

    fn open_external_web_page(
        &self,
        external_url: &str,
        callback: OpenWebPageCallback,
    );
}

/// Default (stub) web browser applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultWebBrowserApplet`.
pub struct DefaultWebBrowserApplet;

impl Applet for DefaultWebBrowserApplet {
    fn close(&self) {}
}

impl WebBrowserApplet for DefaultWebBrowserApplet {
    fn open_local_web_page(
        &self,
        local_url: &str,
        _extract_romfs_callback: ExtractRomfsCallback,
        callback: OpenWebPageCallback,
    ) {
        log::warn!(
            "(STUBBED) called, backend requested to open local web page at {}",
            local_url
        );
        callback(
            WebExitReason::WindowClosed,
            "http://localhost/".to_string(),
        );
    }

    fn open_external_web_page(
        &self,
        external_url: &str,
        callback: OpenWebPageCallback,
    ) {
        log::warn!(
            "(STUBBED) called, backend requested to open external web page at {}",
            external_url
        );
        callback(
            WebExitReason::WindowClosed,
            "http://localhost/".to_string(),
        );
    }
}
