// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `core/frontend/applets/web_browser.{h,cpp}`.
//! Web browser applet interface.

use super::applet::Applet;
pub use crate::hle::service::am::frontend::applet_web_browser_types::WebExitReason;

/// Callback for ROMFS extraction.
///
/// Corresponds to upstream `WebBrowserApplet::ExtractROMFSCallback`.
pub type ExtractRomfsCallback = Box<dyn Fn() + Send + Sync>;

/// Callback for web page results.
///
/// Corresponds to upstream `WebBrowserApplet::OpenWebPageCallback`.
pub type OpenWebPageCallback = Box<dyn Fn(WebExitReason, String) + Send + Sync>;

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

    fn open_external_web_page(&self, external_url: &str, callback: OpenWebPageCallback);
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
            WebExitReason::WINDOW_CLOSED,
            "http://localhost/".to_string(),
        );
    }

    fn open_external_web_page(&self, external_url: &str, callback: OpenWebPageCallback) {
        log::warn!(
            "(STUBBED) called, backend requested to open external web page at {}",
            external_url
        );
        callback(
            WebExitReason::WINDOW_CLOSED,
            "http://localhost/".to_string(),
        );
    }
}
