// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_web_browser.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_web_browser.cpp

/// WebBrowser frontend applet stub.
pub struct WebBrowser {
    complete: bool,
}

impl WebBrowser {
    pub fn new() -> Self {
        Self { complete: false }
    }
}
