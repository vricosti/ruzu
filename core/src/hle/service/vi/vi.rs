// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/vi.cpp/.h
//!
//! LoopProcess registers the following named services:
//!   vi:u   -> IApplicationRootService
//!   vi:s   -> ISystemRootService
//!   vi:m   -> IManagerRootService

pub const VI_SERVICE_NAMES: &[&str] = &["vi:u", "vi:s", "vi:m"];
