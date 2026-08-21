// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/
//! Upstream files:
//!   - sockets.h / sockets.cpp
//!   - bsd.h / bsd.cpp
//!   - nsd.h / nsd.cpp
//!   - sfdnsres.h / sfdnsres.cpp
//!   - sockets_translate.h / sockets_translate.cpp

pub mod bsd;
pub mod nsd;
pub mod sfdnsres;
pub mod sockets;
pub mod sockets_translate;
