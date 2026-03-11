// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/sockets_translate.h
//! Port of zuyu/src/core/hle/service/sockets/sockets_translate.cpp
//!
//! Translation utilities between BSD/guest socket types and host types.

use super::sockets::Errno;

/// Translate a host errno to a guest Errno.
pub fn translate_errno(host_errno: i32) -> Errno {
    match host_errno {
        0 => Errno::SUCCESS,
        9 => Errno::BADF,
        11 => Errno::AGAIN,
        22 => Errno::INVAL,
        24 => Errno::MFILE,
        104 => Errno::CONNRESET,
        107 => Errno::NOTCONN,
        110 => Errno::TIMEDOUT,
        111 => Errno::CONNREFUSED,
        115 => Errno::INPROGRESS,
        _ => {
            log::warn!("Unhandled host errno {}, returning INVAL", host_errno);
            Errno::INVAL
        }
    }
}
