// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/bsd.h
//! Port of zuyu/src/core/hle/service/sockets/bsd.cpp
//!
//! BSD socket service — "bsd:u" and "bsd:s".

/// IPC command table for BSD.
pub mod commands {
    pub const REGISTER_CLIENT: u32 = 0;
    pub const START_MONITORING: u32 = 1;
    pub const SOCKET: u32 = 2;
    pub const SOCKET_EXEMPT: u32 = 3;
    pub const OPEN: u32 = 4;
    pub const SELECT: u32 = 5;
    pub const POLL: u32 = 6;
    pub const SYSCTL: u32 = 7;
    pub const RECV: u32 = 8;
    pub const RECV_FROM: u32 = 9;
    pub const SEND: u32 = 10;
    pub const SEND_TO: u32 = 11;
    pub const ACCEPT: u32 = 12;
    pub const BIND: u32 = 13;
    pub const CONNECT: u32 = 14;
    pub const GET_PEER_NAME: u32 = 15;
    pub const GET_SOCK_NAME: u32 = 16;
    pub const GET_SOCK_OPT: u32 = 17;
    pub const LISTEN: u32 = 18;
    pub const IOCTL: u32 = 19;
    pub const FCNTL: u32 = 20;
    pub const SET_SOCK_OPT: u32 = 21;
    pub const SHUTDOWN: u32 = 22;
    pub const SHUTDOWN_ALL_SOCKETS: u32 = 23;
    pub const WRITE: u32 = 24;
    pub const READ: u32 = 25;
    pub const CLOSE: u32 = 26;
    pub const DUPLICATE_SOCKET: u32 = 27;
    pub const GET_RESOURCE_STATISTICS: u32 = 28;
    pub const RECV_MMSG: u32 = 29;
    pub const SEND_MMSG: u32 = 30;
    pub const EVENT_FD: u32 = 31;
}

/// BSD socket service.
///
/// Corresponds to `BSD` in upstream bsd.h / bsd.cpp.
pub struct Bsd {
    is_privileged: bool,
}

impl Bsd {
    pub fn new(is_privileged: bool) -> Self {
        Self { is_privileged }
    }
}
