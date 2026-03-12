// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/sockets.h
//! Port of zuyu/src/core/hle/service/sockets/sockets.cpp
//!
//! Socket service registration and common types.

/// Errno values matching upstream.
///
/// Corresponds to `Errno` in upstream sockets.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Errno {
    SUCCESS = 0,
    BADF = 9,
    AGAIN = 11,
    INVAL = 22,
    MFILE = 24,
    PIPE = 32,
    CONNABORTED = 103,
    CONNRESET = 104,
    NOTCONN = 107,
    TIMEDOUT = 110,
    CONNREFUSED = 111,
    INPROGRESS = 115,
}

/// Domain (address family).
///
/// Corresponds to `Domain` in upstream sockets.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Domain {
    Unspecified = 0,
    INET = 2,
}

/// Type (socket type).
///
/// Corresponds to `Type` in upstream sockets.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Unspecified = 0,
    STREAM = 1,
    DGRAM = 2,
    RAW = 3,
    SEQPACKET = 5,
}

/// Protocol.
///
/// Corresponds to `Protocol` in upstream sockets.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Protocol {
    Unspecified = 0,
    ICMP = 1,
    TCP = 6,
    UDP = 17,
}

/// LoopProcess — registers "bsd:u", "bsd:s", "bsd:a", "nsd:u", "nsd:a", "sfdnsres" services.
///
/// Corresponds to `Service::Sockets::LoopProcess` in upstream sockets.cpp.
pub fn loop_process() {
    log::debug!("Sockets::LoopProcess called");
    // TODO: Register socket services with ServerManager
}
