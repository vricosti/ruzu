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
    MSGSIZE = 90,
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

/// Socket level for setsockopt/getsockopt.
///
/// Corresponds to `SocketLevel` in upstream sockets.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketLevel {
    SOCKET = 0xffff, // SOL_SOCKET
}

/// Socket option names.
///
/// Corresponds to `OptName` in upstream sockets.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptName {
    REUSEADDR = 0x4,
    KEEPALIVE = 0x8,
    BROADCAST = 0x20,
    LINGER = 0x80,
    SNDBUF = 0x1001,
    RCVBUF = 0x1002,
    SNDTIMEO = 0x1005,
    RCVTIMEO = 0x1006,
    ERROR = 0x1007,
    NOSIGPIPE = 0x800, // at least according to libnx
}

/// ShutdownHow modes.
///
/// Corresponds to `ShutdownHow` in upstream sockets.h.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShutdownHow {
    RD = 0,
    WR = 1,
    RDWR = 2,
}

/// Fcntl command codes.
///
/// Corresponds to `FcntlCmd` in upstream sockets.h.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FcntlCmd {
    GETFL = 3,
    SETFL = 4,
}

/// Guest socket address structure.
///
/// Corresponds to `SockAddrIn` in upstream sockets.h.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SockAddrIn {
    pub len: u8,
    pub family: u8,
    pub portno: u16,
    pub ip: [u8; 4],
    pub zeroes: [u8; 8],
}

/// PollEvents flags.
///
/// Corresponds to `PollEvents` in upstream sockets.h.
/// Uses DECLARE_ENUM_FLAG_OPERATORS in C++.
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct PollEvents: u16 {
        const IN = 1 << 0;
        const PRI = 1 << 1;
        const OUT = 1 << 2;
        const ERR = 1 << 3;
        const HUP = 1 << 4;
        const NVAL = 1 << 5;
        const RD_NORM = 1 << 6;
        const RD_BAND = 1 << 7;
        const WR_BAND = 1 << 8;
    }
}

/// PollFD structure for poll operations.
///
/// Corresponds to `PollFD` in upstream sockets.h.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PollFD {
    pub fd: i32,
    pub events: u16,  // PollEvents
    pub revents: u16, // PollEvents
}

/// Linger structure for SO_LINGER option.
///
/// Corresponds to `Linger` in upstream sockets.h.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Linger {
    pub onoff: u32,
    pub linger: u32,
}

/// LoopProcess -- registers "bsd:u", "bsd:s", "bsdcfg", "nsd:u", "nsd:a", "sfdnsres" services.
///
/// Corresponds to `Service::Sockets::LoopProcess` in upstream sockets.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let mut server_manager = ServerManager::new(system);

    let stub_names = &["bsd:u", "bsd:s", "bsdcfg", "nsd:u", "nsd:a", "sfdnsres"];
    for &name in stub_names {
        let svc_name = name.to_string();
        server_manager.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(
                    &svc_name,
                ))
            }),
            16,
        );
    }

    ServerManager::run_server(server_manager);
}
