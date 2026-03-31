// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/sockets_translate.h
//! Port of zuyu/src/core/hle/service/sockets/sockets_translate.cpp
//!
//! Translation utilities between BSD/guest socket types and internal network types.

use super::sockets::{Domain, Errno, Protocol, Type};

/// GetAddrInfoError codes matching upstream.
///
/// Corresponds to `GetAddrInfoError` in upstream sockets.h.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum GetAddrInfoError {
    SUCCESS = 0,
    ADDRFAMILY = 1,
    AGAIN = 2,
    BADFLAGS = 3,
    FAIL = 4,
    FAMILY = 5,
    MEMORY = 6,
    NODATA = 7,
    NONAME = 8,
    SERVICE = 9,
    SOCKTYPE = 10,
    SYSTEM = 11,
    BADHINTS = 12,
    PROTOCOL = 13,
    OVERFLOW = 14,
    OTHER = 15,
}

/// NetDbError codes.
///
/// Corresponds to `NetDbError` in upstream sfdnsres.cpp.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum NetDbError {
    Internal = -1,
    Success = 0,
    HostNotFound = 1,
    TryAgain = 2,
    NoRecovery = 3,
    NoData = 4,
}

/// PollEvents flags.
///
/// Corresponds to `PollEvents` in upstream sockets.h.
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct PollEvents: u16 {
        const IN = 0x001;
        const PRI = 0x002;
        const OUT = 0x004;
        const ERR = 0x008;
        const HUP = 0x010;
        const NVAL = 0x020;
        const RD_NORM = 0x040;
        const RD_BAND = 0x080;
        const WR_BAND = 0x100;
    }
}

/// ShutdownHow modes.
///
/// Corresponds to `ShutdownHow` in upstream sockets.h.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ShutdownHow {
    RD = 0,
    WR = 1,
    RDWR = 2,
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

/// Translate Errno values from an internal representation.
///
/// Corresponds to `Translate(Network::Errno)` in upstream sockets_translate.cpp.
pub fn translate_errno_from_network(value: i32) -> Errno {
    match value {
        0 => Errno::SUCCESS,
        9 => Errno::BADF,
        11 => Errno::AGAIN,
        22 => Errno::INVAL,
        24 => Errno::MFILE,
        32 => Errno::PIPE,
        103 => Errno::CONNABORTED,
        104 => Errno::CONNRESET,
        107 => Errno::NOTCONN,
        110 => Errno::TIMEDOUT,
        111 => Errno::CONNREFUSED,
        115 => Errno::INPROGRESS,
        _ => {
            log::warn!("Unimplemented errno translation for value={}", value);
            Errno::SUCCESS
        }
    }
}

/// Translate GetAddrInfoError to NetDbError.
///
/// Corresponds to `GetAddrInfoErrorToNetDbError` in upstream sfdnsres.cpp.
pub fn get_addr_info_error_to_netdb_error(result: GetAddrInfoError) -> NetDbError {
    match result {
        GetAddrInfoError::SUCCESS => NetDbError::Success,
        GetAddrInfoError::AGAIN => NetDbError::TryAgain,
        GetAddrInfoError::NODATA => NetDbError::HostNotFound,
        GetAddrInfoError::SERVICE => NetDbError::Success,
        _ => NetDbError::HostNotFound,
    }
}

/// Translate GetAddrInfoError to Errno.
///
/// Corresponds to `GetAddrInfoErrorToErrno` in upstream sfdnsres.cpp.
pub fn get_addr_info_error_to_errno(result: GetAddrInfoError) -> Errno {
    match result {
        GetAddrInfoError::SUCCESS => Errno::SUCCESS,
        GetAddrInfoError::AGAIN => Errno::SUCCESS,
        GetAddrInfoError::NODATA => Errno::SUCCESS,
        GetAddrInfoError::SERVICE => Errno::INVAL,
        _ => Errno::SUCCESS,
    }
}

/// Translate GetAddrInfoError to human-readable string.
///
/// Corresponds to `Translate(GetAddrInfoError)` returning `const char*` in upstream.
pub fn get_addr_info_error_string(error: GetAddrInfoError) -> &'static str {
    match error {
        GetAddrInfoError::SUCCESS => "Success",
        GetAddrInfoError::ADDRFAMILY => "Address family for hostname not supported",
        GetAddrInfoError::AGAIN => "Temporary failure in name resolution",
        GetAddrInfoError::BADFLAGS => "Invalid value for ai_flags",
        GetAddrInfoError::FAIL => "Non-recoverable failure in name resolution",
        GetAddrInfoError::FAMILY => "ai_family not supported",
        GetAddrInfoError::MEMORY => "Memory allocation failure",
        GetAddrInfoError::NODATA => "No address associated with hostname",
        GetAddrInfoError::NONAME => "hostname nor servname provided, or not known",
        GetAddrInfoError::SERVICE => "servname not supported for ai_socktype",
        GetAddrInfoError::SOCKTYPE => "ai_socktype not supported",
        GetAddrInfoError::SYSTEM => "System error returned in errno",
        GetAddrInfoError::BADHINTS => "Invalid value for hints",
        GetAddrInfoError::PROTOCOL => "Resolved protocol is unknown",
        GetAddrInfoError::OVERFLOW => "Argument buffer overflow",
        _ => "Unknown error",
    }
}

/// Translate guest Domain to internal domain.
pub fn translate_domain_to_network(domain: Domain) -> u32 {
    match domain {
        Domain::Unspecified => 0,
        Domain::INET => 2,
    }
}

/// Translate internal domain to guest Domain.
pub fn translate_domain_from_network(domain: u32) -> Domain {
    match domain {
        0 => Domain::Unspecified,
        2 => Domain::INET,
        _ => {
            log::warn!("Unimplemented domain={}", domain);
            Domain::Unspecified
        }
    }
}

/// Translate guest Type to internal type.
pub fn translate_type_to_network(ty: Type) -> u32 {
    match ty {
        Type::Unspecified => 0,
        Type::STREAM => 1,
        Type::DGRAM => 2,
        Type::RAW => 3,
        Type::SEQPACKET => 5,
    }
}

/// Translate internal type to guest Type.
pub fn translate_type_from_network(ty: u32) -> Type {
    match ty {
        1 => Type::STREAM,
        2 => Type::DGRAM,
        3 => Type::RAW,
        5 => Type::SEQPACKET,
        _ => {
            log::warn!("Unimplemented type={}", ty);
            Type::STREAM
        }
    }
}

/// Translate guest Protocol to internal protocol.
pub fn translate_protocol_to_network(protocol: Protocol) -> u32 {
    match protocol {
        Protocol::Unspecified => 0,
        Protocol::ICMP => 1,
        Protocol::TCP => 6,
        Protocol::UDP => 17,
    }
}

/// Translate internal protocol to guest Protocol.
pub fn translate_protocol_from_network(protocol: u32) -> Protocol {
    match protocol {
        0 => Protocol::Unspecified,
        1 => Protocol::ICMP,
        6 => Protocol::TCP,
        17 => Protocol::UDP,
        _ => {
            log::warn!("Unimplemented protocol={}", protocol);
            Protocol::Unspecified
        }
    }
}

/// Translate guest SockAddrIn to internal representation.
///
/// Corresponds to `Translate(SockAddrIn)` in upstream sockets_translate.cpp.
/// Note: portno byte-swap matches upstream (big-endian to host).
pub fn translate_sockaddr_to_network(value: &SockAddrIn) -> (u32, [u8; 4], u16) {
    // Upstream: assert len == 0 || len == sizeof(SockAddrIn) || len == 6
    let family = translate_domain_to_network(match value.family {
        0 => Domain::Unspecified,
        2 => Domain::INET,
        _ => Domain::Unspecified,
    });
    let port = (value.portno >> 8) | (value.portno << 8);
    (family, value.ip, port)
}

/// Translate internal representation to guest SockAddrIn.
///
/// Corresponds to `Translate(Network::SockAddrIn)` in upstream.
pub fn translate_sockaddr_from_network(family: u32, ip: [u8; 4], portno: u16) -> SockAddrIn {
    let domain = translate_domain_from_network(family);
    SockAddrIn {
        len: std::mem::size_of::<SockAddrIn>() as u8,
        family: domain as u8,
        portno: (portno >> 8) | (portno << 8),
        ip,
        zeroes: [0u8; 8],
    }
}
