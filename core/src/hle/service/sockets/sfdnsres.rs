// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/sfdnsres.h
//! Port of zuyu/src/core/hle/service/sockets/sfdnsres.cpp
//!
//! SFDNSRES service — DNS resolution ("sfdnsres").

use super::sockets::{Domain, Errno};
use super::sockets_translate::{
    get_addr_info_error_string, get_addr_info_error_to_errno, get_addr_info_error_to_netdb_error,
    GetAddrInfoError, NetDbError, SockAddrIn,
};

/// IPC command table for SFDNSRES.
///
/// Corresponds to the function table in upstream sfdnsres.cpp.
pub mod commands {
    pub const SET_DNS_ADDRESSES_PRIVATE_REQUEST: u32 = 0;
    pub const GET_DNS_ADDRESS_PRIVATE_REQUEST: u32 = 1;
    pub const GET_HOST_BY_NAME_REQUEST: u32 = 2;
    pub const GET_HOST_BY_ADDR_REQUEST: u32 = 3;
    pub const GET_HOST_STRING_ERROR_REQUEST: u32 = 4;
    pub const GET_GAI_STRING_ERROR_REQUEST: u32 = 5;
    pub const GET_ADDR_INFO_REQUEST: u32 = 6;
    pub const GET_NAME_INFO_REQUEST: u32 = 7;
    pub const REQUEST_CANCEL_HANDLE_REQUEST: u32 = 8;
    pub const CANCEL_REQUEST: u32 = 9;
    pub const GET_HOST_BY_NAME_REQUEST_WITH_OPTIONS: u32 = 10;
    pub const GET_HOST_BY_ADDR_REQUEST_WITH_OPTIONS: u32 = 11;
    pub const GET_ADDR_INFO_REQUEST_WITH_OPTIONS: u32 = 12;
    pub const GET_NAME_INFO_REQUEST_WITH_OPTIONS: u32 = 13;
    pub const RESOLVER_SET_OPTION_REQUEST: u32 = 14;
    pub const RESOLVER_GET_OPTION_REQUEST: u32 = 15;
}

/// Output from GetHostByNameRequest.
///
/// Corresponds to `OutputParameters` in `GetHostByNameRequest` upstream.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct GetHostByNameOutput {
    pub netdb_error: i32,
    pub bsd_errno: u32,
    pub data_size: u32,
}

/// Output from GetHostByNameRequestWithOptions.
///
/// Corresponds to `OutputParameters` in `GetHostByNameRequestWithOptions` upstream.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct GetHostByNameWithOptionsOutput {
    pub data_size: u32,
    pub netdb_error: i32,
    pub bsd_errno: u32,
}

/// Output from GetAddrInfoRequest.
///
/// Corresponds to `OutputParameters` in `GetAddrInfoRequest` upstream.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct GetAddrInfoOutput {
    pub bsd_errno: u32,
    pub gai_error: i32,
    pub data_size: u32,
}

/// Output from GetAddrInfoRequestWithOptions.
///
/// Corresponds to `OutputParameters` in `GetAddrInfoRequestWithOptions` upstream.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct GetAddrInfoWithOptionsOutput {
    pub data_size: u32,
    pub gai_error: i32,
    pub netdb_error: i32,
    pub bsd_errno: u32,
}

/// SFDNSRES service.
///
/// Corresponds to `SFDNSRES` in upstream sfdnsres.h / sfdnsres.cpp.
pub struct Sfdnsres;

impl Sfdnsres {
    pub fn new() -> Self {
        Self
    }

    /// Append a value as raw bytes to a buffer.
    fn append_raw<T: Copy>(buf: &mut Vec<u8>, value: T) {
        let ptr = &value as *const T as *const u8;
        let bytes = unsafe { std::slice::from_raw_parts(ptr, std::mem::size_of::<T>()) };
        buf.extend_from_slice(bytes);
    }

    /// Append a NUL-terminated string to a buffer.
    fn append_nul_terminated(buf: &mut Vec<u8>, s: &str) {
        buf.extend_from_slice(s.as_bytes());
        buf.push(0);
    }

    /// Serialize addrinfo results as a hostent-style buffer.
    ///
    /// Corresponds to `SerializeAddrInfoAsHostEnt` in upstream sfdnsres.cpp.
    fn serialize_addr_info_as_host_ent(addrs: &[[u8; 4]], host: &str) -> Vec<u8> {
        let mut data = Vec::new();

        // h_name: input hostname (NUL-terminated).
        Self::append_nul_terminated(&mut data, host);

        // h_aliases: empty.
        Self::append_raw(&mut data, 0u32.to_be()); // count of aliases = 0

        // h_addrtype
        Self::append_raw(&mut data, (Domain::INET as u16).to_be());
        // h_length
        Self::append_raw(&mut data, 4u16.to_be()); // sizeof(IPv4Address)

        // h_addr_list count.
        Self::append_raw(&mut data, (addrs.len() as u32).to_be());

        for addr in addrs {
            // On the Switch, this is passed through htonl despite already being
            // big-endian, so it ends up as little-endian.
            let ip_int = u32::from_be_bytes(*addr);
            Self::append_raw(&mut data, ip_int.to_le());

            log::info!(
                "Resolved host '{}' to IPv4 address {}.{}.{}.{}",
                host,
                addr[0],
                addr[1],
                addr[2],
                addr[3]
            );
        }

        data
    }

    /// Common implementation for GetHostByNameRequest / GetHostByNameRequestWithOptions.
    ///
    /// Corresponds to `GetHostByNameRequestImpl` in upstream sfdnsres.cpp.
    pub fn get_host_by_name_impl(
        host: &str,
    ) -> (u32, GetAddrInfoError) {
        // Prevent resolution of Nintendo servers.
        if host.contains("srv.nintendo.net") {
            log::warn!(
                "Resolution of hostname {} requested, returning EAI_AGAIN",
                host
            );
            return (0, GetAddrInfoError::AGAIN);
        }

        // Attempt DNS resolution via std::net.
        use std::net::ToSocketAddrs;
        let lookup = format!("{}:0", host);
        match lookup.to_socket_addrs() {
            Ok(addrs) => {
                let ipv4_addrs: Vec<[u8; 4]> = addrs
                    .filter_map(|a| match a {
                        std::net::SocketAddr::V4(v4) => Some(v4.ip().octets()),
                        _ => None,
                    })
                    .collect();

                if ipv4_addrs.is_empty() {
                    return (0, GetAddrInfoError::NODATA);
                }

                let data = Self::serialize_addr_info_as_host_ent(&ipv4_addrs, host);
                let data_size = data.len() as u32;
                // TODO: Write data to output buffer via IPC context.
                (data_size, GetAddrInfoError::SUCCESS)
            }
            Err(e) => {
                log::warn!("DNS resolution failed for '{}': {}", host, e);
                (0, GetAddrInfoError::NONAME)
            }
        }
    }

    /// Common implementation for GetAddrInfoRequest / GetAddrInfoRequestWithOptions.
    ///
    /// Corresponds to `GetAddrInfoRequestImpl` in upstream sfdnsres.cpp.
    pub fn get_addr_info_impl(
        host: &str,
        _service: Option<&str>,
    ) -> (u32, GetAddrInfoError) {
        // Prevent resolution of Nintendo servers.
        if host.contains("srv.nintendo.net") {
            log::warn!(
                "Resolution of hostname {} requested, returning EAI_AGAIN",
                host
            );
            return (0, GetAddrInfoError::AGAIN);
        }

        // Attempt DNS resolution.
        use std::net::ToSocketAddrs;
        let lookup = format!("{}:0", host);
        match lookup.to_socket_addrs() {
            Ok(addrs) => {
                let ipv4_addrs: Vec<std::net::SocketAddrV4> = addrs
                    .filter_map(|a| match a {
                        std::net::SocketAddr::V4(v4) => Some(v4),
                        _ => None,
                    })
                    .collect();

                if ipv4_addrs.is_empty() {
                    return (0, GetAddrInfoError::NODATA);
                }

                let data = Self::serialize_addr_info(&ipv4_addrs, host);
                let data_size = data.len() as u32;
                // TODO: Write data to output buffer via IPC context.
                (data_size, GetAddrInfoError::SUCCESS)
            }
            Err(e) => {
                log::warn!("DNS resolution failed for '{}': {}", host, e);
                (0, GetAddrInfoError::NONAME)
            }
        }
    }

    /// Serialize addrinfo results in the Switch addrinfo format.
    ///
    /// Corresponds to `SerializeAddrInfo` in upstream sfdnsres.cpp.
    fn serialize_addr_info(addrs: &[std::net::SocketAddrV4], host: &str) -> Vec<u8> {
        let mut data = Vec::new();

        for addr in addrs {
            let ip = addr.ip().octets();
            let port = addr.port();

            // Magic.
            Self::append_raw(&mut data, 0xBEEFCAFEu32.to_be());
            // ai_flags = 0.
            Self::append_raw(&mut data, 0u32.to_be());
            // ai_family = INET.
            Self::append_raw(&mut data, (Domain::INET as u32).to_be());
            // ai_socktype = STREAM (default).
            Self::append_raw(&mut data, 1u32.to_be());
            // ai_protocol = TCP (default).
            Self::append_raw(&mut data, 6u32.to_be());
            // ai_addrlen = sizeof(SockAddrIn).
            Self::append_raw(
                &mut data,
                (std::mem::size_of::<SockAddrIn>() as u32).to_be(),
            );

            // ai_addr: SockAddrIn fields.
            // sin_family.
            Self::append_raw(&mut data, (Domain::INET as u16).to_be());
            // sin_port (LE due to Switch double-swap).
            Self::append_raw(&mut data, port.to_le());
            // sin_addr (LE due to Switch double-swap).
            let ip_int = u32::from_be_bytes(ip);
            Self::append_raw(&mut data, ip_int.to_le());
            // sin_zero (8 bytes).
            data.extend_from_slice(&[0u8; 8]);

            // Canon name (empty NUL-terminated string).
            data.push(0);

            log::info!(
                "Resolved host '{}' to IPv4 address {}.{}.{}.{}",
                host,
                ip[0],
                ip[1],
                ip[2],
                ip[3]
            );
        }

        // 4-byte sentinel value.
        data.extend_from_slice(&[0u8; 4]);

        data
    }

    /// GetGaiStringErrorRequest (cmd 5).
    ///
    /// Corresponds to `SFDNSRES::GetGaiStringErrorRequest` in upstream.
    pub fn get_gai_string_error(gai_errno: i32) -> &'static str {
        let error = match gai_errno {
            0 => GetAddrInfoError::SUCCESS,
            1 => GetAddrInfoError::ADDRFAMILY,
            2 => GetAddrInfoError::AGAIN,
            3 => GetAddrInfoError::BADFLAGS,
            4 => GetAddrInfoError::FAIL,
            5 => GetAddrInfoError::FAMILY,
            6 => GetAddrInfoError::MEMORY,
            7 => GetAddrInfoError::NODATA,
            8 => GetAddrInfoError::NONAME,
            9 => GetAddrInfoError::SERVICE,
            10 => GetAddrInfoError::SOCKTYPE,
            11 => GetAddrInfoError::SYSTEM,
            12 => GetAddrInfoError::BADHINTS,
            13 => GetAddrInfoError::PROTOCOL,
            14 => GetAddrInfoError::OVERFLOW,
            _ => GetAddrInfoError::OTHER,
        };
        get_addr_info_error_string(error)
    }

    /// ResolverSetOptionRequest (cmd 14).
    ///
    /// Corresponds to `SFDNSRES::ResolverSetOptionRequest` in upstream.
    pub fn resolver_set_option_request(&self) -> i32 {
        log::warn!("ResolverSetOptionRequest (STUBBED) called");
        0 // bsd errno = success
    }
}
