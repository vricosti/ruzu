// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/internal_network/sockets.h and sockets.cpp (partial)
//! Socket abstraction layer.

use crate::internal_network::network::{
    Domain, Errno, Protocol, ProxyPacket, ShutdownHow, SockAddrIn, Type,
};

/// Socket base trait.
///
/// Corresponds to upstream `Network::SocketBase`.
pub trait SocketBase {
    fn initialize(&mut self, domain: Domain, type_: Type, protocol: Protocol) -> Errno;
    fn close(&mut self) -> Errno;

    fn accept(&mut self) -> (AcceptResult, Errno);
    fn connect(&mut self, addr_in: SockAddrIn) -> Errno;

    fn get_peer_name(&self) -> (SockAddrIn, Errno);
    fn get_sock_name(&self) -> (SockAddrIn, Errno);

    fn bind(&mut self, addr: SockAddrIn) -> Errno;
    fn listen(&mut self, backlog: i32) -> Errno;
    fn shutdown(&mut self, how: ShutdownHow) -> Errno;

    fn recv(&mut self, flags: i32, message: &mut [u8]) -> (i32, Errno);
    fn recv_from(&mut self, flags: i32, message: &mut [u8], addr: Option<&mut SockAddrIn>)
        -> (i32, Errno);
    fn send(&mut self, message: &[u8], flags: i32) -> (i32, Errno);
    fn send_to(&mut self, flags: u32, message: &[u8], addr: Option<&SockAddrIn>) -> (i32, Errno);

    fn set_linger(&mut self, enable: bool, linger: u32) -> Errno;
    fn set_reuse_addr(&mut self, enable: bool) -> Errno;
    fn set_keep_alive(&mut self, enable: bool) -> Errno;
    fn set_broadcast(&mut self, enable: bool) -> Errno;
    fn set_snd_buf(&mut self, value: u32) -> Errno;
    fn set_rcv_buf(&mut self, value: u32) -> Errno;
    fn set_snd_timeo(&mut self, value: u32) -> Errno;
    fn set_rcv_timeo(&mut self, value: u32) -> Errno;
    fn set_non_block(&mut self, enable: bool) -> Errno;

    fn get_pending_error(&self) -> (Errno, Errno);
    fn is_opened(&self) -> bool;
    fn handle_proxy_packet(&mut self, packet: &ProxyPacket);

    fn get_fd(&self) -> i32;
}

/// Accept result.
///
/// Corresponds to upstream `SocketBase::AcceptResult`.
pub struct AcceptResult {
    pub socket: Option<Box<dyn SocketBase>>,
    pub sockaddr_in: SockAddrIn,
}

impl Default for AcceptResult {
    fn default() -> Self {
        Self {
            socket: None,
            sockaddr_in: SockAddrIn::default(),
        }
    }
}

/// Native socket implementation.
///
/// Corresponds to upstream `Network::Socket`.
pub struct Socket {
    fd: i32,
    is_non_blocking: bool,
}

const INVALID_SOCKET: i32 = -1;

impl Socket {
    pub fn new() -> Self {
        Self {
            fd: INVALID_SOCKET,
            is_non_blocking: false,
        }
    }

    pub fn from_fd(fd: i32) -> Self {
        Self {
            fd,
            is_non_blocking: false,
        }
    }
}

impl Drop for Socket {
    fn drop(&mut self) {
        if self.fd == INVALID_SOCKET {
            return;
        }
        #[cfg(unix)]
        unsafe {
            libc::close(self.fd);
        }
        self.fd = INVALID_SOCKET;
    }
}

impl SocketBase for Socket {
    fn initialize(&mut self, domain: Domain, type_: Type, protocol: Protocol) -> Errno {
        #[cfg(unix)]
        {
            let native_domain = match domain {
                Domain::Unspecified => 0,
                Domain::INET => libc::AF_INET,
            };
            let native_type = match type_ {
                Type::Unspecified => 0,
                Type::STREAM => libc::SOCK_STREAM,
                Type::DGRAM => libc::SOCK_DGRAM,
                Type::RAW => libc::SOCK_RAW,
                Type::SEQPACKET => libc::SOCK_SEQPACKET,
            };
            let native_proto = match protocol {
                Protocol::Unspecified => 0,
                Protocol::TCP => libc::IPPROTO_TCP,
                Protocol::UDP => libc::IPPROTO_UDP,
            };
            self.fd = unsafe { libc::socket(native_domain, native_type, native_proto) };
            if self.fd != INVALID_SOCKET {
                return Errno::Success;
            }
            return Errno::Other;
        }
        #[cfg(not(unix))]
        {
            // TODO: Windows socket creation
            Errno::Other
        }
    }

    fn close(&mut self) -> Errno {
        if self.fd != INVALID_SOCKET {
            #[cfg(unix)]
            unsafe {
                libc::close(self.fd);
            }
            self.fd = INVALID_SOCKET;
        }
        Errno::Success
    }

    fn accept(&mut self) -> (AcceptResult, Errno) {
        // TODO: Implement accept with interrupt support
        (AcceptResult::default(), Errno::Other)
    }

    fn connect(&mut self, _addr_in: SockAddrIn) -> Errno {
        // TODO: Implement connect
        Errno::Other
    }

    fn get_peer_name(&self) -> (SockAddrIn, Errno) {
        // TODO: Implement getpeername
        (SockAddrIn::default(), Errno::Other)
    }

    fn get_sock_name(&self) -> (SockAddrIn, Errno) {
        // TODO: Implement getsockname
        (SockAddrIn::default(), Errno::Other)
    }

    fn bind(&mut self, _addr: SockAddrIn) -> Errno {
        // TODO: Implement bind
        Errno::Other
    }

    fn listen(&mut self, _backlog: i32) -> Errno {
        // TODO: Implement listen
        Errno::Other
    }

    fn shutdown(&mut self, _how: ShutdownHow) -> Errno {
        // TODO: Implement shutdown
        Errno::Other
    }

    fn recv(&mut self, _flags: i32, _message: &mut [u8]) -> (i32, Errno) {
        // TODO: Implement recv
        (-1, Errno::Other)
    }

    fn recv_from(
        &mut self,
        _flags: i32,
        _message: &mut [u8],
        _addr: Option<&mut SockAddrIn>,
    ) -> (i32, Errno) {
        // TODO: Implement recvfrom
        (-1, Errno::Other)
    }

    fn send(&mut self, _message: &[u8], _flags: i32) -> (i32, Errno) {
        // TODO: Implement send
        (-1, Errno::Other)
    }

    fn send_to(
        &mut self,
        _flags: u32,
        _message: &[u8],
        _addr: Option<&SockAddrIn>,
    ) -> (i32, Errno) {
        // TODO: Implement sendto
        (-1, Errno::Other)
    }

    fn set_linger(&mut self, _enable: bool, _linger: u32) -> Errno {
        Errno::Other
    }
    fn set_reuse_addr(&mut self, _enable: bool) -> Errno {
        Errno::Other
    }
    fn set_keep_alive(&mut self, _enable: bool) -> Errno {
        Errno::Other
    }
    fn set_broadcast(&mut self, _enable: bool) -> Errno {
        Errno::Other
    }
    fn set_snd_buf(&mut self, _value: u32) -> Errno {
        Errno::Other
    }
    fn set_rcv_buf(&mut self, _value: u32) -> Errno {
        Errno::Other
    }
    fn set_snd_timeo(&mut self, _value: u32) -> Errno {
        Errno::Other
    }
    fn set_rcv_timeo(&mut self, _value: u32) -> Errno {
        Errno::Other
    }

    fn set_non_block(&mut self, enable: bool) -> Errno {
        #[cfg(unix)]
        {
            let flags = unsafe { libc::fcntl(self.fd, libc::F_GETFL) };
            if flags == -1 {
                return Errno::Other;
            }
            let new_flags = if enable {
                flags | libc::O_NONBLOCK
            } else {
                flags & !libc::O_NONBLOCK
            };
            if unsafe { libc::fcntl(self.fd, libc::F_SETFL, new_flags) } == 0 {
                self.is_non_blocking = enable;
                return Errno::Success;
            }
            return Errno::Other;
        }
        #[cfg(not(unix))]
        Errno::Other
    }

    fn get_pending_error(&self) -> (Errno, Errno) {
        (Errno::Success, Errno::Success)
    }

    fn is_opened(&self) -> bool {
        self.fd != INVALID_SOCKET
    }

    fn handle_proxy_packet(&mut self, _packet: &ProxyPacket) {
        log::warn!("ProxyPacket received, but not in Proxy mode!");
    }

    fn get_fd(&self) -> i32 {
        self.fd
    }
}

/// Poll a set of file descriptors.
///
/// Corresponds to upstream `Network::Poll`.
pub fn poll(_pollfds: &mut [PollFD], _timeout: i32) -> (i32, Errno) {
    // TODO: Implement using platform poll/WSAPoll
    (0, Errno::Success)
}

/// Poll file descriptor entry (simplified).
pub struct PollFD {
    pub fd: i32,
    pub events: u16,
    pub revents: u16,
}
