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
pub trait SocketBase: Send + Sync {
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
    fn recv_from(
        &mut self,
        flags: i32,
        message: &mut [u8],
        addr: Option<&mut SockAddrIn>,
    ) -> (i32, Errno);
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

/// Convert our SockAddrIn to libc::sockaddr_in.
#[cfg(unix)]
fn to_sockaddr_in(addr: &SockAddrIn) -> libc::sockaddr_in {
    let ip = addr.ip;
    #[cfg(target_os = "linux")]
    let sa = libc::sockaddr_in {
        sin_family: libc::AF_INET as libc::sa_family_t,
        sin_port: addr.portno.to_be(),
        sin_addr: libc::in_addr {
            s_addr: u32::from_ne_bytes(ip).to_be(),
        },
        sin_zero: [0; 8],
    };
    #[cfg(target_os = "macos")]
    let sa = libc::sockaddr_in {
        sin_len: std::mem::size_of::<libc::sockaddr_in>() as u8,
        sin_family: libc::AF_INET as libc::sa_family_t,
        sin_port: addr.portno.to_be(),
        sin_addr: libc::in_addr {
            s_addr: u32::from_ne_bytes(ip).to_be(),
        },
        sin_zero: [0; 8],
    };
    sa
}

/// Convert libc::sockaddr_in to our SockAddrIn.
#[cfg(unix)]
fn from_sockaddr_in(addr: &libc::sockaddr_in) -> SockAddrIn {
    let ip_bytes = u32::from_be(addr.sin_addr.s_addr).to_ne_bytes();
    SockAddrIn {
        family: Some(Domain::INET),
        ip: ip_bytes,
        portno: u16::from_be(addr.sin_port),
    }
}

/// Get the last socket error as an Errno.
#[cfg(unix)]
fn get_last_error() -> Errno {
    #[cfg(target_os = "linux")]
    let err = unsafe { *libc::__errno_location() };
    #[cfg(target_os = "macos")]
    let err = unsafe { *libc::__error() };
    match err {
        libc::EWOULDBLOCK | libc::EAGAIN => Errno::Again,
        libc::ECONNREFUSED => Errno::Connrefused,
        libc::ECONNRESET => Errno::Connreset,
        libc::ECONNABORTED => Errno::Connaborted,
        libc::EINPROGRESS => Errno::Inprogress,
        libc::ENOTCONN => Errno::Notconn,
        libc::ETIMEDOUT => Errno::Timedout,
        libc::EBADF => Errno::Badf,
        libc::EINVAL => Errno::Inval,
        libc::EPIPE => Errno::Pipe,
        libc::EMSGSIZE => Errno::Msgsize,
        libc::EHOSTUNREACH => Errno::Hostunreach,
        libc::ENETDOWN => Errno::Netdown,
        libc::ENETUNREACH => Errno::Netunreach,
        _ => {
            log::warn!("Unmapped socket errno: {}", err);
            Errno::Other
        }
    }
}

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
                Protocol::ICMP => libc::IPPROTO_ICMP,
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
        #[cfg(unix)]
        {
            let mut addr: libc::sockaddr_in = unsafe { std::mem::zeroed() };
            let mut addrlen: libc::socklen_t = std::mem::size_of::<libc::sockaddr_in>() as u32;
            let new_fd = unsafe {
                libc::accept(
                    self.fd,
                    &mut addr as *mut libc::sockaddr_in as *mut libc::sockaddr,
                    &mut addrlen,
                )
            };
            if new_fd < 0 {
                return (AcceptResult::default(), get_last_error());
            }
            let mut new_socket = Socket::new();
            new_socket.fd = new_fd;
            (
                AcceptResult {
                    socket: Some(Box::new(new_socket)),
                    sockaddr_in: from_sockaddr_in(&addr),
                },
                Errno::Success,
            )
        }
        #[cfg(not(unix))]
        {
            (AcceptResult::default(), Errno::Other)
        }
    }

    fn connect(&mut self, addr_in: SockAddrIn) -> Errno {
        #[cfg(unix)]
        {
            let addr = to_sockaddr_in(&addr_in);
            let result = unsafe {
                libc::connect(
                    self.fd,
                    &addr as *const libc::sockaddr_in as *const libc::sockaddr,
                    std::mem::size_of::<libc::sockaddr_in>() as u32,
                )
            };
            if result == 0 {
                Errno::Success
            } else {
                get_last_error()
            }
        }
        #[cfg(not(unix))]
        {
            let _ = addr_in;
            Errno::Other
        }
    }

    fn get_peer_name(&self) -> (SockAddrIn, Errno) {
        #[cfg(unix)]
        {
            let mut addr: libc::sockaddr_in = unsafe { std::mem::zeroed() };
            let mut addrlen: libc::socklen_t = std::mem::size_of::<libc::sockaddr_in>() as u32;
            if unsafe {
                libc::getpeername(
                    self.fd,
                    &mut addr as *mut libc::sockaddr_in as *mut libc::sockaddr,
                    &mut addrlen,
                )
            } != 0
            {
                return (SockAddrIn::default(), get_last_error());
            }
            (from_sockaddr_in(&addr), Errno::Success)
        }
        #[cfg(not(unix))]
        {
            (SockAddrIn::default(), Errno::Other)
        }
    }

    fn get_sock_name(&self) -> (SockAddrIn, Errno) {
        #[cfg(unix)]
        {
            let mut addr: libc::sockaddr_in = unsafe { std::mem::zeroed() };
            let mut addrlen: libc::socklen_t = std::mem::size_of::<libc::sockaddr_in>() as u32;
            if unsafe {
                libc::getsockname(
                    self.fd,
                    &mut addr as *mut libc::sockaddr_in as *mut libc::sockaddr,
                    &mut addrlen,
                )
            } != 0
            {
                return (SockAddrIn::default(), get_last_error());
            }
            (from_sockaddr_in(&addr), Errno::Success)
        }
        #[cfg(not(unix))]
        {
            (SockAddrIn::default(), Errno::Other)
        }
    }

    fn bind(&mut self, addr: SockAddrIn) -> Errno {
        #[cfg(unix)]
        {
            let addr_in = to_sockaddr_in(&addr);
            if unsafe {
                libc::bind(
                    self.fd,
                    &addr_in as *const libc::sockaddr_in as *const libc::sockaddr,
                    std::mem::size_of::<libc::sockaddr_in>() as u32,
                )
            } == 0
            {
                Errno::Success
            } else {
                get_last_error()
            }
        }
        #[cfg(not(unix))]
        {
            let _ = addr;
            Errno::Other
        }
    }

    fn listen(&mut self, backlog: i32) -> Errno {
        #[cfg(unix)]
        {
            if unsafe { libc::listen(self.fd, backlog) } == 0 {
                Errno::Success
            } else {
                get_last_error()
            }
        }
        #[cfg(not(unix))]
        {
            let _ = backlog;
            Errno::Other
        }
    }

    fn shutdown(&mut self, how: ShutdownHow) -> Errno {
        #[cfg(unix)]
        {
            let host_how = match how {
                ShutdownHow::RD => libc::SHUT_RD,
                ShutdownHow::WR => libc::SHUT_WR,
                ShutdownHow::RDWR => libc::SHUT_RDWR,
            };
            if unsafe { libc::shutdown(self.fd, host_how) } == 0 {
                Errno::Success
            } else {
                get_last_error()
            }
        }
        #[cfg(not(unix))]
        {
            let _ = how;
            Errno::Other
        }
    }

    fn recv(&mut self, flags: i32, message: &mut [u8]) -> (i32, Errno) {
        #[cfg(unix)]
        {
            let result = unsafe {
                libc::recv(
                    self.fd,
                    message.as_mut_ptr() as *mut libc::c_void,
                    message.len(),
                    flags,
                )
            };
            if result >= 0 {
                (result as i32, Errno::Success)
            } else {
                (-1, get_last_error())
            }
        }
        #[cfg(not(unix))]
        {
            let _ = (flags, message);
            (-1, Errno::Other)
        }
    }

    fn recv_from(
        &mut self,
        flags: i32,
        message: &mut [u8],
        addr: Option<&mut SockAddrIn>,
    ) -> (i32, Errno) {
        #[cfg(unix)]
        {
            let mut addr_in: libc::sockaddr_in = unsafe { std::mem::zeroed() };
            let mut addrlen: libc::socklen_t = std::mem::size_of::<libc::sockaddr_in>() as u32;
            let (p_addr, p_addrlen) = if addr.is_some() {
                (
                    &mut addr_in as *mut libc::sockaddr_in as *mut libc::sockaddr,
                    &mut addrlen as *mut libc::socklen_t,
                )
            } else {
                (std::ptr::null_mut(), std::ptr::null_mut())
            };

            let result = unsafe {
                libc::recvfrom(
                    self.fd,
                    message.as_mut_ptr() as *mut libc::c_void,
                    message.len(),
                    flags,
                    p_addr,
                    p_addrlen,
                )
            };
            if result >= 0 {
                if let Some(out_addr) = addr {
                    *out_addr = from_sockaddr_in(&addr_in);
                }
                (result as i32, Errno::Success)
            } else {
                (-1, get_last_error())
            }
        }
        #[cfg(not(unix))]
        {
            let _ = (flags, message, addr);
            (-1, Errno::Other)
        }
    }

    fn send(&mut self, message: &[u8], flags: i32) -> (i32, Errno) {
        #[cfg(unix)]
        {
            let result = unsafe {
                libc::send(
                    self.fd,
                    message.as_ptr() as *const libc::c_void,
                    message.len(),
                    flags,
                )
            };
            if result >= 0 {
                (result as i32, Errno::Success)
            } else {
                (-1, get_last_error())
            }
        }
        #[cfg(not(unix))]
        {
            let _ = (message, flags);
            (-1, Errno::Other)
        }
    }

    fn send_to(&mut self, flags: u32, message: &[u8], addr: Option<&SockAddrIn>) -> (i32, Errno) {
        #[cfg(unix)]
        {
            let (p_addr, addrlen) = if let Some(a) = addr {
                let addr_in = to_sockaddr_in(a);
                // Need to keep addr_in alive — use a local and pass pointer.
                let boxed = Box::new(addr_in);
                let ptr = Box::into_raw(boxed);
                (
                    ptr as *const libc::sockaddr,
                    std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
                )
            } else {
                (std::ptr::null(), 0)
            };

            let result = unsafe {
                libc::sendto(
                    self.fd,
                    message.as_ptr() as *const libc::c_void,
                    message.len(),
                    flags as i32,
                    p_addr,
                    addrlen,
                )
            };

            // Clean up the boxed addr if we allocated one.
            if !p_addr.is_null() {
                unsafe {
                    let _ = Box::from_raw(p_addr as *mut libc::sockaddr_in);
                }
            }

            if result >= 0 {
                (result as i32, Errno::Success)
            } else {
                (-1, get_last_error())
            }
        }
        #[cfg(not(unix))]
        {
            let _ = (flags, message, addr);
            (-1, Errno::Other)
        }
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
pub fn poll(pollfds: &mut [PollFD], timeout: i32) -> (i32, Errno) {
    #[cfg(unix)]
    {
        let mut fds: Vec<libc::pollfd> = pollfds
            .iter()
            .map(|pfd| libc::pollfd {
                fd: pfd.fd,
                events: pfd.events as i16,
                revents: 0,
            })
            .collect();

        let result = unsafe { libc::poll(fds.as_mut_ptr(), fds.len() as libc::nfds_t, timeout) };

        if result >= 0 {
            for (i, fd) in fds.iter().enumerate() {
                pollfds[i].revents = fd.revents as u16;
            }
            (result, Errno::Success)
        } else {
            (-1, get_last_error())
        }
    }
    #[cfg(not(unix))]
    {
        let _ = (pollfds, timeout);
        (0, Errno::Success)
    }
}

/// Poll file descriptor entry (simplified).
pub struct PollFD {
    pub fd: i32,
    pub events: u16,
    pub revents: u16,
}
