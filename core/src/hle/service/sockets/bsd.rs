// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/sockets/bsd.h
//! Port of zuyu/src/core/hle/service/sockets/bsd.cpp
//!
//! BSD socket service -- "bsd:u" and "bsd:s".

use super::sockets::{
    Domain, Errno, FcntlCmd, Linger, OptName, PollEvents, PollFD, Protocol, ShutdownHow,
    SockAddrIn, SocketLevel, Type,
};
use crate::internal_network::network::{
    Domain as NetDomain, Errno as NetErrno, Protocol as NetProtocol, ShutdownHow as NetShutdownHow,
    SockAddrIn as NetSockAddrIn, Type as NetType,
};
use crate::internal_network::sockets::{
    self as net_sockets, Socket, SocketBase,
};

/// Maximum number of file descriptors.
///
/// Corresponds to `MAX_FD` in upstream bsd.h.
pub const MAX_FD: usize = 128;

/// Non-blocking socket flag.
///
/// Corresponds to `Network::FLAG_O_NONBLOCK` used in upstream bsd.cpp.
pub const FLAG_O_NONBLOCK: i32 = 0x800;

/// MSG_DONTWAIT flag.
///
/// Corresponds to `Network::FLAG_MSG_DONTWAIT` used in upstream bsd.cpp.
pub const FLAG_MSG_DONTWAIT: u32 = 0x80;

/// IPC command table for BSD.
///
/// Corresponds to the function table in upstream bsd.cpp constructor.
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
    pub const REGISTER_RESOURCE_STATISTICS_NAME: u32 = 32;
    pub const INITIALIZE2: u32 = 33;
}

/// BSDCFG IPC command table.
///
/// Corresponds to the function table in upstream bsd.cpp `BSDCFG` constructor.
pub mod bsdcfg_commands {
    pub const SET_IF_UP: u32 = 0;
    pub const SET_IF_UP_WITH_EVENT: u32 = 1;
    pub const CANCEL_IF: u32 = 2;
    pub const SET_IF_DOWN: u32 = 3;
    pub const GET_IF_STATE: u32 = 4;
    pub const DHCP_RENEW: u32 = 5;
    pub const ADD_STATIC_ARP_ENTRY: u32 = 6;
    pub const REMOVE_ARP_ENTRY: u32 = 7;
    pub const LOOKUP_ARP_ENTRY: u32 = 8;
    pub const LOOKUP_ARP_ENTRY2: u32 = 9;
    pub const CLEAR_ARP_ENTRIES: u32 = 10;
    pub const CLEAR_ARP_ENTRIES2: u32 = 11;
    pub const PRINT_ARP_ENTRIES: u32 = 12;
    pub const UNKNOWN13: u32 = 13;
    pub const UNKNOWN14: u32 = 14;
    pub const UNKNOWN15: u32 = 15;
}

/// Helper: check if a socket type is connection-based.
///
/// Corresponds to `IsConnectionBased` in upstream bsd.cpp.
fn is_connection_based(ty: Type) -> bool {
    match ty {
        Type::STREAM => true,
        Type::DGRAM => false,
        _ => {
            log::warn!("Unimplemented socket type={:?}", ty);
            false
        }
    }
}

// --- Translation helpers between guest (service) and internal (network) types ---

/// Translate guest Domain to internal Domain.
///
/// Corresponds to `Translate(Domain)` in upstream sockets_translate.cpp.
fn translate_domain(domain: Domain) -> NetDomain {
    match domain {
        Domain::Unspecified => NetDomain::Unspecified,
        Domain::INET => NetDomain::INET,
    }
}

/// Translate internal Domain to guest Domain.
fn translate_domain_back(domain: Option<NetDomain>) -> u8 {
    match domain {
        Some(NetDomain::INET) => Domain::INET as u8,
        _ => Domain::Unspecified as u8,
    }
}

/// Translate guest Type to internal Type.
fn translate_type(ty: Type) -> NetType {
    match ty {
        Type::Unspecified => NetType::Unspecified,
        Type::STREAM => NetType::STREAM,
        Type::DGRAM => NetType::DGRAM,
        Type::RAW => NetType::RAW,
        Type::SEQPACKET => NetType::SEQPACKET,
    }
}

/// Translate guest Protocol to internal Protocol.
fn translate_protocol(protocol: Protocol) -> NetProtocol {
    match protocol {
        Protocol::Unspecified => NetProtocol::Unspecified,
        Protocol::ICMP => NetProtocol::ICMP,
        Protocol::TCP => NetProtocol::TCP,
        Protocol::UDP => NetProtocol::UDP,
    }
}

/// Translate guest SockAddrIn to internal SockAddrIn.
///
/// Corresponds to `Translate(SockAddrIn)` in upstream sockets_translate.cpp.
/// Note: portno byte-swap matches upstream (big-endian to host).
fn translate_sockaddr_to_network(value: &SockAddrIn) -> NetSockAddrIn {
    // Note: 6 is incorrect, but can be passed by homebrew (because libnx sets
    // sin_len to 6 when deserializing getaddrinfo results).
    assert!(
        value.len == 0
            || value.len == std::mem::size_of::<SockAddrIn>() as u8
            || value.len == 6
    );

    let domain = match value.family {
        2 => NetDomain::INET,
        _ => NetDomain::Unspecified,
    };

    NetSockAddrIn {
        family: Some(domain),
        ip: value.ip,
        portno: (value.portno >> 8) | (value.portno << 8),
    }
}

/// Translate internal SockAddrIn to guest SockAddrIn.
///
/// Corresponds to `Translate(Network::SockAddrIn)` in upstream sockets_translate.cpp.
fn translate_sockaddr_from_network(value: &NetSockAddrIn) -> SockAddrIn {
    SockAddrIn {
        len: std::mem::size_of::<SockAddrIn>() as u8,
        family: translate_domain_back(value.family),
        portno: (value.portno >> 8) | (value.portno << 8),
        ip: value.ip,
        zeroes: [0u8; 8],
    }
}

/// Translate internal Errno to guest Errno.
///
/// Corresponds to `Translate(Network::Errno)` in upstream sockets_translate.cpp.
fn translate_errno(value: NetErrno) -> Errno {
    match value {
        NetErrno::Success => Errno::SUCCESS,
        NetErrno::Badf => Errno::BADF,
        NetErrno::Again => Errno::AGAIN,
        NetErrno::Inval => Errno::INVAL,
        NetErrno::Mfile => Errno::MFILE,
        NetErrno::Pipe => Errno::PIPE,
        NetErrno::Connrefused => Errno::CONNREFUSED,
        NetErrno::Notconn => Errno::NOTCONN,
        NetErrno::Timedout => Errno::TIMEDOUT,
        NetErrno::Connaborted => Errno::CONNABORTED,
        NetErrno::Connreset => Errno::CONNRESET,
        NetErrno::Inprogress => Errno::INPROGRESS,
        _ => {
            log::warn!("Unimplemented errno={:?}", value);
            Errno::SUCCESS
        }
    }
}

/// Translate an (i32, NetErrno) pair to (i32, Errno).
///
/// Corresponds to `Translate(std::pair<s32, Network::Errno>)` in upstream.
fn translate_result(value: (i32, NetErrno)) -> (i32, Errno) {
    (value.0, translate_errno(value.1))
}

/// Translate guest ShutdownHow to internal ShutdownHow.
fn translate_shutdown_how(how: ShutdownHow) -> NetShutdownHow {
    match how {
        ShutdownHow::RD => NetShutdownHow::RD,
        ShutdownHow::WR => NetShutdownHow::WR,
        ShutdownHow::RDWR => NetShutdownHow::RDWR,
    }
}

/// Per-file-descriptor state.
///
/// Corresponds to `BSD::FileDescriptor` in upstream bsd.h.
pub struct FileDescriptor {
    /// Platform socket (corresponds to upstream shared_ptr<SocketBase>).
    pub socket: Box<dyn SocketBase>,
    pub flags: i32,
    pub is_connection_based: bool,
}

/// Work structs for async operations.
///
/// Corresponds to PollWork, AcceptWork, ConnectWork, RecvWork, RecvFromWork,
/// SendWork, SendToWork in upstream bsd.h.
/// In this port, work is executed synchronously (matching upstream ExecuteWork pattern).

/// BSD socket service.
///
/// Corresponds to `BSD` in upstream bsd.h / bsd.cpp.
pub struct Bsd {
    file_descriptors: [Option<FileDescriptor>; MAX_FD],
    is_privileged: bool,
}

impl Bsd {
    pub fn new(is_privileged: bool) -> Self {
        Self {
            file_descriptors: std::array::from_fn(|_| None),
            is_privileged,
        }
    }

    /// Returns whether this is a privileged (bsd:s) instance.
    pub fn is_privileged(&self) -> bool {
        self.is_privileged
    }

    // --- Internal implementation methods ---

    /// Find the first free file descriptor slot.
    ///
    /// Corresponds to `BSD::FindFreeFileDescriptorHandle` in upstream bsd.cpp.
    fn find_free_file_descriptor_handle(&self) -> i32 {
        for (i, fd) in self.file_descriptors.iter().enumerate() {
            if fd.is_none() {
                return i as i32;
            }
        }
        -1
    }

    /// Check if a file descriptor index is valid.
    ///
    /// Corresponds to `BSD::IsFileDescriptorValid` in upstream bsd.cpp.
    fn is_file_descriptor_valid(&self, fd: i32) -> bool {
        if fd > MAX_FD as i32 || fd < 0 {
            log::error!("Invalid file descriptor handle={}", fd);
            return false;
        }
        if self.file_descriptors[fd as usize].is_none() {
            log::error!("File descriptor handle={} is not allocated", fd);
            return false;
        }
        true
    }

    /// Build a standard errno response.
    ///
    /// Corresponds to `BSD::BuildErrnoResponse` in upstream bsd.cpp.
    /// Returns (ret, errno) where ret is 0 on success, -1 on error.
    pub fn build_errno_response(bsd_errno: Errno) -> (i32, Errno) {
        let ret = if bsd_errno == Errno::SUCCESS { 0 } else { -1 };
        (ret, bsd_errno)
    }

    /// SocketImpl -- create a new socket.
    ///
    /// Corresponds to `BSD::SocketImpl` in upstream bsd.cpp.
    pub fn socket_impl(&mut self, domain: Domain, mut ty: Type, protocol: Protocol) -> (i32, Errno) {
        if ty == Type::SEQPACKET {
            log::warn!("SOCK_SEQPACKET errno management unimplemented");
        } else if ty == Type::RAW && (domain != Domain::INET || protocol != Protocol::ICMP) {
            log::warn!("SOCK_RAW errno management unimplemented");
        }

        // Check and strip unknown flag (bit 29)
        let raw_type = ty as u32;
        let unk_flag = (raw_type & 0x20000000) != 0;
        if unk_flag {
            log::warn!("Unknown flag in type");
            ty = unsafe { std::mem::transmute(raw_type & !0x20000000) };
        }

        let fd = self.find_free_file_descriptor_handle();
        if fd < 0 {
            log::error!("No more file descriptors available");
            return (-1, Errno::MFILE);
        }

        log::info!("New socket fd={}", fd);

        let mut socket = Socket::new();
        let errno = socket.initialize(
            translate_domain(domain),
            translate_type(ty),
            translate_protocol(protocol),
        );
        if errno != NetErrno::Success {
            return (-1, translate_errno(errno));
        }

        self.file_descriptors[fd as usize] = Some(FileDescriptor {
            socket: Box::new(socket),
            flags: 0,
            is_connection_based: is_connection_based(ty),
        });

        (fd, Errno::SUCCESS)
    }

    /// PollImpl -- poll file descriptors.
    ///
    /// Corresponds to `BSD::PollImpl` in upstream bsd.cpp.
    pub fn poll_impl(
        &self,
        write_buffer: &mut [u8],
        read_buffer: &[u8],
        nfds: i32,
        timeout: i32,
    ) -> (i32, Errno) {
        if nfds <= 0 {
            // When no entries are provided, -1 is returned with errno zero
            return (-1, Errno::SUCCESS);
        }

        let poll_fd_size = std::mem::size_of::<PollFD>();
        if read_buffer.len() < nfds as usize * poll_fd_size {
            return (-1, Errno::INVAL);
        }
        if write_buffer.len() < nfds as usize * poll_fd_size {
            return (-1, Errno::INVAL);
        }

        // Validate timeout
        if timeout >= 0 {
            let seconds = timeout as i64 / 1000;
            let nanoseconds = (timeout as u64 % 1000) * 1_000_000;
            if seconds < 0 {
                return (-1, Errno::INVAL);
            }
            if nanoseconds > 999_999_999 {
                return (-1, Errno::INVAL);
            }
        } else if timeout != -1 {
            return (-1, Errno::INVAL);
        }

        // Parse poll fds from read buffer
        let mut fds = vec![PollFD::default(); nfds as usize];
        unsafe {
            std::ptr::copy_nonoverlapping(
                read_buffer.as_ptr(),
                fds.as_mut_ptr() as *mut u8,
                nfds as usize * poll_fd_size,
            );
        }

        // Validate fds
        for pollfd in fds.iter_mut() {
            if pollfd.fd > MAX_FD as i32 || pollfd.fd < 0 {
                log::error!("File descriptor handle={} is invalid", pollfd.fd);
                pollfd.revents = 0;
                return (0, Errno::SUCCESS);
            }

            if self.file_descriptors[pollfd.fd as usize].is_none() {
                log::trace!("File descriptor handle={} is not allocated", pollfd.fd);
                pollfd.revents = PollEvents::NVAL.bits();
                return (0, Errno::SUCCESS);
            }
        }

        // Build host poll fds using the real socket file descriptors
        let mut host_pollfds: Vec<net_sockets::PollFD> = fds
            .iter()
            .map(|pollfd| {
                let descriptor = self.file_descriptors[pollfd.fd as usize].as_ref().unwrap();
                net_sockets::PollFD {
                    fd: descriptor.socket.get_fd(),
                    events: pollfd.events,
                    revents: 0,
                }
            })
            .collect();

        let result = net_sockets::poll(&mut host_pollfds, timeout);

        // Copy revents back
        for (i, host_pollfd) in host_pollfds.iter().enumerate() {
            fds[i].revents = host_pollfd.revents;
        }

        unsafe {
            std::ptr::copy_nonoverlapping(
                fds.as_ptr() as *const u8,
                write_buffer.as_mut_ptr(),
                nfds as usize * poll_fd_size,
            );
        }

        translate_result(result)
    }

    /// AcceptImpl -- accept a connection.
    ///
    /// Corresponds to `BSD::AcceptImpl` in upstream bsd.cpp.
    pub fn accept_impl(&mut self, fd: i32, write_buffer: &mut Vec<u8>) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }

        let new_fd = self.find_free_file_descriptor_handle();
        if new_fd < 0 {
            log::error!("No more file descriptors available");
            return (-1, Errno::MFILE);
        }

        let is_conn_based = self.file_descriptors[fd as usize]
            .as_ref()
            .unwrap()
            .is_connection_based;

        let (result, bsd_errno) = self.file_descriptors[fd as usize]
            .as_mut()
            .unwrap()
            .socket
            .accept();

        if bsd_errno != NetErrno::Success {
            return (-1, translate_errno(bsd_errno));
        }

        let accept_result = result;
        let guest_addr_in = translate_sockaddr_from_network(&accept_result.sockaddr_in);

        // Write the guest address to the write buffer
        let addr_size = std::mem::size_of::<SockAddrIn>();
        write_buffer.resize(addr_size, 0);
        unsafe {
            std::ptr::copy_nonoverlapping(
                &guest_addr_in as *const SockAddrIn as *const u8,
                write_buffer.as_mut_ptr(),
                addr_size,
            );
        }

        self.file_descriptors[new_fd as usize] = Some(FileDescriptor {
            socket: accept_result.socket.unwrap(),
            flags: 0,
            is_connection_based: is_conn_based,
        });

        (new_fd, Errno::SUCCESS)
    }

    /// BindImpl -- bind a socket to an address.
    ///
    /// Corresponds to `BSD::BindImpl` in upstream bsd.cpp.
    pub fn bind_impl(&mut self, fd: i32, addr: &[u8]) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        assert!(addr.len() == std::mem::size_of::<SockAddrIn>());

        let mut guest_addr = SockAddrIn::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                addr.as_ptr(),
                &mut guest_addr as *mut SockAddrIn as *mut u8,
                std::mem::size_of::<SockAddrIn>(),
            );
        }
        let net_addr = translate_sockaddr_to_network(&guest_addr);

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();
        translate_errno(descriptor.socket.bind(net_addr))
    }

    /// ConnectImpl -- connect to remote address.
    ///
    /// Corresponds to `BSD::ConnectImpl` in upstream bsd.cpp.
    pub fn connect_impl(&mut self, fd: i32, addr: &[u8]) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        assert!(addr.len() == std::mem::size_of::<SockAddrIn>());

        let mut guest_addr = SockAddrIn::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                addr.as_ptr(),
                &mut guest_addr as *mut SockAddrIn as *mut u8,
                std::mem::size_of::<SockAddrIn>(),
            );
        }
        let net_addr = translate_sockaddr_to_network(&guest_addr);

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();
        translate_errno(descriptor.socket.connect(net_addr))
    }

    /// GetPeerNameImpl
    ///
    /// Corresponds to `BSD::GetPeerNameImpl` in upstream bsd.cpp.
    pub fn get_peer_name_impl(&self, fd: i32, write_buffer: &mut Vec<u8>) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }

        let descriptor = self.file_descriptors[fd as usize].as_ref().unwrap();
        let (addr_in, bsd_errno) = descriptor.socket.get_peer_name();
        if bsd_errno != NetErrno::Success {
            return translate_errno(bsd_errno);
        }

        let guest_addr_in = translate_sockaddr_from_network(&addr_in);
        let addr_size = std::mem::size_of::<SockAddrIn>();
        assert!(write_buffer.len() >= addr_size);
        write_buffer.resize(addr_size, 0);
        unsafe {
            std::ptr::copy_nonoverlapping(
                &guest_addr_in as *const SockAddrIn as *const u8,
                write_buffer.as_mut_ptr(),
                addr_size,
            );
        }
        translate_errno(bsd_errno)
    }

    /// GetSockNameImpl
    ///
    /// Corresponds to `BSD::GetSockNameImpl` in upstream bsd.cpp.
    pub fn get_sock_name_impl(&self, fd: i32, write_buffer: &mut Vec<u8>) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }

        let descriptor = self.file_descriptors[fd as usize].as_ref().unwrap();
        let (addr_in, bsd_errno) = descriptor.socket.get_sock_name();
        if bsd_errno != NetErrno::Success {
            return translate_errno(bsd_errno);
        }

        let guest_addr_in = translate_sockaddr_from_network(&addr_in);
        let addr_size = std::mem::size_of::<SockAddrIn>();
        assert!(write_buffer.len() >= addr_size);
        write_buffer.resize(addr_size, 0);
        unsafe {
            std::ptr::copy_nonoverlapping(
                &guest_addr_in as *const SockAddrIn as *const u8,
                write_buffer.as_mut_ptr(),
                addr_size,
            );
        }
        translate_errno(bsd_errno)
    }

    /// ListenImpl -- listen on a socket.
    ///
    /// Corresponds to `BSD::ListenImpl` in upstream bsd.cpp.
    pub fn listen_impl(&mut self, fd: i32, backlog: i32) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();
        translate_errno(descriptor.socket.listen(backlog))
    }

    /// FcntlImpl -- file control operations.
    ///
    /// Corresponds to `BSD::FcntlImpl` in upstream bsd.cpp.
    pub fn fcntl_impl(&mut self, fd: i32, cmd: FcntlCmd, arg: i32) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();

        match cmd {
            FcntlCmd::GETFL => {
                assert!(arg == 0);
                (descriptor.flags, Errno::SUCCESS)
            }
            FcntlCmd::SETFL => {
                let enable = (arg & FLAG_O_NONBLOCK) != 0;
                let bsd_errno = translate_errno(descriptor.socket.set_non_block(enable));
                if bsd_errno != Errno::SUCCESS {
                    return (-1, bsd_errno);
                }
                descriptor.flags = arg;
                (0, Errno::SUCCESS)
            }
        }
    }

    /// GetSockOptImpl
    ///
    /// Corresponds to `BSD::GetSockOptImpl` in upstream bsd.cpp.
    pub fn get_sock_opt_impl(
        &self,
        fd: i32,
        level: u32,
        optname: OptName,
        optval: &mut Vec<u8>,
    ) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }

        if level != SocketLevel::SOCKET as u32 {
            log::warn!("Unknown getsockopt level={}", level);
            return Errno::SUCCESS;
        }

        let descriptor = self.file_descriptors[fd as usize].as_ref().unwrap();

        match optname {
            OptName::ERROR => {
                let (pending_err, getsockopt_err) = descriptor.socket.get_pending_error();
                if getsockopt_err == NetErrno::Success {
                    let translated_pending_err = translate_errno(pending_err);
                    if optval.len() != std::mem::size_of::<Errno>() {
                        return Errno::INVAL;
                    }
                    optval.resize(std::mem::size_of::<Errno>(), 0);
                    let err_val = translated_pending_err as u32;
                    optval[..4].copy_from_slice(&err_val.to_ne_bytes());
                }
                translate_errno(getsockopt_err)
            }
            _ => {
                log::warn!("Unimplemented optname={:?}", optname);
                Errno::SUCCESS
            }
        }
    }

    /// SetSockOptImpl
    ///
    /// Corresponds to `BSD::SetSockOptImpl` in upstream bsd.cpp.
    pub fn set_sock_opt_impl(
        &mut self,
        fd: i32,
        level: u32,
        optname: OptName,
        optval: &[u8],
    ) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }

        if level != SocketLevel::SOCKET as u32 {
            log::warn!("Unknown setsockopt level={}", level);
            return Errno::SUCCESS;
        }

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();

        if optname == OptName::LINGER {
            assert!(optval.len() == std::mem::size_of::<Linger>());
            let mut linger = Linger::default();
            unsafe {
                std::ptr::copy_nonoverlapping(
                    optval.as_ptr(),
                    &mut linger as *mut Linger as *mut u8,
                    std::mem::size_of::<Linger>(),
                );
            }
            assert!(linger.onoff == 0 || linger.onoff == 1);
            return translate_errno(
                descriptor.socket.set_linger(linger.onoff != 0, linger.linger),
            );
        }

        assert!(optval.len() == std::mem::size_of::<u32>());
        let value = u32::from_ne_bytes([optval[0], optval[1], optval[2], optval[3]]);

        match optname {
            OptName::REUSEADDR => {
                assert!(value == 0 || value == 1);
                translate_errno(descriptor.socket.set_reuse_addr(value != 0))
            }
            OptName::KEEPALIVE => {
                assert!(value == 0 || value == 1);
                translate_errno(descriptor.socket.set_keep_alive(value != 0))
            }
            OptName::BROADCAST => {
                assert!(value == 0 || value == 1);
                translate_errno(descriptor.socket.set_broadcast(value != 0))
            }
            OptName::SNDBUF => translate_errno(descriptor.socket.set_snd_buf(value)),
            OptName::RCVBUF => translate_errno(descriptor.socket.set_rcv_buf(value)),
            OptName::SNDTIMEO => translate_errno(descriptor.socket.set_snd_timeo(value)),
            OptName::RCVTIMEO => translate_errno(descriptor.socket.set_rcv_timeo(value)),
            OptName::NOSIGPIPE => {
                log::warn!("(STUBBED) setting NOSIGPIPE to {}", value);
                Errno::SUCCESS
            }
            _ => {
                log::warn!("Unimplemented optname={:?}", optname);
                Errno::SUCCESS
            }
        }
    }

    /// ShutdownImpl
    ///
    /// Corresponds to `BSD::ShutdownImpl` in upstream bsd.cpp.
    pub fn shutdown_impl(&mut self, fd: i32, how: i32) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        let host_how = match how {
            0 => ShutdownHow::RD,
            1 => ShutdownHow::WR,
            2 => ShutdownHow::RDWR,
            _ => return Errno::INVAL,
        };
        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();
        translate_errno(descriptor.socket.shutdown(translate_shutdown_how(host_how)))
    }

    /// RecvImpl
    ///
    /// Corresponds to `BSD::RecvImpl` in upstream bsd.cpp.
    pub fn recv_impl(&mut self, fd: i32, mut flags: u32, message: &mut Vec<u8>) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();

        // Apply MSG_DONTWAIT flag
        if (flags & FLAG_MSG_DONTWAIT) != 0 {
            flags &= !FLAG_MSG_DONTWAIT;
            if (descriptor.flags & FLAG_O_NONBLOCK) == 0 {
                descriptor.socket.set_non_block(true);
            }
        }

        let (ret, bsd_errno) =
            translate_result(descriptor.socket.recv(flags as i32, message.as_mut_slice()));

        // Restore original state
        if (descriptor.flags & FLAG_O_NONBLOCK) == 0 {
            descriptor.socket.set_non_block(false);
        }

        (ret, bsd_errno)
    }

    /// RecvFromImpl
    ///
    /// Corresponds to `BSD::RecvFromImpl` in upstream bsd.cpp.
    pub fn recv_from_impl(
        &mut self,
        fd: i32,
        mut flags: u32,
        message: &mut Vec<u8>,
        addr: &mut Vec<u8>,
    ) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();

        let mut addr_in = NetSockAddrIn::default();
        let use_addr = if descriptor.is_connection_based {
            // Connection based file descriptors (e.g. TCP) zero addr
            addr.clear();
            false
        } else {
            true
        };

        // Apply MSG_DONTWAIT flag
        if (flags & FLAG_MSG_DONTWAIT) != 0 {
            flags &= !FLAG_MSG_DONTWAIT;
            if (descriptor.flags & FLAG_O_NONBLOCK) == 0 {
                descriptor.socket.set_non_block(true);
            }
        }

        let p_addr_in = if use_addr {
            Some(&mut addr_in)
        } else {
            None
        };

        let (ret, bsd_errno) = translate_result(
            descriptor
                .socket
                .recv_from(flags as i32, message.as_mut_slice(), p_addr_in),
        );

        // Restore original state
        if (descriptor.flags & FLAG_O_NONBLOCK) == 0 {
            descriptor.socket.set_non_block(false);
        }

        if use_addr {
            if ret < 0 {
                addr.clear();
            } else {
                assert!(addr.len() == std::mem::size_of::<SockAddrIn>());
                let guest_addr = translate_sockaddr_from_network(&addr_in);
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        &guest_addr as *const SockAddrIn as *const u8,
                        addr.as_mut_ptr(),
                        std::mem::size_of::<SockAddrIn>(),
                    );
                }
            }
        }

        (ret, bsd_errno)
    }

    /// SendImpl
    ///
    /// Corresponds to `BSD::SendImpl` in upstream bsd.cpp.
    pub fn send_impl(&mut self, fd: i32, flags: u32, message: &[u8]) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }
        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();
        translate_result(descriptor.socket.send(message, flags as i32))
    }

    /// SendToImpl
    ///
    /// Corresponds to `BSD::SendToImpl` in upstream bsd.cpp.
    pub fn send_to_impl(
        &mut self,
        fd: i32,
        flags: u32,
        message: &[u8],
        addr: &[u8],
    ) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }

        let p_addr_in = if !addr.is_empty() {
            assert!(addr.len() == std::mem::size_of::<SockAddrIn>());
            let mut guest_addr = SockAddrIn::default();
            unsafe {
                std::ptr::copy_nonoverlapping(
                    addr.as_ptr(),
                    &mut guest_addr as *mut SockAddrIn as *mut u8,
                    std::mem::size_of::<SockAddrIn>(),
                );
            }
            Some(translate_sockaddr_to_network(&guest_addr))
        } else {
            None
        };

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();
        translate_result(
            descriptor
                .socket
                .send_to(flags, message, p_addr_in.as_ref()),
        )
    }

    /// CloseImpl -- close a file descriptor.
    ///
    /// Corresponds to `BSD::CloseImpl` in upstream bsd.cpp.
    pub fn close_impl(&mut self, fd: i32) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }

        let bsd_errno = translate_errno(
            self.file_descriptors[fd as usize]
                .as_mut()
                .unwrap()
                .socket
                .close(),
        );
        if bsd_errno != Errno::SUCCESS {
            return bsd_errno;
        }

        log::info!("Close socket fd={}", fd);
        self.file_descriptors[fd as usize] = None;
        bsd_errno
    }

    /// DuplicateSocketImpl -- duplicate a file descriptor.
    ///
    /// Corresponds to `BSD::DuplicateSocketImpl` in upstream bsd.cpp.
    pub fn duplicate_socket_impl(&mut self, fd: i32) -> Result<i32, Errno> {
        if !self.is_file_descriptor_valid(fd) {
            return Err(Errno::BADF);
        }

        let new_fd = self.find_free_file_descriptor_handle();
        if new_fd < 0 {
            log::error!("No more file descriptors available");
            return Err(Errno::MFILE);
        }

        // Upstream copies the shared_ptr (shared ownership). In Rust we create a new Socket
        // wrapping the same underlying fd via Socket::from_fd. Note: this means the two
        // FileDescriptors share the same OS fd, matching upstream shared_ptr semantics.
        let src = self.file_descriptors[fd as usize].as_ref().unwrap();
        let src_fd_val = src.socket.get_fd();
        // Duplicate the OS-level file descriptor so both can close independently
        #[cfg(unix)]
        let new_os_fd = unsafe { libc::dup(src_fd_val) };
        #[cfg(not(unix))]
        let new_os_fd = -1;

        if new_os_fd < 0 {
            log::error!("Failed to dup socket fd");
            return Err(Errno::BADF);
        }

        let src_flags = src.flags;
        let src_is_conn = src.is_connection_based;

        self.file_descriptors[new_fd as usize] = Some(FileDescriptor {
            socket: Box::new(Socket::from_fd(new_os_fd)),
            flags: src_flags,
            is_connection_based: src_is_conn,
        });

        Ok(new_fd)
    }

    /// GetSocket -- get a socket reference by fd.
    ///
    /// Corresponds to `BSD::GetSocket` in upstream bsd.cpp.
    /// Used by SSL service to access BSD sockets.
    pub fn get_socket(&self, fd: i32) -> Option<&dyn SocketBase> {
        if !self.is_file_descriptor_valid(fd) {
            return None;
        }
        Some(
            self.file_descriptors[fd as usize]
                .as_ref()
                .unwrap()
                .socket
                .as_ref(),
        )
    }

    /// EventFd -- create event fd (stubbed).
    ///
    /// Corresponds to `BSD::EventFd` in upstream bsd.cpp.
    pub fn event_fd(&self, initval: u64, flags: u32) {
        log::warn!(
            "(STUBBED) BSD::EventFd called, initval={}, flags={}",
            initval,
            flags
        );
    }

    /// OnProxyPacketReceived -- handle incoming proxy packet.
    ///
    /// Corresponds to `BSD::OnProxyPacketReceived` in upstream bsd.cpp.
    pub fn on_proxy_packet_received(&mut self, packet: &crate::internal_network::network::ProxyPacket) {
        for optional_descriptor in self.file_descriptors.iter_mut() {
            if let Some(descriptor) = optional_descriptor {
                descriptor.socket.handle_proxy_packet(packet);
            }
        }
    }
}

/// BSDCFG service.
///
/// Corresponds to `BSDCFG` in upstream bsd.h / bsd.cpp.
/// All commands are nullptr (unimplemented) in upstream.
pub struct BsdCfg;

impl BsdCfg {
    pub fn new() -> Self {
        Self
    }
}
