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

/// Per-file-descriptor state.
///
/// Corresponds to `BSD::FileDescriptor` in upstream bsd.h.
pub struct FileDescriptor {
    /// Platform socket (abstracted -- in upstream this is a shared_ptr<SocketBase>).
    pub socket_fd: i32,
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

        self.file_descriptors[fd as usize] = Some(FileDescriptor {
            socket_fd: fd, // placeholder -- real impl would create platform socket
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

        // TODO: Actually perform poll via network layer
        // For now, return 0 ready fds
        unsafe {
            std::ptr::copy_nonoverlapping(
                fds.as_ptr() as *const u8,
                write_buffer.as_mut_ptr(),
                nfds as usize * poll_fd_size,
            );
        }

        (0, Errno::SUCCESS)
    }

    /// AcceptImpl -- accept a connection.
    ///
    /// Corresponds to `BSD::AcceptImpl` in upstream bsd.cpp.
    pub fn accept_impl(&mut self, fd: i32, _write_buffer: &mut Vec<u8>) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }

        let new_fd = self.find_free_file_descriptor_handle();
        if new_fd < 0 {
            log::error!("No more file descriptors available");
            return (-1, Errno::MFILE);
        }

        // TODO: Actually accept via network layer
        log::warn!("(STUBBED) BSD::AcceptImpl fd={}", fd);
        (-1, Errno::AGAIN)
    }

    /// BindImpl -- bind a socket to an address.
    ///
    /// Corresponds to `BSD::BindImpl` in upstream bsd.cpp.
    pub fn bind_impl(&self, fd: i32, addr: &[u8]) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        assert!(addr.len() == std::mem::size_of::<SockAddrIn>());
        // TODO: Actually bind via network layer
        log::warn!("(STUBBED) BSD::BindImpl fd={}", fd);
        Errno::SUCCESS
    }

    /// ConnectImpl -- connect to remote address.
    ///
    /// Corresponds to `BSD::ConnectImpl` in upstream bsd.cpp.
    pub fn connect_impl(&self, fd: i32, addr: &[u8]) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        assert!(addr.len() == std::mem::size_of::<SockAddrIn>());
        // TODO: Actually connect via network layer
        log::warn!("(STUBBED) BSD::ConnectImpl fd={}", fd);
        Errno::SUCCESS
    }

    /// GetPeerNameImpl
    ///
    /// Corresponds to `BSD::GetPeerNameImpl` in upstream bsd.cpp.
    pub fn get_peer_name_impl(&self, fd: i32, _write_buffer: &mut Vec<u8>) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        // TODO: Actually get peer name via network layer
        log::warn!("(STUBBED) BSD::GetPeerNameImpl fd={}", fd);
        Errno::SUCCESS
    }

    /// GetSockNameImpl
    ///
    /// Corresponds to `BSD::GetSockNameImpl` in upstream bsd.cpp.
    pub fn get_sock_name_impl(&self, fd: i32, _write_buffer: &mut Vec<u8>) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        // TODO: Actually get sock name via network layer
        log::warn!("(STUBBED) BSD::GetSockNameImpl fd={}", fd);
        Errno::SUCCESS
    }

    /// ListenImpl -- listen on a socket.
    ///
    /// Corresponds to `BSD::ListenImpl` in upstream bsd.cpp.
    pub fn listen_impl(&self, fd: i32, _backlog: i32) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        // TODO: Actually listen via network layer
        Errno::SUCCESS
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
                // TODO: descriptor.socket->SetNonBlock(enable)
                let _ = enable;
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

        match optname {
            OptName::ERROR => {
                // Return SUCCESS as the pending error (no real socket)
                if optval.len() != std::mem::size_of::<Errno>() {
                    return Errno::INVAL;
                }
                let success = Errno::SUCCESS as u32;
                optval[..4].copy_from_slice(&success.to_ne_bytes());
                Errno::SUCCESS
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
        &self,
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
            // TODO: socket->SetLinger(...)
            return Errno::SUCCESS;
        }

        assert!(optval.len() == std::mem::size_of::<u32>());
        let value = u32::from_ne_bytes([optval[0], optval[1], optval[2], optval[3]]);

        match optname {
            OptName::REUSEADDR => {
                assert!(value == 0 || value == 1);
                // TODO: socket->SetReuseAddr(value != 0)
                Errno::SUCCESS
            }
            OptName::KEEPALIVE => {
                assert!(value == 0 || value == 1);
                // TODO: socket->SetKeepAlive(value != 0)
                Errno::SUCCESS
            }
            OptName::BROADCAST => {
                assert!(value == 0 || value == 1);
                // TODO: socket->SetBroadcast(value != 0)
                Errno::SUCCESS
            }
            OptName::SNDBUF => {
                // TODO: socket->SetSndBuf(value)
                Errno::SUCCESS
            }
            OptName::RCVBUF => {
                // TODO: socket->SetRcvBuf(value)
                Errno::SUCCESS
            }
            OptName::SNDTIMEO => {
                // TODO: socket->SetSndTimeo(value)
                Errno::SUCCESS
            }
            OptName::RCVTIMEO => {
                // TODO: socket->SetRcvTimeo(value)
                Errno::SUCCESS
            }
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
    pub fn shutdown_impl(&self, fd: i32, how: i32) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        let _host_how = match how {
            0 => ShutdownHow::RD,
            1 => ShutdownHow::WR,
            2 => ShutdownHow::RDWR,
            _ => return Errno::INVAL,
        };
        // TODO: socket->Shutdown(host_how)
        Errno::SUCCESS
    }

    /// RecvImpl
    ///
    /// Corresponds to `BSD::RecvImpl` in upstream bsd.cpp.
    pub fn recv_impl(&mut self, fd: i32, mut flags: u32, _message: &mut Vec<u8>) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();

        // Apply MSG_DONTWAIT flag
        if (flags & FLAG_MSG_DONTWAIT) != 0 {
            flags &= !FLAG_MSG_DONTWAIT;
            if (descriptor.flags & FLAG_O_NONBLOCK) == 0 {
                // TODO: descriptor.socket->SetNonBlock(true)
            }
        }

        // TODO: Actually recv via network layer
        let ret = 0i32;
        let bsd_errno = Errno::SUCCESS;

        // Restore original state
        if (descriptor.flags & FLAG_O_NONBLOCK) == 0 {
            // TODO: descriptor.socket->SetNonBlock(false)
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
        _message: &mut Vec<u8>,
        addr: &mut Vec<u8>,
    ) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }

        let descriptor = self.file_descriptors[fd as usize].as_mut().unwrap();

        if descriptor.is_connection_based {
            // Connection based file descriptors (e.g. TCP) zero addr
            addr.clear();
        }

        // Apply MSG_DONTWAIT flag
        if (flags & FLAG_MSG_DONTWAIT) != 0 {
            flags &= !FLAG_MSG_DONTWAIT;
            if (descriptor.flags & FLAG_O_NONBLOCK) == 0 {
                // TODO: descriptor.socket->SetNonBlock(true)
            }
        }

        // TODO: Actually recvfrom via network layer
        let ret = 0i32;
        let bsd_errno = Errno::SUCCESS;

        // Restore original state
        if (descriptor.flags & FLAG_O_NONBLOCK) == 0 {
            // TODO: descriptor.socket->SetNonBlock(false)
        }

        (ret, bsd_errno)
    }

    /// SendImpl
    ///
    /// Corresponds to `BSD::SendImpl` in upstream bsd.cpp.
    pub fn send_impl(&self, fd: i32, _flags: u32, message: &[u8]) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }
        // TODO: Actually send via network layer
        (message.len() as i32, Errno::SUCCESS)
    }

    /// SendToImpl
    ///
    /// Corresponds to `BSD::SendToImpl` in upstream bsd.cpp.
    pub fn send_to_impl(
        &self,
        fd: i32,
        _flags: u32,
        message: &[u8],
        addr: &[u8],
    ) -> (i32, Errno) {
        if !self.is_file_descriptor_valid(fd) {
            return (-1, Errno::BADF);
        }
        if !addr.is_empty() {
            assert!(addr.len() == std::mem::size_of::<SockAddrIn>());
        }
        // TODO: Actually sendto via network layer
        (message.len() as i32, Errno::SUCCESS)
    }

    /// CloseImpl -- close a file descriptor.
    ///
    /// Corresponds to `BSD::CloseImpl` in upstream bsd.cpp.
    pub fn close_impl(&mut self, fd: i32) -> Errno {
        if !self.is_file_descriptor_valid(fd) {
            return Errno::BADF;
        }
        // TODO: socket->Close()
        log::info!("Close socket fd={}", fd);
        self.file_descriptors[fd as usize] = None;
        Errno::SUCCESS
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

        // Copy the file descriptor state (upstream copies the shared_ptr)
        let src = self.file_descriptors[fd as usize].as_ref().unwrap();
        self.file_descriptors[new_fd as usize] = Some(FileDescriptor {
            socket_fd: src.socket_fd,
            flags: src.flags,
            is_connection_based: src.is_connection_based,
        });

        Ok(new_fd)
    }

    /// GetSocket -- get a socket reference by fd.
    ///
    /// Corresponds to `BSD::GetSocket` in upstream bsd.cpp.
    /// Used by SSL service to access BSD sockets.
    pub fn get_socket(&self, fd: i32) -> Option<i32> {
        if !self.is_file_descriptor_valid(fd) {
            return None;
        }
        Some(self.file_descriptors[fd as usize].as_ref().unwrap().socket_fd)
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
    pub fn on_proxy_packet_received(&self, _packet: &[u8]) {
        for optional_descriptor in self.file_descriptors.iter() {
            if let Some(_descriptor) = optional_descriptor {
                // TODO: descriptor.socket->HandleProxyPacket(packet)
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
