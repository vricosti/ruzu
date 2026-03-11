// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/debugger/debugger.h and debugger.cpp
//! Top-level Debugger struct that manages debug connections.

use std::sync::Mutex;

/// Signal types for debugger notifications.
///
/// Corresponds to upstream anonymous `SignalType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignalType {
    Stopped,
    Watchpoint,
    ShuttingDown,
}

/// Signal information passed to the debugger.
///
/// Corresponds to upstream `SignalInfo`.
#[derive(Debug, Clone)]
pub struct SignalInfo {
    pub type_: SignalType,
    pub thread_id: u64,
    /// Start address of watchpoint, if applicable.
    pub watchpoint_addr: Option<u64>,
    /// Type of watchpoint, if applicable.
    pub watchpoint_type: Option<u8>,
}

/// Internal debugger implementation.
///
/// Corresponds to upstream `Core::DebuggerImpl`.
struct DebuggerImpl {
    port: u16,
    stopped: bool,
    connection_lock: Mutex<()>,
    // TODO: Add actual TCP server, connection state, GDBStub frontend
    // when async networking and kernel types are available.
}

impl DebuggerImpl {
    fn new(port: u16) -> Option<Self> {
        log::info!("Starting debugger server on port {}...", port);
        // TODO: Initialize TCP acceptor and connection thread.
        // Upstream uses boost::asio for TCP server.
        Some(Self {
            port,
            stopped: false,
            connection_lock: Mutex::new(()),
        })
    }

    fn signal_debugger(&mut self, signal_info: SignalInfo) -> bool {
        let _lock = self.connection_lock.lock().unwrap();

        if self.stopped {
            // Do not notify the debugger about another event.
            return false;
        }

        self.stopped = true;
        // TODO: Write to signal pipe to wake up debug interface.
        // Upstream writes to a boost::process::async_pipe.
        log::debug!(
            "Debugger signaled: {:?}, thread={}",
            signal_info.type_,
            signal_info.thread_id
        );

        true
    }
}

impl Drop for DebuggerImpl {
    fn drop(&mut self) {
        // TODO: Shut down connection thread and IO context.
        log::info!("Shutting down debugger server on port {}", self.port);
    }
}

/// Top-level debugger that manages debug connections.
///
/// Corresponds to upstream `Core::Debugger`.
pub struct Debugger {
    impl_: Option<DebuggerImpl>,
}

impl Debugger {
    /// Create a new Debugger listening on `server_port`.
    /// Does not create the debugger if the port is already in use.
    ///
    /// Corresponds to upstream `Debugger::Debugger(Core::System&, u16)`.
    pub fn new(server_port: u16) -> Self {
        let impl_ = match DebuggerImpl::new(server_port) {
            Some(d) => Some(d),
            None => {
                log::error!("Failed to initialize debugger");
                None
            }
        };
        Self { impl_ }
    }

    /// Notify the debugger that the given thread is stopped.
    ///
    /// Corresponds to upstream `Debugger::NotifyThreadStopped`.
    pub fn notify_thread_stopped(&mut self, thread_id: u64) -> bool {
        if let Some(ref mut impl_) = self.impl_ {
            impl_.signal_debugger(SignalInfo {
                type_: SignalType::Stopped,
                thread_id,
                watchpoint_addr: None,
                watchpoint_type: None,
            })
        } else {
            false
        }
    }

    /// Notify the debugger that a shutdown is being performed.
    ///
    /// Corresponds to upstream `Debugger::NotifyShutdown`.
    pub fn notify_shutdown(&mut self) {
        if let Some(ref mut impl_) = self.impl_ {
            impl_.signal_debugger(SignalInfo {
                type_: SignalType::ShuttingDown,
                thread_id: 0,
                watchpoint_addr: None,
                watchpoint_type: None,
            });
        }
    }

    /// Notify the debugger that a thread has stopped due to a watchpoint.
    ///
    /// Corresponds to upstream `Debugger::NotifyThreadWatchpoint`.
    pub fn notify_thread_watchpoint(
        &mut self,
        thread_id: u64,
        watch_addr: u64,
        watch_type: u8,
    ) -> bool {
        if let Some(ref mut impl_) = self.impl_ {
            impl_.signal_debugger(SignalInfo {
                type_: SignalType::Watchpoint,
                thread_id,
                watchpoint_addr: Some(watch_addr),
                watchpoint_type: Some(watch_type),
            })
        } else {
            false
        }
    }
}
