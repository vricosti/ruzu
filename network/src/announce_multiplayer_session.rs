// SPDX-FileCopyrightText: Copyright 2017 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/network/announce_multiplayer_session.h and
//! announce_multiplayer_session.cpp
//!
//! Instruments `AnnounceMultiplayerRoom::Backend`. Creates a thread that
//! regularly updates the room information and submits them. An async get of
//! room information is also possible.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use parking_lot::Mutex;

use common::announce_multiplayer_room::{
    self, Backend, NullBackend, RoomList, WebResult, WebResultCode,
};

use crate::network::RoomNetwork;
use crate::room::{Room, RoomState, NETWORK_VERSION};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Time between room announcements to web_service.
const ANNOUNCE_TIME_INTERVAL: Duration = Duration::from_secs(15);

// ---------------------------------------------------------------------------
// AnnounceMultiplayerSession
// ---------------------------------------------------------------------------

/// Callback handle for error callbacks.
pub type CallbackHandle = Arc<Box<dyn Fn(&WebResult) + Send + Sync>>;

/// Instruments `AnnounceMultiplayerRoom::Backend`.
/// Maps to C++ `Core::AnnounceMultiplayerSession`.
pub struct AnnounceMultiplayerSession {
    error_callbacks: Mutex<Vec<CallbackHandle>>,

    /// Backend interface that logs fields.
    backend: Mutex<Box<dyn Backend>>,

    /// Whether the room has been registered.
    registered: AtomicBool,

    /// Whether the session is running (announce thread is active).
    running: AtomicBool,
    // NOTE: The actual announce thread, shutdown_event, and RoomNetwork weak
    // reference are not fully ported since the networking layer (ENet) is
    // stubbed. The public API is preserved for structural parity.
}

impl AnnounceMultiplayerSession {
    /// Creates a new session.
    ///
    /// NOTE: In the C++ version this takes `RoomNetwork&` and optionally
    /// creates a `WebService::RoomJson` backend when `ENABLE_WEB_SERVICE` is
    /// defined. Here we default to `NullBackend`.
    pub fn new(_room_network: &RoomNetwork) -> Self {
        Self {
            error_callbacks: Mutex::new(Vec::new()),
            backend: Mutex::new(Box::new(NullBackend)),
            registered: AtomicBool::new(false),
            running: AtomicBool::new(false),
        }
    }

    /// Allows binding a function that will get called if the announce
    /// encounters an error.
    pub fn bind_error_callback(
        &self,
        callback: impl Fn(&WebResult) + Send + Sync + 'static,
    ) -> CallbackHandle {
        let handle: CallbackHandle = Arc::new(Box::new(callback));
        self.error_callbacks.lock().push(handle.clone());
        handle
    }

    /// Unbind a function from the error callbacks.
    pub fn unbind_error_callback(&self, handle: &CallbackHandle) {
        let mut callbacks = self.error_callbacks.lock();
        callbacks.retain(|h| !Arc::ptr_eq(h, handle));
    }

    /// Registers a room to web services.
    pub fn register(&self) -> WebResult {
        // NOTE: Full implementation requires access to Room via RoomNetwork.
        // Stubbed to call backend.register() directly.
        let result = self.backend.lock().register();
        if result.result_code != WebResultCode::Success {
            return result;
        }
        log::info!("Room has been registered");
        self.registered.store(true, Ordering::SeqCst);
        WebResult {
            result_code: WebResultCode::Success,
            result_string: String::new(),
            returned_data: String::new(),
        }
    }

    /// Starts the announce of a room to web services.
    pub fn start(&self) {
        // NOTE: Thread-based announce loop is not ported (ENet dependency).
        // This sets the running flag for API parity.
        self.running.store(true, Ordering::SeqCst);
        log::warn!("AnnounceMultiplayerSession::start: networking thread not ported; announce loop will not run");
    }

    /// Stops the announce to web services.
    pub fn stop(&self) {
        if self.running.load(Ordering::SeqCst) {
            self.running.store(false, Ordering::SeqCst);
            self.backend.lock().delete();
            self.registered.store(false, Ordering::SeqCst);
        }
    }

    /// Returns a list of all room information the backend got.
    pub fn get_room_list(&self) -> RoomList {
        self.backend.lock().get_room_list()
    }

    /// Whether the announce session is still running.
    pub fn is_running(&self) -> bool {
        self.running.load(Ordering::SeqCst)
    }

    /// Recreates the backend, updating the credentials.
    /// This can only be used when the announce session is not running.
    pub fn update_credentials(&self) {
        assert!(
            !self.is_running(),
            "Credentials can only be updated when session is not running"
        );
        // NOTE: Would recreate WebService::RoomJson if ENABLE_WEB_SERVICE.
        // Currently resets to NullBackend.
        *self.backend.lock() = Box::new(NullBackend);
    }
}

impl Drop for AnnounceMultiplayerSession {
    fn drop(&mut self) {
        self.stop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::network::RoomNetwork;

    #[test]
    fn test_session_register_with_null_backend() {
        let rn = RoomNetwork::new();
        let session = AnnounceMultiplayerSession::new(&rn);
        // NullBackend returns NoWebservice
        let result = session.register();
        assert_eq!(result.result_code, WebResultCode::NoWebservice);
    }

    #[test]
    fn test_session_is_not_running_by_default() {
        let rn = RoomNetwork::new();
        let session = AnnounceMultiplayerSession::new(&rn);
        assert!(!session.is_running());
    }
}
