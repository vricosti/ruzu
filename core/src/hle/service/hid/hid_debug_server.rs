//! Port of zuyu/src/core/hle/service/hid/hid_debug_server.h and hid_debug_server.cpp
//!
//! IHidDebugServer service ("hid:dbg").

/// IHidDebugServer - debug interface for HID.
/// IPC commands are debug-only and not documented here in full.
pub struct IHidDebugServer;

impl IHidDebugServer {
    pub fn new() -> Self {
        Self
    }
}
