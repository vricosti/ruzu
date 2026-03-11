//! Port of zuyu/src/core/hle/service/hid/hid_system_server.h and hid_system_server.cpp
//!
//! IHidSystemServer service ("hid:sys").

/// IHidSystemServer - system-level HID interface.
pub struct IHidSystemServer;

impl IHidSystemServer {
    pub fn new() -> Self {
        Self
    }
}
