//! Port of zuyu/src/core/hle/service/hid/applet_resource.h and applet_resource.cpp
//!
//! IAppletResource service.

/// IPC command table for IAppletResource:
///
/// | Cmd | Name                  |
/// |-----|-----------------------|
/// | 0   | GetSharedMemoryHandle |
pub struct IAppletResource;

impl IAppletResource {
    pub fn new() -> Self {
        Self
    }
}
