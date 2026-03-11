//! Port of zuyu/src/core/hle/service/set/setting_formats/device_settings.h and .cpp
//!
//! Device settings format.

/// Port of Service::Set::DeviceSettings
#[derive(Debug, Clone, Default)]
pub struct DeviceSettings {
    // TODO: Port fields from upstream device_settings.h
}

impl DeviceSettings {
    pub fn new() -> Self {
        Self::default()
    }
}
