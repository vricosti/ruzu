//! Port of zuyu/src/core/hle/service/set/setting_formats/private_settings.h and .cpp
//!
//! Private settings format.

/// Port of Service::Set::PrivateSettings
#[derive(Debug, Clone, Default)]
pub struct PrivateSettings {
    // TODO: Port fields from upstream private_settings.h
}

impl PrivateSettings {
    pub fn new() -> Self {
        Self::default()
    }
}
