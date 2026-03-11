//! Port of zuyu/src/core/hle/service/set/setting_formats/appln_settings.h and .cpp
//!
//! Application settings format.

/// Port of Service::Set::ApplnSettings
#[derive(Debug, Clone, Default)]
pub struct ApplnSettings {
    // TODO: Port fields from upstream appln_settings.h
}

impl ApplnSettings {
    pub fn new() -> Self {
        Self::default()
    }
}
