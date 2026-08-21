//! Port of zuyu/src/core/hle/service/set/setting_formats/appln_settings.h and .cpp
//!
//! Application settings format.

/// Port of Service::Set::ApplnSettings
#[derive(Clone, Copy)]
#[repr(C)]
pub struct ApplnSettings {
    /// Reserved
    pub _reserved_0x00: [u8; 0x10],

    /// nn::util::Uuid MiiAuthorId, copied from system settings 0x94B0
    pub mii_author_id: [u8; 16],

    /// Reserved
    pub _reserved_0x20: [u8; 0x30],

    /// nn::settings::system::ServiceDiscoveryControlSettings
    pub service_discovery_control_settings: u32,

    /// Reserved
    pub _reserved_0x54: [u8; 0x20],

    /// in_repair_process_enable_flag
    pub in_repair_process_enable_flag: bool,

    /// Padding
    pub _padding_0x75: [u8; 0x3],
}

// offsetof checks (matching upstream static_asserts)
const _: () = {
    assert!(core::mem::offset_of!(ApplnSettings, mii_author_id) == 0x10);
    assert!(core::mem::offset_of!(ApplnSettings, service_discovery_control_settings) == 0x50);
    assert!(core::mem::offset_of!(ApplnSettings, in_repair_process_enable_flag) == 0x74);
    assert!(core::mem::size_of::<ApplnSettings>() == 0x78);
};

impl Default for ApplnSettings {
    fn default() -> Self {
        // SAFETY: All-zero is valid for this repr(C) struct of plain data types.
        unsafe { core::mem::zeroed() }
    }
}

impl core::fmt::Debug for ApplnSettings {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ApplnSettings")
            .field(
                "service_discovery_control_settings",
                &self.service_discovery_control_settings,
            )
            .field(
                "in_repair_process_enable_flag",
                &self.in_repair_process_enable_flag,
            )
            .finish()
    }
}

impl ApplnSettings {
    pub fn new() -> Self {
        Self::default()
    }
}
