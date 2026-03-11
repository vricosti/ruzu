//! Port of zuyu/src/core/hle/service/set/firmware_debug_settings_server.h and .cpp
//!
//! IFirmwareDebugSettingsServer service ("set:fd").

/// IPC command table for IFirmwareDebugSettingsServer ("set:fd"):
///
/// | Cmd | Name                        |
/// |-----|-----------------------------|
/// | 2   | SetSettingsItemValue        |
/// | 3   | ResetSettingsItemValue      |
/// | 4   | CreateSettingsItemKeyIterator|
/// | 10  | ReadSettings                |
/// | 11  | ResetSettings               |
/// | 20  | SetWebInspectorFlag         |
/// | 21  | SetAllowedSslHosts          |
/// | 22  | SetHostFsMountPoint         |
/// | 23  | SetMemoryUsageRateFlag      |
pub struct IFirmwareDebugSettingsServer;

impl IFirmwareDebugSettingsServer {
    pub fn new() -> Self {
        Self
    }
}
