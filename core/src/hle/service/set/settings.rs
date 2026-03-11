//! Port of zuyu/src/core/hle/service/set/settings.h and settings.cpp
//!
//! Entry point for the settings service module.

/// Named services registered by the set module:
/// - "set"     -> ISettingsServer
/// - "set:cal" -> IFactorySettingsServer
/// - "set:fd"  -> IFirmwareDebugSettingsServer
/// - "set:sys" -> ISystemSettingsServer
pub fn loop_process() {
    // TODO: Wire up to ServerManager
    todo!("Set::LoopProcess");
}
