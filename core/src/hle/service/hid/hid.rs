//! Port of zuyu/src/core/hle/service/hid/hid.h and hid.cpp
//!
//! Entry point for the HID service module.

/// Named services registered by the HID module:
/// - "hid"      -> IHidServer
/// - "hid:dbg"  -> IHidDebugServer
/// - "hid:sys"  -> IHidSystemServer
/// - "hidbus"   -> Hidbus
/// - "irs"      -> IRS
/// - "irs:sys"  -> IRS_SYS
/// - "xcd:sys"  -> XCD_SYS
pub fn loop_process() {
    // Upstream creates ResourceManager and HidFirmwareSettings, initializes
    // ResourceManager, then registers: "hid", "hid:dbg", "hid:sys", "hidbus",
    // "irs", "irs:sys", "xcd:sys" with the ServerManager.
    // TODO: Wire up to ServerManager when service framework is ported.
    log::warn!("HID::loop_process: ServerManager not yet ported, services not registered");
}
