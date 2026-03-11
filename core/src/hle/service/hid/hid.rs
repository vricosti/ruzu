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
    // TODO: Wire up to ServerManager when service framework is ported.
    // Creates ResourceManager and HidFirmwareSettings, initializes ResourceManager,
    // then registers all services.
    todo!("HID::LoopProcess");
}
