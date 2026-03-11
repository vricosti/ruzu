//! Port of zuyu/src/core/hle/service/hid/active_vibration_device_list.h and .cpp
//!
//! IActiveVibrationDeviceList service.

/// IPC command table for IActiveVibrationDeviceList:
///
/// | Cmd | Name                      |
/// |-----|---------------------------|
/// | 0   | ActivateVibrationDevice   |
pub struct IActiveVibrationDeviceList;

impl IActiveVibrationDeviceList {
    pub fn new() -> Self {
        Self
    }
}
