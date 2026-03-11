//! Port of zuyu/src/core/hle/service/hid/hid_server.h and hid_server.cpp
//!
//! IHidServer service ("hid").

/// IPC command table for IHidServer ("hid"):
///
/// | Cmd  | Name                                           |
/// |------|------------------------------------------------|
/// | 0    | CreateAppletResource                           |
/// | 1    | ActivateDebugPad                               |
/// | 11   | ActivateTouchScreen                            |
/// | 21   | ActivateMouse                                  |
/// | 31   | ActivateKeyboard                               |
/// | 32   | SendKeyboardLockKeyEvent                       |
/// | 40   | AcquireXpadIdEventHandle                       |
/// | 41   | ReleaseXpadIdEventHandle                       |
/// | 51   | ActivateXpad                                   |
/// | 55   | GetXpadIds                                     |
/// | 56   | ActivateJoyXpad                                |
/// | 58   | GetJoyXpadLifoHandle                           |
/// | 59   | GetJoyXpadIds                                  |
/// | 60   | ActivateSixAxisSensor                          |
/// | 61   | DeactivateSixAxisSensor                        |
/// | 62   | GetSixAxisSensorLifoHandle                     |
/// | 63   | ActivateJoySixAxisSensor                       |
/// | 64   | DeactivateJoySixAxisSensor                     |
/// | 65   | GetJoySixAxisSensorLifoHandle                  |
/// | 66   | StartSixAxisSensor                             |
/// | 67   | StopSixAxisSensor                              |
/// | 68   | IsSixAxisSensorFusionEnabled                   |
/// | 69   | EnableSixAxisSensorFusion                      |
/// | 70   | SetSixAxisSensorFusionParameters               |
/// | 71   | GetSixAxisSensorFusionParameters               |
/// | 72   | ResetSixAxisSensorFusionParameters             |
/// | 73   | SetGyroscopeZeroDriftMode                      |
/// | 74   | GetGyroscopeZeroDriftMode                      |
/// | 75   | ResetGyroscopeZeroDriftMode                    |
/// | 76   | IsSixAxisSensorAtRest                          |
/// | 77   | IsFirmwareUpdateAvailableForSixAxisSensor      |
/// | 78   | EnableSixAxisSensorUnalteredPassthrough         |
/// | 79   | IsSixAxisSensorUnalteredPassthroughEnabled      |
/// | 80   | LoadSixAxisSensorCalibrationParameter           |
/// | 82   | GetSixAxisSensorIcInformation                   |
/// | 83   | ResetIsSixAxisSensorDeviceNewlyAssigned          |
/// | 91   | ActivateGesture                                  |
/// | 100  | SetSupportedNpadStyleSet                         |
/// | 101  | GetSupportedNpadStyleSet                         |
/// | 102  | SetSupportedNpadIdType                           |
/// | 103  | ActivateNpad                                     |
/// | 104  | DeactivateNpad                                   |
/// | 106  | AcquireNpadStyleSetUpdateEventHandle             |
/// | 107  | DisconnectNpad                                   |
/// | 108  | GetPlayerLedPattern                              |
/// | 109  | ActivateNpadWithRevision                         |
/// | 120  | SetNpadJoyHoldType                               |
/// | 121  | GetNpadJoyHoldType                               |
/// | 122  | SetNpadJoyAssignmentModeSingleByDefault           |
/// | 123  | SetNpadJoyAssignmentModeSingle                    |
/// | 124  | SetNpadJoyAssignmentModeDual                      |
/// | 125  | MergeSingleJoyAsDualJoy                           |
/// | 126  | StartLrAssignmentMode                             |
/// | 127  | StopLrAssignmentMode                              |
/// | 128  | SetNpadHandheldActivationMode                     |
/// | 129  | GetNpadHandheldActivationMode                     |
/// | 130  | SwapNpadAssignment                                |
/// | 131  | IsUnintendedHomeButtonInputProtectionEnabled       |
/// | 132  | EnableUnintendedHomeButtonInputProtection          |
/// | 133  | SetNpadJoyAssignmentModeSingleWithDestination      |
/// | 134  | SetNpadAnalogStickUseCenterClamp                   |
/// | 135  | SetNpadCaptureButtonAssignment                     |
/// | 136  | ClearNpadCaptureButtonAssignment                   |
/// | 200  | GetVibrationDeviceInfo                             |
/// | 201  | SendVibrationValue                                 |
/// | 202  | GetActualVibrationValue                            |
/// | 203  | CreateActiveVibrationDeviceList                    |
/// | 204  | PermitVibration                                    |
/// | 205  | IsVibrationPermitted                               |
/// | 206  | SendVibrationValues                                |
/// | 207  | SendVibrationGcErmCommand                          |
/// | 208  | GetActualVibrationGcErmCommand                     |
/// | 209  | BeginPermitVibrationSession                        |
/// | 210  | EndPermitVibrationSession                          |
/// | 211  | IsVibrationDeviceMounted                           |
/// | 212  | SendVibrationValueInBool                           |
/// | 300  | ActivateConsoleSixAxisSensor                       |
/// | 301  | StartConsoleSixAxisSensor                          |
/// | 302  | StopConsoleSixAxisSensor                           |
/// | 303  | ActivateSevenSixAxisSensor                         |
/// | 304  | StartSevenSixAxisSensor                            |
/// | 305  | StopSevenSixAxisSensor                             |
/// | 306  | InitializeSevenSixAxisSensor                       |
/// | 307  | FinalizeSevenSixAxisSensor                         |
/// | 308  | ResetSevenSixAxisSensorTimestamp                   |
/// | 400  | IsUsbFullKeyControllerEnabled                      |
/// | 500-541 | (Palma controller commands)                     |
/// | 1000 | SetNpadCommunicationMode                           |
/// | 1001 | GetNpadCommunicationMode                           |
/// | 1002 | SetTouchScreenConfiguration                        |
/// | 1003 | IsFirmwareUpdateNeededForNotification               |
/// | 1004 | SetTouchScreenResolution                            |
pub struct IHidServer {
    // resource_manager: Arc<ResourceManager>,
    // firmware_settings: Arc<HidFirmwareSettings>,
}

impl IHidServer {
    pub fn new() -> Self {
        Self {}
    }
}
