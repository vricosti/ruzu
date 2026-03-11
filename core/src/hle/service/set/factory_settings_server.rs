//! Port of zuyu/src/core/hle/service/set/factory_settings_server.h and .cpp
//!
//! IFactorySettingsServer service ("set:cal").

/// IPC command table for IFactorySettingsServer ("set:cal"):
///
/// | Cmd | Name                                      |
/// |-----|-------------------------------------------|
/// | 0   | GetBluetoothBdAddress                     |
/// | 1   | GetConfigurationId1                       |
/// | 2   | GetAccelerometerOffset                    |
/// | 3   | GetAccelerometerScale                     |
/// | 4   | GetGyroscopeOffset                        |
/// | 5   | GetGyroscopeScale                         |
/// | 6   | GetWirelessLanMacAddress                  |
/// | 7   | GetWirelessLanCountryCodeCount            |
/// | 8   | GetWirelessLanCountryCodes                |
/// | 9   | GetSerialNumber                           |
/// | 10  | SetInitialSystemAppletProgramId           |
/// | 11  | SetOverlayDispProgramId                   |
/// | 12  | GetBatteryLot                             |
/// | 14  | GetEciDeviceCertificate                   |
/// | 15  | GetEticketDeviceCertificate               |
/// | 16  | GetSslKey                                 |
/// | 17  | GetSslCertificate                         |
/// | 18  | GetGameCardKey                            |
/// | 19  | GetGameCardCertificate                    |
/// | 20  | GetEciDeviceKey                           |
/// | 21  | GetEticketDeviceKey                       |
/// | 22  | GetSpeakerParameter                       |
/// | 23  | GetLcdVendorId                            |
/// | 24  | GetEciDeviceCertificate2                  |
/// | 25  | GetEciDeviceKey2                          |
/// | 26  | GetAmiiboKey                              |
/// | 27  | GetAmiiboEcqvCertificate                  |
/// | 28  | GetAmiiboEcdsaCertificate                 |
/// | 29  | GetAmiiboEcqvBlsKey                       |
/// | 30  | GetAmiiboEcqvBlsCertificate               |
/// | 31  | GetAmiiboEcqvBlsRootCertificate           |
/// | 32  | GetUsbTypeCPowerSourceCircuitVersion      |
/// | 33  | GetAnalogStickModuleTypeL                 |
/// | 34  | GetAnalogStickModelParameterL             |
/// | 35  | GetAnalogStickFactoryCalibrationL         |
/// | 36  | GetAnalogStickModuleTypeR                 |
/// | 37  | GetAnalogStickModelParameterR             |
/// | 38  | GetAnalogStickFactoryCalibrationR         |
/// | 39  | GetConsoleSixAxisSensorModuleType         |
/// | 40  | GetConsoleSixAxisSensorHorizontalOffset   |
/// | 41  | GetBatteryVersion                         |
/// | 42  | GetDeviceId                               |
/// | 43  | GetConsoleSixAxisSensorMountType          |
pub struct IFactorySettingsServer;

impl IFactorySettingsServer {
    pub fn new() -> Self {
        Self
    }
}
