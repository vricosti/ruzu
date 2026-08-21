//! Port of zuyu/src/core/hle/service/hid/hid_debug_server.h and hid_debug_server.cpp
//!
//! IHidDebugServer service ("hid:dbg").

use std::collections::BTreeMap;
use std::sync::Arc;

use hid_core::resource_manager::ResourceManager;
use hid_core::resources::hid_firmware_settings::HidFirmwareSettings;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IHidDebugServer - debug interface for HID.
pub struct IHidDebugServer {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
    firmware_settings: Arc<HidFirmwareSettings>,
}

impl IHidDebugServer {
    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::debug!("(STUBBED) hid:dbg command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new(
        resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
        firmware_settings: Arc<HidFirmwareSettings>,
    ) -> Self {
        // clang-format off
        let handlers = build_handler_map(&[
            (0, Some(Self::stub_success_handler), "DeactivateDebugPad"),
            (
                1,
                Some(Self::stub_success_handler),
                "SetDebugPadAutoPilotState",
            ),
            (
                2,
                Some(Self::stub_success_handler),
                "UnsetDebugPadAutoPilotState",
            ),
            (
                10,
                Some(Self::stub_success_handler),
                "DeactivateTouchScreen",
            ),
            (
                11,
                Some(Self::stub_success_handler),
                "SetTouchScreenAutoPilotState",
            ),
            (
                12,
                Some(Self::stub_success_handler),
                "UnsetTouchScreenAutoPilotState",
            ),
            (
                13,
                Some(Self::stub_success_handler),
                "GetTouchScreenConfiguration",
            ),
            (
                14,
                Some(Self::stub_success_handler),
                "ProcessTouchScreenAutoTune",
            ),
            (
                15,
                Some(Self::stub_success_handler),
                "ForceStopTouchScreenManagement",
            ),
            (
                16,
                Some(Self::stub_success_handler),
                "ForceRestartTouchScreenManagement",
            ),
            (17, Some(Self::stub_success_handler), "IsTouchScreenManaged"),
            (20, Some(Self::stub_success_handler), "DeactivateMouse"),
            (
                21,
                Some(Self::stub_success_handler),
                "SetMouseAutoPilotState",
            ),
            (
                22,
                Some(Self::stub_success_handler),
                "UnsetMouseAutoPilotState",
            ),
            (
                25,
                Some(Self::stub_success_handler),
                "SetDebugMouseAutoPilotState",
            ),
            (
                26,
                Some(Self::stub_success_handler),
                "UnsetDebugMouseAutoPilotState",
            ),
            (30, Some(Self::stub_success_handler), "DeactivateKeyboard"),
            (
                31,
                Some(Self::stub_success_handler),
                "SetKeyboardAutoPilotState",
            ),
            (
                32,
                Some(Self::stub_success_handler),
                "UnsetKeyboardAutoPilotState",
            ),
            (50, Some(Self::stub_success_handler), "DeactivateXpad"),
            (
                51,
                Some(Self::stub_success_handler),
                "SetXpadAutoPilotState",
            ),
            (
                52,
                Some(Self::stub_success_handler),
                "UnsetXpadAutoPilotState",
            ),
            (53, Some(Self::stub_success_handler), "DeactivateJoyXpad"),
            (
                60,
                Some(Self::stub_success_handler),
                "ClearNpadSystemCommonPolicy",
            ),
            (61, Some(Self::stub_success_handler), "DeactivateNpad"),
            (62, Some(Self::stub_success_handler), "ForceDisconnectNpad"),
            (91, Some(Self::stub_success_handler), "DeactivateGesture"),
            (
                110,
                Some(Self::stub_success_handler),
                "DeactivateHomeButton",
            ),
            (
                111,
                Some(Self::stub_success_handler),
                "SetHomeButtonAutoPilotState",
            ),
            (
                112,
                Some(Self::stub_success_handler),
                "UnsetHomeButtonAutoPilotState",
            ),
            (
                120,
                Some(Self::stub_success_handler),
                "DeactivateSleepButton",
            ),
            (
                121,
                Some(Self::stub_success_handler),
                "SetSleepButtonAutoPilotState",
            ),
            (
                122,
                Some(Self::stub_success_handler),
                "UnsetSleepButtonAutoPilotState",
            ),
            (
                123,
                Some(Self::stub_success_handler),
                "DeactivateInputDetector",
            ),
            (
                130,
                Some(Self::stub_success_handler),
                "DeactivateCaptureButton",
            ),
            (
                131,
                Some(Self::stub_success_handler),
                "SetCaptureButtonAutoPilotState",
            ),
            (
                132,
                Some(Self::stub_success_handler),
                "UnsetCaptureButtonAutoPilotState",
            ),
            (
                133,
                Some(Self::stub_success_handler),
                "SetShiftAccelerometerCalibrationValue",
            ),
            (
                134,
                Some(Self::stub_success_handler),
                "GetShiftAccelerometerCalibrationValue",
            ),
            (
                135,
                Some(Self::stub_success_handler),
                "SetShiftGyroscopeCalibrationValue",
            ),
            (
                136,
                Some(Self::stub_success_handler),
                "GetShiftGyroscopeCalibrationValue",
            ),
            (
                140,
                Some(Self::stub_success_handler),
                "DeactivateConsoleSixAxisSensor",
            ),
            (
                141,
                Some(Self::stub_success_handler),
                "GetConsoleSixAxisSensorSamplingFrequency",
            ),
            (
                142,
                Some(Self::stub_success_handler),
                "DeactivateSevenSixAxisSensor",
            ),
            (
                143,
                Some(Self::stub_success_handler),
                "GetConsoleSixAxisSensorCountStates",
            ),
            (144, Some(Self::stub_success_handler), "GetAccelerometerFsr"),
            (145, Some(Self::stub_success_handler), "SetAccelerometerFsr"),
            (146, Some(Self::stub_success_handler), "GetAccelerometerOdr"),
            (147, Some(Self::stub_success_handler), "SetAccelerometerOdr"),
            (148, Some(Self::stub_success_handler), "GetGyroscopeFsr"),
            (149, Some(Self::stub_success_handler), "SetGyroscopeFsr"),
            (150, Some(Self::stub_success_handler), "GetGyroscopeOdr"),
            (151, Some(Self::stub_success_handler), "SetGyroscopeOdr"),
            (152, Some(Self::stub_success_handler), "GetWhoAmI"),
            (
                201,
                Some(Self::stub_success_handler),
                "ActivateFirmwareUpdate",
            ),
            (
                202,
                Some(Self::stub_success_handler),
                "DeactivateFirmwareUpdate",
            ),
            (203, Some(Self::stub_success_handler), "StartFirmwareUpdate"),
            (
                204,
                Some(Self::stub_success_handler),
                "GetFirmwareUpdateStage",
            ),
            (205, Some(Self::stub_success_handler), "GetFirmwareVersion"),
            (
                206,
                Some(Self::stub_success_handler),
                "GetDestinationFirmwareVersion",
            ),
            (
                207,
                Some(Self::stub_success_handler),
                "DiscardFirmwareInfoCacheForRevert",
            ),
            (
                208,
                Some(Self::stub_success_handler),
                "StartFirmwareUpdateForRevert",
            ),
            (
                209,
                Some(Self::stub_success_handler),
                "GetAvailableFirmwareVersionForRevert",
            ),
            (
                210,
                Some(Self::stub_success_handler),
                "IsFirmwareUpdatingDevice",
            ),
            (
                211,
                Some(Self::stub_success_handler),
                "StartFirmwareUpdateIndividual",
            ),
            (
                215,
                Some(Self::stub_success_handler),
                "SetUsbFirmwareForceUpdateEnabled",
            ),
            (
                216,
                Some(Self::stub_success_handler),
                "SetAllKuinaDevicesToFirmwareUpdateMode",
            ),
            (
                221,
                Some(Self::stub_success_handler),
                "UpdateControllerColor",
            ),
            (222, Some(Self::stub_success_handler), "ConnectUsbPadsAsync"),
            (
                223,
                Some(Self::stub_success_handler),
                "DisconnectUsbPadsAsync",
            ),
            (224, Some(Self::stub_success_handler), "UpdateDesignInfo"),
            (
                225,
                Some(Self::stub_success_handler),
                "GetUniquePadDriverState",
            ),
            (
                226,
                Some(Self::stub_success_handler),
                "GetSixAxisSensorDriverStates",
            ),
            (227, Some(Self::stub_success_handler), "GetRxPacketHistory"),
            (
                228,
                Some(Self::stub_success_handler),
                "AcquireOperationEventHandle",
            ),
            (229, Some(Self::stub_success_handler), "ReadSerialFlash"),
            (230, Some(Self::stub_success_handler), "WriteSerialFlash"),
            (231, Some(Self::stub_success_handler), "GetOperationResult"),
            (232, Some(Self::stub_success_handler), "EnableShipmentMode"),
            (233, Some(Self::stub_success_handler), "ClearPairingInfo"),
            (
                234,
                Some(Self::stub_success_handler),
                "GetUniquePadDeviceTypeSetInternal",
            ),
            (
                235,
                Some(Self::stub_success_handler),
                "EnableAnalogStickPower",
            ),
            (
                236,
                Some(Self::stub_success_handler),
                "RequestKuinaUartClockCal",
            ),
            (
                237,
                Some(Self::stub_success_handler),
                "GetKuinaUartClockCal",
            ),
            (
                238,
                Some(Self::stub_success_handler),
                "SetKuinaUartClockTrim",
            ),
            (239, Some(Self::stub_success_handler), "KuinaLoopbackTest"),
            (
                240,
                Some(Self::stub_success_handler),
                "RequestBatteryVoltage",
            ),
            (241, Some(Self::stub_success_handler), "GetBatteryVoltage"),
            (
                242,
                Some(Self::stub_success_handler),
                "GetUniquePadPowerInfo",
            ),
            (243, Some(Self::stub_success_handler), "RebootUniquePad"),
            (
                244,
                Some(Self::stub_success_handler),
                "RequestKuinaFirmwareVersion",
            ),
            (
                245,
                Some(Self::stub_success_handler),
                "GetKuinaFirmwareVersion",
            ),
            (246, Some(Self::stub_success_handler), "GetVidPid"),
            (
                247,
                Some(Self::stub_success_handler),
                "GetAnalogStickCalibrationValue",
            ),
            (248, Some(Self::stub_success_handler), "GetUniquePadIdsFull"),
            (249, Some(Self::stub_success_handler), "ConnectUniquePad"),
            (250, Some(Self::stub_success_handler), "IsVirtual"),
            (
                251,
                Some(Self::stub_success_handler),
                "GetAnalogStickModuleParam",
            ),
            (
                301,
                Some(Self::stub_success_handler),
                "GetAbstractedPadHandles",
            ),
            (
                302,
                Some(Self::stub_success_handler),
                "GetAbstractedPadState",
            ),
            (
                303,
                Some(Self::stub_success_handler),
                "GetAbstractedPadsState",
            ),
            (
                321,
                Some(Self::stub_success_handler),
                "SetAutoPilotVirtualPadState",
            ),
            (
                322,
                Some(Self::stub_success_handler),
                "UnsetAutoPilotVirtualPadState",
            ),
            (
                323,
                Some(Self::stub_success_handler),
                "UnsetAllAutoPilotVirtualPadState",
            ),
            (
                324,
                Some(Self::stub_success_handler),
                "AttachHdlsWorkBuffer",
            ),
            (
                325,
                Some(Self::stub_success_handler),
                "ReleaseHdlsWorkBuffer",
            ),
            (
                326,
                Some(Self::stub_success_handler),
                "DumpHdlsNpadAssignmentState",
            ),
            (327, Some(Self::stub_success_handler), "DumpHdlsStates"),
            (
                328,
                Some(Self::stub_success_handler),
                "ApplyHdlsNpadAssignmentState",
            ),
            (329, Some(Self::stub_success_handler), "ApplyHdlsStateList"),
            (
                330,
                Some(Self::stub_success_handler),
                "AttachHdlsVirtualDevice",
            ),
            (
                331,
                Some(Self::stub_success_handler),
                "DetachHdlsVirtualDevice",
            ),
            (332, Some(Self::stub_success_handler), "SetHdlsState"),
            (350, Some(Self::stub_success_handler), "AddRegisteredDevice"),
            (
                400,
                Some(Self::stub_success_handler),
                "DisableExternalMcuOnNxDevice",
            ),
            (
                401,
                Some(Self::stub_success_handler),
                "DisableRailDeviceFiltering",
            ),
            (402, Some(Self::stub_success_handler), "EnableWiredPairing"),
            (
                403,
                Some(Self::stub_success_handler),
                "EnableShipmentModeAutoClear",
            ),
            (404, Some(Self::stub_success_handler), "SetRailEnabled"),
            (500, Some(Self::stub_success_handler), "SetFactoryInt"),
            (
                501,
                Some(Self::stub_success_handler),
                "IsFactoryBootEnabled",
            ),
            (
                550,
                Some(Self::stub_success_handler),
                "SetAnalogStickModelDataTemporarily",
            ),
            (
                551,
                Some(Self::stub_success_handler),
                "GetAnalogStickModelData",
            ),
            (
                552,
                Some(Self::stub_success_handler),
                "ResetAnalogStickModelData",
            ),
            (600, Some(Self::stub_success_handler), "ConvertPadState"),
            (650, Some(Self::stub_success_handler), "AddButtonPlayData"),
            (651, Some(Self::stub_success_handler), "StartButtonPlayData"),
            (652, Some(Self::stub_success_handler), "StopButtonPlayData"),
            (
                2000,
                Some(Self::stub_success_handler),
                "DeactivateDigitizer",
            ),
            (
                2001,
                Some(Self::stub_success_handler),
                "SetDigitizerAutoPilotState",
            ),
            (
                2002,
                Some(Self::stub_success_handler),
                "UnsetDigitizerAutoPilotState",
            ),
            (
                2002,
                Some(Self::stub_success_handler),
                "ReloadFirmwareDebugSettings",
            ),
        ]);
        // clang-format on

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            resource_manager,
            firmware_settings,
        }
    }
}

impl SessionRequestHandler for IHidDebugServer {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "hid:dbg"
    }
}

impl ServiceFramework for IHidDebugServer {
    fn get_service_name(&self) -> &str {
        "hid:dbg"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
