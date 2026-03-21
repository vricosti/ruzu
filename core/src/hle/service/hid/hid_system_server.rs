//! Port of zuyu/src/core/hle/service/hid/hid_system_server.h and hid_system_server.cpp
//!
//! IHidSystemServer service ("hid:sys").

use std::collections::BTreeMap;
use std::sync::Arc;

use hid_core::resource_manager::ResourceManager;
use hid_core::resources::hid_firmware_settings::HidFirmwareSettings;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Convert `common::ResultCode` (used by hid_core) to `crate::hle::result::ResultCode`
/// (used by the service IPC layer). Both are u32 newtypes with identical layout.
#[inline]
fn to_ipc_result(r: common::ResultCode) -> ResultCode {
    ResultCode::new(r.raw())
}

/// IHidSystemServer - system-level HID interface.
pub struct IHidSystemServer {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
    firmware_settings: Arc<HidFirmwareSettings>,
}

impl IHidSystemServer {
    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::debug!("(STUBBED) hid:sys command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Upstream: IHidSystemServer::RegisterAppletResourceUserId (cmd 501)
    fn register_applet_resource_user_id(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = unsafe { &*(this as *const dyn ServiceFramework as *const IHidSystemServer) };
        let mut rp = RequestParser::new(ctx);
        let enable_input = rp.pop_bool();
        let _padding = rp.pop_u32();
        let aruid = rp.pop_u64();

        log::info!(
            "RegisterAppletResourceUserId called, enable_input={}, aruid={}",
            enable_input,
            aruid
        );

        let result = server
            .resource_manager
            .lock()
            .register_applet_resource_user_id(aruid, enable_input);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(to_ipc_result(result));
    }

    /// Upstream: IHidSystemServer::UnregisterAppletResourceUserId (cmd 502)
    fn unregister_applet_resource_user_id(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let server = unsafe { &*(this as *const dyn ServiceFramework as *const IHidSystemServer) };
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();

        log::info!(
            "UnregisterAppletResourceUserId called, aruid={}",
            aruid
        );

        server
            .resource_manager
            .lock()
            .unregister_applet_resource_user_id(aruid);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Upstream: IHidSystemServer::EnableAppletToGetInput (cmd 503)
    fn enable_applet_to_get_input(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = unsafe { &*(this as *const dyn ServiceFramework as *const IHidSystemServer) };
        let mut rp = RequestParser::new(ctx);
        let is_enabled = rp.pop_bool();
        let _padding = rp.pop_u32();
        let aruid = rp.pop_u64();

        log::info!(
            "EnableAppletToGetInput called, is_enabled={}, aruid={}",
            is_enabled,
            aruid
        );

        server.resource_manager.lock().enable_input(aruid, is_enabled);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Upstream: IHidSystemServer::SetVibrationMasterVolume (cmd 510)
    fn set_vibration_master_volume(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let master_volume = rp.pop_f32();

        log::info!("SetVibrationMasterVolume called, volume={}", master_volume);

        // TODO: forward to resource_manager->GetNpad()->GetVibrationHandler()->SetVibrationMasterVolume
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Upstream: IHidSystemServer::GetVibrationMasterVolume (cmd 511)
    fn get_vibration_master_volume(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("GetVibrationMasterVolume called");

        // TODO: forward to resource_manager->GetNpad()->GetVibrationHandler()->GetVibrationMasterVolume
        let master_volume: f32 = 1.0;

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_f32(master_volume);
    }

    /// Upstream: IHidSystemServer::BeginPermitVibrationSession (cmd 512)
    fn begin_permit_vibration_session(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();

        log::info!(
            "BeginPermitVibrationSession called, aruid={}",
            aruid
        );

        // TODO: forward to resource_manager->GetNpad()->GetVibrationHandler()->BeginPermitVibrationSession
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Upstream: IHidSystemServer::EndPermitVibrationSession (cmd 513)
    fn end_permit_vibration_session(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("EndPermitVibrationSession called");

        // TODO: forward to resource_manager->GetNpad()->GetVibrationHandler()->EndPermitVibrationSession
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new(
        resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
        firmware_settings: Arc<HidFirmwareSettings>,
    ) -> Self {
        // clang-format off
        let handlers = build_handler_map(&[
            (31, Some(Self::stub_success_handler), "SendKeyboardLockKeyEvent"),
            (101, Some(Self::stub_success_handler), "AcquireHomeButtonEventHandle"),
            (111, Some(Self::stub_success_handler), "ActivateHomeButton"),
            (121, Some(Self::stub_success_handler), "AcquireSleepButtonEventHandle"),
            (131, Some(Self::stub_success_handler), "ActivateSleepButton"),
            (141, Some(Self::stub_success_handler), "AcquireCaptureButtonEventHandle"),
            (151, Some(Self::stub_success_handler), "ActivateCaptureButton"),
            (161, Some(Self::stub_success_handler), "GetPlatformConfig"),
            (210, Some(Self::stub_success_handler), "AcquireNfcDeviceUpdateEventHandle"),
            (211, Some(Self::stub_success_handler), "GetNpadsWithNfc"),
            (212, Some(Self::stub_success_handler), "AcquireNfcActivateEventHandle"),
            (213, Some(Self::stub_success_handler), "ActivateNfc"),
            (214, Some(Self::stub_success_handler), "GetXcdHandleForNpadWithNfc"),
            (215, Some(Self::stub_success_handler), "IsNfcActivated"),
            (230, Some(Self::stub_success_handler), "AcquireIrSensorEventHandle"),
            (231, Some(Self::stub_success_handler), "ActivateIrSensor"),
            (232, Some(Self::stub_success_handler), "GetIrSensorState"),
            (233, Some(Self::stub_success_handler), "GetXcdHandleForNpadWithIrSensor"),
            (301, Some(Self::stub_success_handler), "ActivateNpadSystem"),
            (303, Some(Self::stub_success_handler), "ApplyNpadSystemCommonPolicy"),
            (304, Some(Self::stub_success_handler), "EnableAssigningSingleOnSlSrPress"),
            (305, Some(Self::stub_success_handler), "DisableAssigningSingleOnSlSrPress"),
            (306, Some(Self::stub_success_handler), "GetLastActiveNpad"),
            (307, Some(Self::stub_success_handler), "GetNpadSystemExtStyle"),
            (308, Some(Self::stub_success_handler), "ApplyNpadSystemCommonPolicyFull"),
            (309, Some(Self::stub_success_handler), "GetNpadFullKeyGripColor"),
            (310, Some(Self::stub_success_handler), "GetMaskedSupportedNpadStyleSet"),
            (311, Some(Self::stub_success_handler), "SetNpadPlayerLedBlinkingDevice"),
            (312, Some(Self::stub_success_handler), "SetSupportedNpadStyleSetAll"),
            (313, Some(Self::stub_success_handler), "GetNpadCaptureButtonAssignment"),
            (314, Some(Self::stub_success_handler), "GetAppletFooterUiType"),
            (315, Some(Self::stub_success_handler), "GetAppletDetailedUiType"),
            (316, Some(Self::stub_success_handler), "GetNpadInterfaceType"),
            (317, Some(Self::stub_success_handler), "GetNpadLeftRightInterfaceType"),
            (318, Some(Self::stub_success_handler), "HasBattery"),
            (319, Some(Self::stub_success_handler), "HasLeftRightBattery"),
            (321, Some(Self::stub_success_handler), "GetUniquePadsFromNpad"),
            (322, Some(Self::stub_success_handler), "SetNpadSystemExtStateEnabled"),
            (323, Some(Self::stub_success_handler), "GetLastActiveUniquePad"),
            (324, Some(Self::stub_success_handler), "GetUniquePadButtonSet"),
            (325, Some(Self::stub_success_handler), "GetUniquePadColor"),
            (326, Some(Self::stub_success_handler), "GetUniquePadAppletDetailedUiType"),
            (327, Some(Self::stub_success_handler), "GetAbstractedPadIdDataFromNpad"),
            (328, Some(Self::stub_success_handler), "AttachAbstractedPadToNpad"),
            (329, Some(Self::stub_success_handler), "DetachAbstractedPadAll"),
            (330, Some(Self::stub_success_handler), "CheckAbstractedPadConnection"),
            (500, Some(Self::stub_success_handler), "SetAppletResourceUserId"),
            (501, Some(Self::register_applet_resource_user_id), "RegisterAppletResourceUserId"),
            (502, Some(Self::unregister_applet_resource_user_id), "UnregisterAppletResourceUserId"),
            (503, Some(Self::enable_applet_to_get_input), "EnableAppletToGetInput"),
            (504, Some(Self::stub_success_handler), "SetAruidValidForVibration"),
            (505, Some(Self::stub_success_handler), "EnableAppletToGetSixAxisSensor"),
            (506, Some(Self::stub_success_handler), "EnableAppletToGetPadInput"),
            (507, Some(Self::stub_success_handler), "EnableAppletToGetTouchScreen"),
            (510, Some(Self::set_vibration_master_volume), "SetVibrationMasterVolume"),
            (511, Some(Self::get_vibration_master_volume), "GetVibrationMasterVolume"),
            (512, Some(Self::begin_permit_vibration_session), "BeginPermitVibrationSession"),
            (513, Some(Self::end_permit_vibration_session), "EndPermitVibrationSession"),
            (514, Some(Self::stub_success_handler), "Unknown514"),
            (520, Some(Self::stub_success_handler), "EnableHandheldHids"),
            (521, Some(Self::stub_success_handler), "DisableHandheldHids"),
            (522, Some(Self::stub_success_handler), "SetJoyConRailEnabled"),
            (523, Some(Self::stub_success_handler), "IsJoyConRailEnabled"),
            (524, Some(Self::stub_success_handler), "IsHandheldHidsEnabled"),
            (525, Some(Self::stub_success_handler), "IsJoyConAttachedOnAllRail"),
            (540, Some(Self::stub_success_handler), "AcquirePlayReportControllerUsageUpdateEvent"),
            (541, Some(Self::stub_success_handler), "GetPlayReportControllerUsages"),
            (542, Some(Self::stub_success_handler), "AcquirePlayReportRegisteredDeviceUpdateEvent"),
            (543, Some(Self::stub_success_handler), "GetRegisteredDevicesOld"),
            (544, Some(Self::stub_success_handler), "AcquireConnectionTriggerTimeoutEvent"),
            (545, Some(Self::stub_success_handler), "SendConnectionTrigger"),
            (546, Some(Self::stub_success_handler), "AcquireDeviceRegisteredEventForControllerSupport"),
            (547, Some(Self::stub_success_handler), "GetAllowedBluetoothLinksCount"),
            (548, Some(Self::stub_success_handler), "GetRegisteredDevices"),
            (549, Some(Self::stub_success_handler), "GetConnectableRegisteredDevices"),
            (700, Some(Self::stub_success_handler), "ActivateUniquePad"),
            (702, Some(Self::stub_success_handler), "AcquireUniquePadConnectionEventHandle"),
            (703, Some(Self::stub_success_handler), "GetUniquePadIds"),
            (751, Some(Self::stub_success_handler), "AcquireJoyDetachOnBluetoothOffEventHandle"),
            (800, Some(Self::stub_success_handler), "ListSixAxisSensorHandles"),
            (801, Some(Self::stub_success_handler), "IsSixAxisSensorUserCalibrationSupported"),
            (802, Some(Self::stub_success_handler), "ResetSixAxisSensorCalibrationValues"),
            (803, Some(Self::stub_success_handler), "StartSixAxisSensorUserCalibration"),
            (804, Some(Self::stub_success_handler), "CancelSixAxisSensorUserCalibration"),
            (805, Some(Self::stub_success_handler), "GetUniquePadBluetoothAddress"),
            (806, Some(Self::stub_success_handler), "DisconnectUniquePad"),
            (807, Some(Self::stub_success_handler), "GetUniquePadType"),
            (808, Some(Self::stub_success_handler), "GetUniquePadInterface"),
            (809, Some(Self::stub_success_handler), "GetUniquePadSerialNumber"),
            (810, Some(Self::stub_success_handler), "GetUniquePadControllerNumber"),
            (811, Some(Self::stub_success_handler), "GetSixAxisSensorUserCalibrationStage"),
            (812, Some(Self::stub_success_handler), "GetConsoleUniqueSixAxisSensorHandle"),
            (821, Some(Self::stub_success_handler), "StartAnalogStickManualCalibration"),
            (822, Some(Self::stub_success_handler), "RetryCurrentAnalogStickManualCalibrationStage"),
            (823, Some(Self::stub_success_handler), "CancelAnalogStickManualCalibration"),
            (824, Some(Self::stub_success_handler), "ResetAnalogStickManualCalibration"),
            (825, Some(Self::stub_success_handler), "GetAnalogStickState"),
            (826, Some(Self::stub_success_handler), "GetAnalogStickManualCalibrationStage"),
            (827, Some(Self::stub_success_handler), "IsAnalogStickButtonPressed"),
            (828, Some(Self::stub_success_handler), "IsAnalogStickInReleasePosition"),
            (829, Some(Self::stub_success_handler), "IsAnalogStickInCircumference"),
            (830, Some(Self::stub_success_handler), "SetNotificationLedPattern"),
            (831, Some(Self::stub_success_handler), "SetNotificationLedPatternWithTimeout"),
            (832, Some(Self::stub_success_handler), "PrepareHidsForNotificationWake"),
            (850, Some(Self::stub_success_handler), "IsUsbFullKeyControllerEnabled"),
            (851, Some(Self::stub_success_handler), "EnableUsbFullKeyController"),
            (852, Some(Self::stub_success_handler), "IsUsbConnected"),
            (870, Some(Self::stub_success_handler), "IsHandheldButtonPressedOnConsoleMode"),
            (900, Some(Self::stub_success_handler), "ActivateInputDetector"),
            (901, Some(Self::stub_success_handler), "NotifyInputDetector"),
            (1000, Some(Self::stub_success_handler), "InitializeFirmwareUpdate"),
            (1001, Some(Self::stub_success_handler), "GetFirmwareVersion"),
            (1002, Some(Self::stub_success_handler), "GetAvailableFirmwareVersion"),
            (1003, Some(Self::stub_success_handler), "IsFirmwareUpdateAvailable"),
            (1004, Some(Self::stub_success_handler), "CheckFirmwareUpdateRequired"),
            (1005, Some(Self::stub_success_handler), "StartFirmwareUpdate"),
            (1006, Some(Self::stub_success_handler), "AbortFirmwareUpdate"),
            (1007, Some(Self::stub_success_handler), "GetFirmwareUpdateState"),
            (1008, Some(Self::stub_success_handler), "ActivateAudioControl"),
            (1009, Some(Self::stub_success_handler), "AcquireAudioControlEventHandle"),
            (1010, Some(Self::stub_success_handler), "GetAudioControlStates"),
            (1011, Some(Self::stub_success_handler), "DeactivateAudioControl"),
            (1050, Some(Self::stub_success_handler), "IsSixAxisSensorAccurateUserCalibrationSupported"),
            (1051, Some(Self::stub_success_handler), "StartSixAxisSensorAccurateUserCalibration"),
            (1052, Some(Self::stub_success_handler), "CancelSixAxisSensorAccurateUserCalibration"),
            (1053, Some(Self::stub_success_handler), "GetSixAxisSensorAccurateUserCalibrationState"),
            (1100, Some(Self::stub_success_handler), "GetHidbusSystemServiceObject"),
            (1120, Some(Self::stub_success_handler), "SetFirmwareHotfixUpdateSkipEnabled"),
            (1130, Some(Self::stub_success_handler), "InitializeUsbFirmwareUpdate"),
            (1131, Some(Self::stub_success_handler), "FinalizeUsbFirmwareUpdate"),
            (1132, Some(Self::stub_success_handler), "CheckUsbFirmwareUpdateRequired"),
            (1133, Some(Self::stub_success_handler), "StartUsbFirmwareUpdate"),
            (1134, Some(Self::stub_success_handler), "GetUsbFirmwareUpdateState"),
            (1135, Some(Self::stub_success_handler), "InitializeUsbFirmwareUpdateWithoutMemory"),
            (1150, Some(Self::stub_success_handler), "SetTouchScreenMagnification"),
            (1151, Some(Self::stub_success_handler), "GetTouchScreenFirmwareVersion"),
            (1152, Some(Self::stub_success_handler), "SetTouchScreenDefaultConfiguration"),
            (1153, Some(Self::stub_success_handler), "GetTouchScreenDefaultConfiguration"),
            (1154, Some(Self::stub_success_handler), "IsFirmwareAvailableForNotification"),
            (1155, Some(Self::stub_success_handler), "SetForceHandheldStyleVibration"),
            (1156, Some(Self::stub_success_handler), "SendConnectionTriggerWithoutTimeoutEvent"),
            (1157, Some(Self::stub_success_handler), "CancelConnectionTrigger"),
            (1200, Some(Self::stub_success_handler), "IsButtonConfigSupported"),
            (1201, Some(Self::stub_success_handler), "IsButtonConfigEmbeddedSupported"),
            (1202, Some(Self::stub_success_handler), "DeleteButtonConfig"),
            (1203, Some(Self::stub_success_handler), "DeleteButtonConfigEmbedded"),
            (1204, Some(Self::stub_success_handler), "SetButtonConfigEnabled"),
            (1205, Some(Self::stub_success_handler), "SetButtonConfigEmbeddedEnabled"),
            (1206, Some(Self::stub_success_handler), "IsButtonConfigEnabled"),
            (1207, Some(Self::stub_success_handler), "IsButtonConfigEmbeddedEnabled"),
            (1208, Some(Self::stub_success_handler), "SetButtonConfigEmbedded"),
            (1209, Some(Self::stub_success_handler), "SetButtonConfigFull"),
            (1210, Some(Self::stub_success_handler), "SetButtonConfigLeft"),
            (1211, Some(Self::stub_success_handler), "SetButtonConfigRight"),
            (1212, Some(Self::stub_success_handler), "GetButtonConfigEmbedded"),
            (1213, Some(Self::stub_success_handler), "GetButtonConfigFull"),
            (1214, Some(Self::stub_success_handler), "GetButtonConfigLeft"),
            (1215, Some(Self::stub_success_handler), "GetButtonConfigRight"),
            (1250, Some(Self::stub_success_handler), "IsCustomButtonConfigSupported"),
            (1251, Some(Self::stub_success_handler), "IsDefaultButtonConfigEmbedded"),
            (1252, Some(Self::stub_success_handler), "IsDefaultButtonConfigFull"),
            (1253, Some(Self::stub_success_handler), "IsDefaultButtonConfigLeft"),
            (1254, Some(Self::stub_success_handler), "IsDefaultButtonConfigRight"),
            (1255, Some(Self::stub_success_handler), "IsButtonConfigStorageEmbeddedEmpty"),
            (1256, Some(Self::stub_success_handler), "IsButtonConfigStorageFullEmpty"),
            (1257, Some(Self::stub_success_handler), "IsButtonConfigStorageLeftEmpty"),
            (1258, Some(Self::stub_success_handler), "IsButtonConfigStorageRightEmpty"),
            (1259, Some(Self::stub_success_handler), "GetButtonConfigStorageEmbeddedDeprecated"),
            (1260, Some(Self::stub_success_handler), "GetButtonConfigStorageFullDeprecated"),
            (1261, Some(Self::stub_success_handler), "GetButtonConfigStorageLeftDeprecated"),
            (1262, Some(Self::stub_success_handler), "GetButtonConfigStorageRightDeprecated"),
            (1263, Some(Self::stub_success_handler), "SetButtonConfigStorageEmbeddedDeprecated"),
            (1264, Some(Self::stub_success_handler), "SetButtonConfigStorageFullDeprecated"),
            (1265, Some(Self::stub_success_handler), "SetButtonConfigStorageLeftDeprecated"),
            (1266, Some(Self::stub_success_handler), "SetButtonConfigStorageRightDeprecated"),
            (1267, Some(Self::stub_success_handler), "DeleteButtonConfigStorageEmbedded"),
            (1268, Some(Self::stub_success_handler), "DeleteButtonConfigStorageFull"),
            (1269, Some(Self::stub_success_handler), "DeleteButtonConfigStorageLeft"),
            (1270, Some(Self::stub_success_handler), "DeleteButtonConfigStorageRight"),
            (1271, Some(Self::stub_success_handler), "IsUsingCustomButtonConfig"),
            (1272, Some(Self::stub_success_handler), "IsAnyCustomButtonConfigEnabled"),
            (1273, Some(Self::stub_success_handler), "SetAllCustomButtonConfigEnabled"),
            (1274, Some(Self::stub_success_handler), "SetDefaultButtonConfig"),
            (1275, Some(Self::stub_success_handler), "SetAllDefaultButtonConfig"),
            (1276, Some(Self::stub_success_handler), "SetHidButtonConfigEmbedded"),
            (1277, Some(Self::stub_success_handler), "SetHidButtonConfigFull"),
            (1278, Some(Self::stub_success_handler), "SetHidButtonConfigLeft"),
            (1279, Some(Self::stub_success_handler), "SetHidButtonConfigRight"),
            (1280, Some(Self::stub_success_handler), "GetHidButtonConfigEmbedded"),
            (1281, Some(Self::stub_success_handler), "GetHidButtonConfigFull"),
            (1282, Some(Self::stub_success_handler), "GetHidButtonConfigLeft"),
            (1283, Some(Self::stub_success_handler), "GetHidButtonConfigRight"),
            (1284, Some(Self::stub_success_handler), "GetButtonConfigStorageEmbedded"),
            (1285, Some(Self::stub_success_handler), "GetButtonConfigStorageFull"),
            (1286, Some(Self::stub_success_handler), "GetButtonConfigStorageLeft"),
            (1287, Some(Self::stub_success_handler), "GetButtonConfigStorageRight"),
            (1288, Some(Self::stub_success_handler), "SetButtonConfigStorageEmbedded"),
            (1289, Some(Self::stub_success_handler), "SetButtonConfigStorageFull"),
            (1290, Some(Self::stub_success_handler), "DeleteButtonConfigStorageRight"),
            (1291, Some(Self::stub_success_handler), "DeleteButtonConfigStorageRight"),
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

impl SessionRequestHandler for IHidSystemServer {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "hid:sys"
    }
}

impl ServiceFramework for IHidSystemServer {
    fn get_service_name(&self) -> &str {
        "hid:sys"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
