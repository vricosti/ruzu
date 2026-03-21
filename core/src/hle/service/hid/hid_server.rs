//! Port of zuyu/src/core/hle/service/hid/hid_server.h and hid_server.cpp
//!
//! IHidServer service ("hid").

use std::collections::BTreeMap;
use std::sync::Arc;

use hid_core::hid_types::*;
use hid_core::resource_manager::ResourceManager;
use hid_core::resources::hid_firmware_settings::HidFirmwareSettings;
use hid_core::resources::npad::npad_types::*;

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

pub struct IHidServer {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
    firmware_settings: Arc<HidFirmwareSettings>,
}

impl IHidServer {
    pub fn new(
        resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
        firmware_settings: Arc<HidFirmwareSettings>,
    ) -> Self {
        // clang-format off
        let handlers = build_handler_map(&[
            (0, Some(Self::create_applet_resource), "CreateAppletResource"),
            (1, Some(Self::activate_debug_pad), "ActivateDebugPad"),
            (11, Some(Self::activate_touch_screen), "ActivateTouchScreen"),
            (21, Some(Self::activate_mouse), "ActivateMouse"),
            (26, Some(Self::stub_success_handler), "ActivateDebugMouse"),
            (31, Some(Self::activate_keyboard), "ActivateKeyboard"),
            (32, Some(Self::stub_success_handler), "SendKeyboardLockKeyEvent"),
            (40, Some(Self::stub_success_handler), "AcquireXpadIdEventHandle"),
            (41, Some(Self::stub_success_handler), "ReleaseXpadIdEventHandle"),
            (51, Some(Self::stub_success_handler), "ActivateXpad"),
            (55, Some(Self::stub_success_handler), "GetXpadIds"),
            (56, Some(Self::stub_success_handler), "ActivateJoyXpad"),
            (58, Some(Self::stub_success_handler), "GetJoyXpadLifoHandle"),
            (59, Some(Self::stub_success_handler), "GetJoyXpadIds"),
            (60, Some(Self::stub_success_handler), "ActivateSixAxisSensor"),
            (61, Some(Self::stub_success_handler), "DeactivateSixAxisSensor"),
            (62, Some(Self::stub_success_handler), "GetSixAxisSensorLifoHandle"),
            (63, Some(Self::stub_success_handler), "ActivateJoySixAxisSensor"),
            (64, Some(Self::stub_success_handler), "DeactivateJoySixAxisSensor"),
            (65, Some(Self::stub_success_handler), "GetJoySixAxisSensorLifoHandle"),
            (66, Some(Self::stub_success_handler), "StartSixAxisSensor"),
            (67, Some(Self::stub_success_handler), "StopSixAxisSensor"),
            (68, Some(Self::stub_success_handler), "IsSixAxisSensorFusionEnabled"),
            (69, Some(Self::stub_success_handler), "EnableSixAxisSensorFusion"),
            (70, Some(Self::stub_success_handler), "SetSixAxisSensorFusionParameters"),
            (71, Some(Self::stub_success_handler), "GetSixAxisSensorFusionParameters"),
            (72, Some(Self::stub_success_handler), "ResetSixAxisSensorFusionParameters"),
            (73, Some(Self::stub_success_handler), "SetAccelerometerParameters"),
            (74, Some(Self::stub_success_handler), "GetAccelerometerParameters"),
            (75, Some(Self::stub_success_handler), "ResetAccelerometerParameters"),
            (76, Some(Self::stub_success_handler), "SetAccelerometerPlayMode"),
            (77, Some(Self::stub_success_handler), "GetAccelerometerPlayMode"),
            (78, Some(Self::stub_success_handler), "ResetAccelerometerPlayMode"),
            (79, Some(Self::stub_success_handler), "SetGyroscopeZeroDriftMode"),
            (80, Some(Self::stub_success_handler), "GetGyroscopeZeroDriftMode"),
            (81, Some(Self::stub_success_handler), "ResetGyroscopeZeroDriftMode"),
            (82, Some(Self::stub_success_handler), "IsSixAxisSensorAtRest"),
            (83, Some(Self::stub_success_handler), "IsFirmwareUpdateAvailableForSixAxisSensor"),
            (84, Some(Self::stub_success_handler), "EnableSixAxisSensorUnalteredPassthrough"),
            (85, Some(Self::stub_success_handler), "IsSixAxisSensorUnalteredPassthroughEnabled"),
            (86, Some(Self::stub_success_handler), "StoreSixAxisSensorCalibrationParameter"),
            (87, Some(Self::stub_success_handler), "LoadSixAxisSensorCalibrationParameter"),
            (88, Some(Self::stub_success_handler), "GetSixAxisSensorIcInformation"),
            (89, Some(Self::stub_success_handler), "ResetIsSixAxisSensorDeviceNewlyAssigned"),
            (91, Some(Self::stub_success_handler), "ActivateGesture"),
            (100, Some(Self::set_supported_npad_style_set), "SetSupportedNpadStyleSet"),
            (101, Some(Self::get_supported_npad_style_set), "GetSupportedNpadStyleSet"),
            (102, Some(Self::set_supported_npad_id_type), "SetSupportedNpadIdType"),
            (103, Some(Self::activate_npad), "ActivateNpad"),
            (104, Some(Self::stub_success_handler), "DeactivateNpad"),
            (106, Some(Self::stub_success_handler), "AcquireNpadStyleSetUpdateEventHandle"),
            (107, Some(Self::stub_success_handler), "DisconnectNpad"),
            (108, Some(Self::get_player_led_pattern), "GetPlayerLedPattern"),
            (109, Some(Self::activate_npad_with_revision), "ActivateNpadWithRevision"),
            (120, Some(Self::set_npad_joy_hold_type), "SetNpadJoyHoldType"),
            (121, Some(Self::get_npad_joy_hold_type), "GetNpadJoyHoldType"),
            (122, Some(Self::stub_success_handler), "SetNpadJoyAssignmentModeSingleByDefault"),
            (123, Some(Self::stub_success_handler), "SetNpadJoyAssignmentModeSingle"),
            (124, Some(Self::stub_success_handler), "SetNpadJoyAssignmentModeDual"),
            (125, Some(Self::stub_success_handler), "MergeSingleJoyAsDualJoy"),
            (126, Some(Self::stub_success_handler), "StartLrAssignmentMode"),
            (127, Some(Self::stub_success_handler), "StopLrAssignmentMode"),
            (128, Some(Self::stub_success_handler), "SetNpadHandheldActivationMode"),
            (129, Some(Self::stub_success_handler), "GetNpadHandheldActivationMode"),
            (130, Some(Self::stub_success_handler), "SwapNpadAssignment"),
            (131, Some(Self::stub_success_handler), "IsUnintendedHomeButtonInputProtectionEnabled"),
            (132, Some(Self::stub_success_handler), "EnableUnintendedHomeButtonInputProtection"),
            (133, Some(Self::stub_success_handler), "SetNpadJoyAssignmentModeSingleWithDestination"),
            (134, Some(Self::stub_success_handler), "SetNpadAnalogStickUseCenterClamp"),
            (135, Some(Self::stub_success_handler), "SetNpadCaptureButtonAssignment"),
            (136, Some(Self::stub_success_handler), "ClearNpadCaptureButtonAssignment"),
            (200, Some(Self::get_vibration_device_info), "GetVibrationDeviceInfo"),
            (201, Some(Self::send_vibration_value), "SendVibrationValue"),
            (202, Some(Self::stub_success_handler), "GetActualVibrationValue"),
            (203, Some(Self::create_active_vibration_device_list), "CreateActiveVibrationDeviceList"),
            (204, Some(Self::permit_vibration), "PermitVibration"),
            (205, Some(Self::is_vibration_permitted), "IsVibrationPermitted"),
            (206, Some(Self::send_vibration_values), "SendVibrationValues"),
            (207, Some(Self::stub_success_handler), "SendVibrationGcErmCommand"),
            (208, Some(Self::stub_success_handler), "GetActualVibrationGcErmCommand"),
            (209, Some(Self::stub_success_handler), "BeginPermitVibrationSession"),
            (210, Some(Self::stub_success_handler), "EndPermitVibrationSession"),
            (211, Some(Self::stub_success_handler), "IsVibrationDeviceMounted"),
            (212, Some(Self::stub_success_handler), "SendVibrationValueInBool"),
            (300, Some(Self::stub_success_handler), "ActivateConsoleSixAxisSensor"),
            (301, Some(Self::stub_success_handler), "StartConsoleSixAxisSensor"),
            (302, Some(Self::stub_success_handler), "StopConsoleSixAxisSensor"),
            (303, Some(Self::stub_success_handler), "ActivateSevenSixAxisSensor"),
            (304, Some(Self::stub_success_handler), "StartSevenSixAxisSensor"),
            (305, Some(Self::stub_success_handler), "StopSevenSixAxisSensor"),
            (306, Some(Self::stub_success_handler), "InitializeSevenSixAxisSensor"),
            (307, Some(Self::stub_success_handler), "FinalizeSevenSixAxisSensor"),
            (308, Some(Self::stub_success_handler), "SetSevenSixAxisSensorFusionStrength"),
            (309, Some(Self::stub_success_handler), "GetSevenSixAxisSensorFusionStrength"),
            (310, Some(Self::stub_success_handler), "ResetSevenSixAxisSensorTimestamp"),
            (400, Some(Self::stub_success_handler), "IsUsbFullKeyControllerEnabled"),
            (401, Some(Self::stub_success_handler), "EnableUsbFullKeyController"),
            (402, Some(Self::stub_success_handler), "IsUsbFullKeyControllerConnected"),
            (403, Some(Self::stub_success_handler), "HasBattery"),
            (404, Some(Self::stub_success_handler), "HasLeftRightBattery"),
            (405, Some(Self::stub_success_handler), "GetNpadInterfaceType"),
            (406, Some(Self::stub_success_handler), "GetNpadLeftRightInterfaceType"),
            (407, Some(Self::stub_success_handler), "GetNpadOfHighestBatteryLevel"),
            (408, Some(Self::stub_success_handler), "GetNpadOfHighestBatteryLevelForJoyRight"),
            (500, Some(Self::stub_success_handler), "GetPalmaConnectionHandle"),
            (501, Some(Self::stub_success_handler), "InitializePalma"),
            (502, Some(Self::stub_success_handler), "AcquirePalmaOperationCompleteEvent"),
            (503, Some(Self::stub_success_handler), "GetPalmaOperationInfo"),
            (504, Some(Self::stub_success_handler), "PlayPalmaActivity"),
            (505, Some(Self::stub_success_handler), "SetPalmaFrModeType"),
            (506, Some(Self::stub_success_handler), "ReadPalmaStep"),
            (507, Some(Self::stub_success_handler), "EnablePalmaStep"),
            (508, Some(Self::stub_success_handler), "ResetPalmaStep"),
            (509, Some(Self::stub_success_handler), "ReadPalmaApplicationSection"),
            (510, Some(Self::stub_success_handler), "WritePalmaApplicationSection"),
            (511, Some(Self::stub_success_handler), "ReadPalmaUniqueCode"),
            (512, Some(Self::stub_success_handler), "SetPalmaUniqueCodeInvalid"),
            (513, Some(Self::stub_success_handler), "WritePalmaActivityEntry"),
            (514, Some(Self::stub_success_handler), "WritePalmaRgbLedPatternEntry"),
            (515, Some(Self::stub_success_handler), "WritePalmaWaveEntry"),
            (516, Some(Self::stub_success_handler), "SetPalmaDataBaseIdentificationVersion"),
            (517, Some(Self::stub_success_handler), "GetPalmaDataBaseIdentificationVersion"),
            (518, Some(Self::stub_success_handler), "SuspendPalmaFeature"),
            (519, Some(Self::stub_success_handler), "GetPalmaOperationResult"),
            (520, Some(Self::stub_success_handler), "ReadPalmaPlayLog"),
            (521, Some(Self::stub_success_handler), "ResetPalmaPlayLog"),
            (522, Some(Self::stub_success_handler), "SetIsPalmaAllConnectable"),
            (523, Some(Self::stub_success_handler), "SetIsPalmaPairedConnectable"),
            (524, Some(Self::stub_success_handler), "PairPalma"),
            (525, Some(Self::stub_success_handler), "SetPalmaBoostMode"),
            (526, Some(Self::stub_success_handler), "CancelWritePalmaWaveEntry"),
            (527, Some(Self::stub_success_handler), "EnablePalmaBoostMode"),
            (528, Some(Self::stub_success_handler), "GetPalmaBluetoothAddress"),
            (529, Some(Self::stub_success_handler), "SetDisallowedPalmaConnection"),
            (1000, Some(Self::stub_success_handler), "SetNpadCommunicationMode"),
            (1001, Some(Self::stub_success_handler), "GetNpadCommunicationMode"),
            (1002, Some(Self::stub_success_handler), "SetTouchScreenConfiguration"),
            (1003, Some(Self::stub_success_handler), "IsFirmwareUpdateNeededForNotification"),
            (1004, Some(Self::stub_success_handler), "SetTouchScreenResolution"),
            (2000, Some(Self::stub_success_handler), "ActivateDigitizer"),
        ]);
        // clang-format on

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            resource_manager,
            firmware_settings,
        }
    }

    /// Downcast helper: recovers `&IHidServer` from `&dyn ServiceFramework`.
    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("(STUBBED) HID command {}", ctx.get_command());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    // cmd 0: CreateAppletResource
    fn create_applet_resource(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();

        let result = server.resource_manager.lock().create_applet_resource(aruid);
        log::debug!("IHidServer::CreateAppletResource called, aruid={}, result=0x{:X}", aruid, result.raw());

        let applet_resource: Arc<dyn SessionRequestHandler> =
            Arc::new(super::applet_resource::IAppletResource::new(
                server.resource_manager.clone(), aruid,
            ));

        let is_domain = ctx.get_manager().map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain { 0 } else {
            ctx.create_session_for_service(applet_resource.clone()).unwrap_or(0)
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(applet_resource);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    // cmd 1: ActivateDebugPad
    fn activate_debug_pad(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::ActivateDebugPad called, aruid={}", aruid);

        let rm = server.resource_manager.lock();
        if !server.firmware_settings.is_device_managed() {
            if let Some(ref debug_pad) = rm.get_debug_pad() {
                let (result, _) = debug_pad.lock().activation.activate();
                if result.is_error() {
                    let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                    rb.push_result(to_ipc_result(result));
                    return;
                }
            }
        }
        let result = if let Some(ref debug_pad) = rm.get_debug_pad() {
            let (r, _) = debug_pad.lock().activation.activate_with_aruid(aruid);
            to_ipc_result(r)
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 11: ActivateTouchScreen
    fn activate_touch_screen(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::ActivateTouchScreen called, aruid={}", aruid);
        // TouchScreen activation requires TouchResource/TouchScreenDriver wiring.
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    // cmd 21: ActivateMouse
    fn activate_mouse(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::ActivateMouse called, aruid={}", aruid);

        let rm = server.resource_manager.lock();
        if !server.firmware_settings.is_device_managed() {
            if let Some(ref mouse) = rm.get_mouse() {
                let (result, _) = mouse.lock().activation.activate();
                if result.is_error() {
                    let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                    rb.push_result(to_ipc_result(result));
                    return;
                }
            }
        }
        let result = if let Some(ref mouse) = rm.get_mouse() {
            let (r, _) = mouse.lock().activation.activate_with_aruid(aruid);
            to_ipc_result(r)
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 31: ActivateKeyboard
    fn activate_keyboard(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::ActivateKeyboard called, aruid={}", aruid);

        let rm = server.resource_manager.lock();
        if !server.firmware_settings.is_device_managed() {
            if let Some(ref keyboard) = rm.get_keyboard() {
                let (result, _) = keyboard.lock().activation.activate();
                if result.is_error() {
                    let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                    rb.push_result(to_ipc_result(result));
                    return;
                }
            }
        }
        let result = if let Some(ref keyboard) = rm.get_keyboard() {
            let (r, _) = keyboard.lock().activation.activate_with_aruid(aruid);
            to_ipc_result(r)
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 100: SetSupportedNpadStyleSet
    fn set_supported_npad_style_set(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let supported_style_set_raw = rp.pop_u32();
        let _padding = rp.pop_u32();
        let aruid = rp.pop_u64();
        let supported_style_set = NpadStyleSet::from_bits_truncate(supported_style_set_raw);
        log::debug!("IHidServer::SetSupportedNpadStyleSet called, style_set={:?}, aruid={}", supported_style_set, aruid);

        let rm = server.resource_manager.lock();
        let result = if let Some(ref npad) = rm.get_npad() {
            let r = npad.lock().set_supported_npad_style_set(aruid, supported_style_set);
            if r.is_error() { to_ipc_result(r) } else { RESULT_SUCCESS }
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 101: GetSupportedNpadStyleSet
    fn get_supported_npad_style_set(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::GetSupportedNpadStyleSet called, aruid={}", aruid);

        let rm = server.resource_manager.lock();
        let (result, style_set) = if let Some(ref npad) = rm.get_npad() {
            match npad.lock().get_supported_npad_style_set(aruid) {
                Ok(ss) => (RESULT_SUCCESS, ss),
                Err(e) => (to_ipc_result(e), NpadStyleSet::NONE),
            }
        } else { (RESULT_SUCCESS, NpadStyleSet::NONE) };

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(result);
        rb.push_u32(style_set.bits());
    }

    // cmd 102: SetSupportedNpadIdType
    fn set_supported_npad_id_type(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::SetSupportedNpadIdType called, aruid={}", aruid);

        let buffer = ctx.read_buffer(0);
        let npad_count = buffer.len() / core::mem::size_of::<u32>();
        let mut npad_ids = Vec::with_capacity(npad_count);
        for i in 0..npad_count {
            let offset = i * 4;
            if offset + 4 <= buffer.len() {
                let raw = u32::from_le_bytes([buffer[offset], buffer[offset+1], buffer[offset+2], buffer[offset+3]]);
                let npad_id: NpadIdType = unsafe { core::mem::transmute(raw) };
                npad_ids.push(npad_id);
            }
        }

        let rm = server.resource_manager.lock();
        let result = if let Some(ref npad) = rm.get_npad() {
            to_ipc_result(npad.lock().set_supported_npad_id_type(aruid, &npad_ids))
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 103: ActivateNpad
    fn activate_npad(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::ActivateNpad called, aruid={}", aruid);

        let rm = server.resource_manager.lock();
        let result = if let Some(ref npad) = rm.get_npad() {
            let mut ng = npad.lock();
            ng.npad_resource_mut().set_npad_revision(aruid, NpadRevision::Revision0);
            to_ipc_result(ng.activate_for_aruid(aruid))
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 108: GetPlayerLedPattern
    fn get_player_led_pattern(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let npad_id_raw = rp.pop_u32();
        let npad_id: NpadIdType = unsafe { core::mem::transmute(npad_id_raw) };
        log::debug!("IHidServer::GetPlayerLedPattern called, npad_id={:?}", npad_id);

        let pattern = match npad_id {
            NpadIdType::Player1 => LedPattern::new(1, 0, 0, 0),
            NpadIdType::Player2 => LedPattern::new(1, 1, 0, 0),
            NpadIdType::Player3 => LedPattern::new(1, 1, 1, 0),
            NpadIdType::Player4 => LedPattern::new(1, 1, 1, 1),
            NpadIdType::Player5 => LedPattern::new(1, 0, 0, 1),
            NpadIdType::Player6 => LedPattern::new(1, 0, 1, 0),
            NpadIdType::Player7 => LedPattern::new(1, 0, 1, 1),
            NpadIdType::Player8 => LedPattern::new(0, 1, 1, 0),
            _ => LedPattern::new(0, 0, 0, 0),
        };

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(pattern.raw);
    }

    // cmd 109: ActivateNpadWithRevision
    fn activate_npad_with_revision(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let revision_raw = rp.pop_u32();
        let _padding = rp.pop_u32();
        let aruid = rp.pop_u64();
        let revision: NpadRevision = unsafe { core::mem::transmute(revision_raw) };
        log::debug!("IHidServer::ActivateNpadWithRevision called, revision={:?}, aruid={}", revision, aruid);

        let rm = server.resource_manager.lock();
        let result = if let Some(ref npad) = rm.get_npad() {
            let mut ng = npad.lock();
            ng.npad_resource_mut().set_npad_revision(aruid, revision);
            to_ipc_result(ng.activate_for_aruid(aruid))
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 120: SetNpadJoyHoldType
    fn set_npad_joy_hold_type(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        let hold_type_raw = rp.pop_u64();
        let hold_type: NpadJoyHoldType = unsafe { core::mem::transmute(hold_type_raw) };
        log::debug!("IHidServer::SetNpadJoyHoldType called, aruid={}, hold_type={:?}", aruid, hold_type);

        if hold_type != NpadJoyHoldType::Horizontal && hold_type != NpadJoyHoldType::Vertical {
            log::error!("Invalid npad joy hold type: {:?}", hold_type);
        }

        let rm = server.resource_manager.lock();
        let result = if let Some(ref npad) = rm.get_npad() {
            to_ipc_result(npad.lock().set_npad_joy_hold_type(aruid, hold_type))
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 121: GetNpadJoyHoldType
    fn get_npad_joy_hold_type(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::GetNpadJoyHoldType called, aruid={}", aruid);

        let rm = server.resource_manager.lock();
        let (result, hold_type) = if let Some(ref npad) = rm.get_npad() {
            match npad.lock().get_npad_joy_hold_type(aruid) {
                Ok(ht) => (RESULT_SUCCESS, ht),
                Err(e) => (to_ipc_result(e), NpadJoyHoldType::Vertical),
            }
        } else { (RESULT_SUCCESS, NpadJoyHoldType::Vertical) };

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(result);
        rb.push_u64(hold_type as u64);
    }

    // cmd 200: GetVibrationDeviceInfo
    fn get_vibration_device_info(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let handle: VibrationDeviceHandle = rp.pop_raw();
        log::debug!("IHidServer::GetVibrationDeviceInfo called, npad_type={:?}, npad_id={}, device_index={:?}", handle.npad_type, handle.npad_id, handle.device_index);

        let rm = server.resource_manager.lock();
        match rm.get_vibration_device_info(&handle) {
            Ok(info) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_raw(&info);
            }
            Err(result) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(to_ipc_result(result));
            }
        }
    }

    // cmd 201: SendVibrationValue
    fn send_vibration_value(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let _handle: VibrationDeviceHandle = rp.pop_raw();
        let _value: VibrationValue = rp.pop_raw();
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::SendVibrationValue called, aruid={}", aruid);
        // ResourceManager::SendVibrationValue not yet ported.
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    // cmd 203: CreateActiveVibrationDeviceList
    fn create_active_vibration_device_list(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        log::debug!("IHidServer::CreateActiveVibrationDeviceList called");

        let device_list: Arc<dyn SessionRequestHandler> =
            Arc::new(super::active_vibration_device_list::IActiveVibrationDeviceList::new(
                server.resource_manager.clone(),
            ));

        let is_domain = ctx.get_manager().map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain { 0 } else {
            ctx.create_session_for_service(device_list.clone()).unwrap_or(0)
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(device_list);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    // cmd 204: PermitVibration
    fn permit_vibration(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let can_vibrate = rp.pop_bool();
        log::debug!("IHidServer::PermitVibration called, can_vibrate={}", can_vibrate);

        let rm = server.resource_manager.lock();
        let result = if let Some(ref npad) = rm.get_npad() {
            let volume = if can_vibrate { 1.0f32 } else { 0.0f32 };
            to_ipc_result(npad.lock().set_vibration_master_volume(volume))
        } else { RESULT_SUCCESS };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    // cmd 205: IsVibrationPermitted
    fn is_vibration_permitted(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let server = Self::as_self(this);
        log::debug!("IHidServer::IsVibrationPermitted called");

        let rm = server.resource_manager.lock();
        let (result, is_permitted) = if let Some(ref npad) = rm.get_npad() {
            match npad.lock().get_vibration_master_volume() {
                Ok(volume) => (RESULT_SUCCESS, volume > 0.0f32),
                Err(e) => (to_ipc_result(e), false),
            }
        } else { (RESULT_SUCCESS, false) };

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(result);
        rb.push_bool(is_permitted);
    }

    // cmd 206: SendVibrationValues
    fn send_vibration_values(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let aruid = rp.pop_u64();
        log::debug!("IHidServer::SendVibrationValues called, aruid={}", aruid);

        let handles_buf = ctx.read_buffer(0);
        let values_buf = ctx.read_buffer(1);
        let handle_size = core::mem::size_of::<VibrationDeviceHandle>();
        let value_size = core::mem::size_of::<VibrationValue>();
        let handle_count = if handle_size > 0 { handles_buf.len() / handle_size } else { 0 };
        let value_count = if value_size > 0 { values_buf.len() / value_size } else { 0 };

        if handle_count != value_count {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(to_ipc_result(hid_core::hid_result::RESULT_VIBRATION_ARRAY_SIZE_MISMATCH));
            return;
        }
        // ResourceManager::SendVibrationValue not yet ported; accept silently.
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IHidServer {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "hid"
    }
}

impl ServiceFramework for IHidServer {
    fn get_service_name(&self) -> &str {
        "hid"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
