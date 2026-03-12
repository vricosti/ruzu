// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/hid_result.h
//!
//! HID result codes. Uses common::ResultCode.

use common::ResultCode;
use common::error::module;

/// Module ID for nn::irsensor results.
const IRSENSOR_MODULE: u32 = 205;

// Service::HID results
pub const PALMA_RESULT_SUCCESS: ResultCode = ResultCode::new(module::HID, 0);
pub const RESULT_TOUCH_NOT_INITIALIZED: ResultCode = ResultCode::new(module::HID, 41);
pub const RESULT_TOUCH_OVERFLOW: ResultCode = ResultCode::new(module::HID, 42);
pub const NPAD_INVALID_HANDLE: ResultCode = ResultCode::new(module::HID, 100);
pub const NPAD_DEVICE_INDEX_OUT_OF_RANGE: ResultCode = ResultCode::new(module::HID, 107);
pub const RESULT_VIBRATION_NOT_INITIALIZED: ResultCode = ResultCode::new(module::HID, 121);
pub const RESULT_VIBRATION_INVALID_STYLE_INDEX: ResultCode = ResultCode::new(module::HID, 122);
pub const RESULT_VIBRATION_INVALID_NPAD_ID: ResultCode = ResultCode::new(module::HID, 123);
pub const RESULT_VIBRATION_DEVICE_INDEX_OUT_OF_RANGE: ResultCode = ResultCode::new(module::HID, 124);
pub const RESULT_VIBRATION_STRENGTH_OUT_OF_RANGE: ResultCode = ResultCode::new(module::HID, 126);
pub const RESULT_VIBRATION_ARRAY_SIZE_MISMATCH: ResultCode = ResultCode::new(module::HID, 131);
pub const INVALID_SIX_AXIS_FUSION_RANGE: ResultCode = ResultCode::new(module::HID, 423);
pub const RESULT_NFC_IS_NOT_READY: ResultCode = ResultCode::new(module::HID, 461);
pub const RESULT_NFC_XCD_HANDLE_IS_NOT_INITIALIZED: ResultCode = ResultCode::new(module::HID, 464);
pub const RESULT_IR_SENSOR_IS_NOT_READY: ResultCode = ResultCode::new(module::HID, 501);
pub const RESULT_GESTURE_OVERFLOW: ResultCode = ResultCode::new(module::HID, 522);
pub const RESULT_GESTURE_NOT_INITIALIZED: ResultCode = ResultCode::new(module::HID, 523);
pub const RESULT_MCU_IS_NOT_READY: ResultCode = ResultCode::new(module::HID, 541);
pub const NPAD_IS_DUAL_JOYCON: ResultCode = ResultCode::new(module::HID, 601);
pub const NPAD_IS_SAME_TYPE: ResultCode = ResultCode::new(module::HID, 602);
pub const RESULT_NPAD_IS_NOT_PRO_CONTROLLER: ResultCode = ResultCode::new(module::HID, 604);
pub const RESULT_INVALID_NPAD_ID: ResultCode = ResultCode::new(module::HID, 709);
pub const RESULT_NPAD_NOT_CONNECTED: ResultCode = ResultCode::new(module::HID, 710);
pub const RESULT_NPAD_HANDLER_OVERFLOW: ResultCode = ResultCode::new(module::HID, 711);
pub const RESULT_NPAD_HANDLER_NOT_INITIALIZED: ResultCode = ResultCode::new(module::HID, 712);
pub const RESULT_INVALID_ARRAY_SIZE: ResultCode = ResultCode::new(module::HID, 715);
pub const RESULT_UNDEFINED_STYLESET: ResultCode = ResultCode::new(module::HID, 716);
pub const RESULT_MULTIPLE_STYLE_SET_SELECTED: ResultCode = ResultCode::new(module::HID, 717);
pub const RESULT_APPLET_RESOURCE_OVERFLOW: ResultCode = ResultCode::new(module::HID, 1041);
pub const RESULT_APPLET_RESOURCE_NOT_INITIALIZED: ResultCode = ResultCode::new(module::HID, 1042);
pub const RESULT_SHARED_MEMORY_NOT_INITIALIZED: ResultCode = ResultCode::new(module::HID, 1043);
pub const RESULT_ARUID_NO_AVAILABLE_ENTRIES: ResultCode = ResultCode::new(module::HID, 1044);
pub const RESULT_ARUID_ALREADY_REGISTERED: ResultCode = ResultCode::new(module::HID, 1046);
pub const RESULT_ARUID_NOT_REGISTERED: ResultCode = ResultCode::new(module::HID, 1047);
pub const RESULT_NPAD_RESOURCE_OVERFLOW: ResultCode = ResultCode::new(module::HID, 2001);
pub const RESULT_NPAD_RESOURCE_NOT_INITIALIZED: ResultCode = ResultCode::new(module::HID, 2002);
pub const INVALID_PALMA_HANDLE: ResultCode = ResultCode::new(module::HID, 3302);

// Service::IRS results
pub const IRS_INVALID_PROCESSOR_STATE: ResultCode = ResultCode::new(IRSENSOR_MODULE, 78);
pub const IRS_INVALID_IR_CAMERA_HANDLE: ResultCode = ResultCode::new(IRSENSOR_MODULE, 204);
