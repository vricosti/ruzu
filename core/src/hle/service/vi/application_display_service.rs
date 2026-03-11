// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/application_display_service.cpp/.h

use super::vi_types::*;

pub const IAPPLICATION_DISPLAY_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (100, true, "GetRelayService"),
    (101, true, "GetSystemDisplayService"),
    (102, true, "GetManagerDisplayService"),
    (103, true, "GetIndirectDisplayTransactionService"),
    (1000, true, "ListDisplays"),
    (1010, true, "OpenDisplay"),
    (1011, true, "OpenDefaultDisplay"),
    (1020, true, "CloseDisplay"),
    (1101, true, "SetDisplayEnabled"),
    (1102, true, "GetDisplayResolution"),
    (2020, true, "OpenLayer"),
    (2021, true, "CloseLayer"),
    (2030, true, "CreateStrayLayer"),
    (2031, true, "DestroyStrayLayer"),
    (2101, true, "SetLayerScalingMode"),
    (2102, true, "ConvertScalingMode"),
    (2450, true, "GetIndirectLayerImageMap"),
    (2451, false, "GetIndirectLayerImageCropMap"),
    (2460, true, "GetIndirectLayerImageRequiredMemoryInfo"),
    (5202, true, "GetDisplayVsyncEvent"),
    (5203, false, "GetDisplayVsyncEventForDebug"),
];

/// GetDisplayResolution returns fixed 1280x720 upstream.
pub fn get_display_resolution(_display_id: u64) -> (i64, i64) {
    log::debug!("IApplicationDisplayService::GetDisplayResolution called");
    (
        DisplayResolution::UndockedWidth as i64,
        DisplayResolution::UndockedHeight as i64,
    )
}

/// SetDisplayEnabled does nothing upstream.
pub fn set_display_enabled(_state: u32, _display_id: u64) {
    log::debug!("IApplicationDisplayService::SetDisplayEnabled called (no-op)");
}

/// ConvertScalingMode maps NintendoScaleMode to ConvertedScaleMode.
pub fn convert_scaling_mode(mode: NintendoScaleMode) -> Option<ConvertedScaleMode> {
    match mode {
        NintendoScaleMode::None => Some(ConvertedScaleMode::None),
        NintendoScaleMode::Freeze => Some(ConvertedScaleMode::Freeze),
        NintendoScaleMode::ScaleToWindow => Some(ConvertedScaleMode::ScaleToWindow),
        NintendoScaleMode::ScaleAndCrop => Some(ConvertedScaleMode::ScaleAndCrop),
        NintendoScaleMode::PreserveAspectRatio => Some(ConvertedScaleMode::PreserveAspectRatio),
        _ => None,
    }
}

/// GetIndirectLayerImageRequiredMemoryInfo computes aligned size.
pub fn get_indirect_layer_image_required_memory_info(width: i64, height: i64) -> (i64, i64) {
    const BASE_SIZE: u64 = 0x20000;
    let texture_size = (width * height * 4) as u64;
    let alignment: i64 = 0x1000;
    let size = ((texture_size + BASE_SIZE - 1) / BASE_SIZE * BASE_SIZE) as i64;
    (size, alignment)
}
