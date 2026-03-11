// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_web_browser_types.h

/// Port of WebArgTLVType
#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WebArgTlvType {
    InitialUrl = 0x1,
    CallbackUrl = 0x3,
    CallbackableUrl = 0x4,
    ApplicationId = 0x5,
    DocumentPath = 0x6,
    DocumentKind = 0x7,
    SystemDataId = 0x8,
    ShareStartPage = 0x9,
    Whitelist = 0xA,
    NewsFlag = 0xE,
    UserID = 0x10,
    BootDisplayKind = 0x17,
    BackgroundKind = 0x18,
    Footer = 0x19,
    Pointer = 0x1A,
    LeftStickMode = 0x1B,
    KeyRepeatFrame0 = 0x1C,
    KeyRepeatFrame1 = 0x1D,
    BootAsMediaPlayer = 0x21,
    DisplayUrlKind = 0x23,
    PageCacheEnabled = 0x24,
    PlayReportEnabled = 0x25,
    Unknown1 = 0x26,
    BootLoadingIconEnabled = 0x28,
    PageScrollIndicatorEnabled = 0x29,
    OverrideWebAudioVolume = 0x2A,
    OverrideMediaAudioVolume = 0x2B,
    Unknown2 = 0x2E,
    MediaAutoPlayEnabled = 0x30,
    TransferMemorySize = 0x33,
    Unknown3 = 0x34,
    Unknown4 = 0x35,
    Unknown5 = 0x36,
    JsExtensionEnabled = 0x3B,
    AdditionalCommentText = 0x3E,
    TouchEnabledOnContents = 0x3F,
    UserAgentAdditionalString = 0x40,
    MediaPlayerSpeedControlEnabled = 0x43,
    AdditionalMediaData0 = 0x44,
    MediaPlayerUserGestureRestrictionEnabled = 0x45,
}

/// Port of WebExitReason
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WebExitReason {
    EndButtonPressed = 0,
    BackCrossButtonPressed = 1,
    ExitRequested = 2,
    CallbackUrlReached = 3,
    LastUrl = 4,
    ErrorDialog = 7,
}

/// Port of ShimKind
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShimKind {
    Shop = 1,
    Login = 2,
    Offline = 3,
    Share = 4,
    Web = 5,
    Wifi = 6,
    Lobby = 7,
}
