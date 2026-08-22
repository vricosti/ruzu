// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `core/hle/service/am/frontend/applet_web_browser_types.h`.

use std::collections::HashMap;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct WebAppletVersion(pub u32);

impl WebAppletVersion {
    pub const VERSION_0: Self = Self(0x0);
    pub const VERSION_131072: Self = Self(0x20000);
    pub const VERSION_196608: Self = Self(0x30000);
    pub const VERSION_327680: Self = Self(0x50000);
    pub const VERSION_393216: Self = Self(0x60000);
    pub const VERSION_524288: Self = Self(0x80000);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ShimKind(pub u32);

impl ShimKind {
    pub const SHOP: Self = Self(1);
    pub const LOGIN: Self = Self(2);
    pub const OFFLINE: Self = Self(3);
    pub const SHARE: Self = Self(4);
    pub const WEB: Self = Self(5);
    pub const WIFI: Self = Self(6);
    pub const LOBBY: Self = Self(7);
    pub const LHUB: Self = Self(8);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct WebExitReason(pub u32);

impl WebExitReason {
    pub const END_BUTTON_PRESSED: Self = Self(0);
    pub const BACK_BUTTON_PRESSED: Self = Self(1);
    pub const EXIT_REQUESTED: Self = Self(2);
    pub const CALLBACK_URL: Self = Self(3);
    pub const WINDOW_CLOSED: Self = Self(4);
    pub const ERROR_DIALOG: Self = Self(7);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct WebArgInputTlvType(pub u16);

impl WebArgInputTlvType {
    pub const INITIAL_URL: Self = Self(0x1);
    pub const CALLBACK_URL: Self = Self(0x3);
    pub const CALLBACKABLE_URL: Self = Self(0x4);
    pub const APPLICATION_ID: Self = Self(0x5);
    pub const DOCUMENT_PATH: Self = Self(0x6);
    pub const DOCUMENT_KIND: Self = Self(0x7);
    pub const SYSTEM_DATA_ID: Self = Self(0x8);
    pub const SHARE_START_PAGE: Self = Self(0x9);
    pub const WHITELIST: Self = Self(0xA);
    pub const NEWS: Self = Self(0xB);
    pub const USER_ID: Self = Self(0xE);
    pub const ALBUM_ENTRY_0: Self = Self(0xF);
    pub const SCREEN_SHOT_ENABLED: Self = Self(0x10);
    pub const EC_CLIENT_CERT_ENABLED: Self = Self(0x11);
    pub const PLAY_REPORT_ENABLED: Self = Self(0x13);
    pub const BOOT_DISPLAY_KIND: Self = Self(0x17);
    pub const BACKGROUND_KIND: Self = Self(0x18);
    pub const FOOTER_ENABLED: Self = Self(0x19);
    pub const POINTER_ENABLED: Self = Self(0x1A);
    pub const LEFT_STICK_MODE: Self = Self(0x1B);
    pub const KEY_REPEAT_FRAME_1: Self = Self(0x1C);
    pub const KEY_REPEAT_FRAME_2: Self = Self(0x1D);
    pub const BOOT_AS_MEDIA_PLAYER_INVERTED: Self = Self(0x1E);
    pub const DISPLAY_URL_KIND: Self = Self(0x1F);
    pub const BOOT_AS_MEDIA_PLAYER: Self = Self(0x21);
    pub const SHOP_JUMP_ENABLED: Self = Self(0x22);
    pub const MEDIA_AUTO_PLAY_ENABLED: Self = Self(0x23);
    pub const LOBBY_PARAMETER: Self = Self(0x24);
    pub const APPLICATION_ALBUM_ENTRY: Self = Self(0x26);
    pub const JS_EXTENSION_ENABLED: Self = Self(0x27);
    pub const ADDITIONAL_COMMENT_TEXT: Self = Self(0x28);
    pub const TOUCH_ENABLED_ON_CONTENTS: Self = Self(0x29);
    pub const USER_AGENT_ADDITIONAL_STRING: Self = Self(0x2A);
    pub const ADDITIONAL_MEDIA_DATA_0: Self = Self(0x2B);
    pub const MEDIA_PLAYER_AUTO_CLOSE_ENABLED: Self = Self(0x2C);
    pub const PAGE_CACHE_ENABLED: Self = Self(0x2D);
    pub const WEB_AUDIO_ENABLED: Self = Self(0x2E);
    pub const YOUTUBE_VIDEO_WHITELIST: Self = Self(0x31);
    pub const FOOTER_FIXED_KIND: Self = Self(0x32);
    pub const PAGE_FADE_ENABLED: Self = Self(0x33);
    pub const MEDIA_CREATOR_APPLICATION_RATING_AGE: Self = Self(0x34);
    pub const BOOT_LOADING_ICON_ENABLED: Self = Self(0x35);
    pub const PAGE_SCROLL_INDICATOR_ENABLED: Self = Self(0x36);
    pub const MEDIA_PLAYER_SPEED_CONTROL_ENABLED: Self = Self(0x37);
    pub const ALBUM_ENTRY_1: Self = Self(0x38);
    pub const ALBUM_ENTRY_2: Self = Self(0x39);
    pub const ALBUM_ENTRY_3: Self = Self(0x3A);
    pub const ADDITIONAL_MEDIA_DATA_1: Self = Self(0x3B);
    pub const ADDITIONAL_MEDIA_DATA_2: Self = Self(0x3C);
    pub const ADDITIONAL_MEDIA_DATA_3: Self = Self(0x3D);
    pub const BOOT_FOOTER_BUTTON: Self = Self(0x3E);
    pub const OVERRIDE_WEB_AUDIO_VOLUME: Self = Self(0x3F);
    pub const OVERRIDE_MEDIA_AUDIO_VOLUME: Self = Self(0x40);
    pub const BOOT_MODE: Self = Self(0x41);
    pub const WEB_SESSION_ENABLED: Self = Self(0x42);
    pub const MEDIA_PLAYER_OFFLINE_ENABLED: Self = Self(0x43);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct WebArgOutputTlvType(pub u16);

impl WebArgOutputTlvType {
    pub const SHARE_EXIT_REASON: Self = Self(0x1);
    pub const LAST_URL: Self = Self(0x2);
    pub const LAST_URL_SIZE: Self = Self(0x3);
    pub const SHARE_POST_RESULT: Self = Self(0x4);
    pub const POST_SERVICE_NAME: Self = Self(0x5);
    pub const POST_SERVICE_NAME_SIZE: Self = Self(0x6);
    pub const POST_ID: Self = Self(0x7);
    pub const POST_ID_SIZE: Self = Self(0x8);
    pub const MEDIA_PLAYER_AUTO_CLOSED_BY_COMPLETION: Self = Self(0x9);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct DocumentKind(pub u32);

impl DocumentKind {
    pub const OFFLINE_HTML_PAGE: Self = Self(1);
    pub const APPLICATION_LEGAL_INFORMATION: Self = Self(2);
    pub const SYSTEM_DATA_PAGE: Self = Self(3);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ShareStartPage(pub u32);

impl ShareStartPage {
    pub const DEFAULT: Self = Self(0);
    pub const SETTINGS: Self = Self(1);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BootDisplayKind(pub u32);

impl BootDisplayKind {
    pub const DEFAULT: Self = Self(0);
    pub const WHITE: Self = Self(1);
    pub const BLACK: Self = Self(2);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct BackgroundKind(pub u32);

impl BackgroundKind {
    pub const DEFAULT: Self = Self(0);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct LeftStickMode(pub u32);

impl LeftStickMode {
    pub const POINTER: Self = Self(0);
    pub const CURSOR: Self = Self(1);
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct WebSessionBootMode(pub u32);

impl WebSessionBootMode {
    pub const ALL_FOREGROUND: Self = Self(0);
    pub const ALL_FOREGROUND_INITIALLY_HIDDEN: Self = Self(1);
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct WebArgHeader {
    pub total_tlv_entries: u16,
    pub _padding: [u8; 2],
    pub shim_kind: ShimKind,
}
const _: () = assert!(std::mem::size_of::<WebArgHeader>() == 0x8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct WebArgInputTlv {
    pub input_tlv_type: WebArgInputTlvType,
    pub arg_data_size: u16,
    pub _padding: u32,
}
const _: () = assert!(std::mem::size_of::<WebArgInputTlv>() == 0x8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct WebArgOutputTlv {
    pub output_tlv_type: WebArgOutputTlvType,
    pub arg_data_size: u16,
    pub _padding: u32,
}
const _: () = assert!(std::mem::size_of::<WebArgOutputTlv>() == 0x8);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct WebCommonReturnValue {
    pub exit_reason: WebExitReason,
    pub _padding: u32,
    pub last_url: [u8; 0x1000],
    pub last_url_size: u64,
}
const _: () = assert!(std::mem::size_of::<WebCommonReturnValue>() == 0x1010);

impl Default for WebCommonReturnValue {
    fn default() -> Self {
        Self {
            exit_reason: WebExitReason::default(),
            _padding: 0,
            last_url: [0; 0x1000],
            last_url_size: 0,
        }
    }
}

pub type WebArgInputTlvMap = HashMap<WebArgInputTlvType, Vec<u8>>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wire_layouts_and_eden_values_match() {
        assert_eq!(std::mem::size_of::<WebArgHeader>(), 0x8);
        assert_eq!(std::mem::size_of::<WebArgInputTlv>(), 0x8);
        assert_eq!(std::mem::size_of::<WebArgOutputTlv>(), 0x8);
        assert_eq!(std::mem::size_of::<WebCommonReturnValue>(), 0x1010);
        assert_eq!(ShimKind::LHUB.0, 8);
        assert_eq!(WebExitReason::WINDOW_CLOSED.0, 4);
        assert_eq!(WebArgInputTlvType::MEDIA_PLAYER_OFFLINE_ENABLED.0, 0x43);
    }
}
