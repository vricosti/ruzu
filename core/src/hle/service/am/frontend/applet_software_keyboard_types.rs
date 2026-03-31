// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_software_keyboard_types.h

/// Port of SwkbdType
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdType {
    Normal = 0,
    NumberPad = 1,
    Qwerty = 2,
    Unknown3 = 3,
    Latin = 4,
    SimplifiedChinese = 5,
    TraditionalChinese = 6,
    Korean = 7,
}

/// Port of SwkbdInitialCursorPosition
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdInitialCursorPosition {
    First = 0,
    Last = 1,
}

/// Port of SwkbdPasswordMode
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdPasswordMode {
    Show = 0,
    Hide = 1,
}

/// Port of SwkbdTextCheckResult
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdTextCheckResult {
    Success = 0,
    ShowFailureDialog = 1,
    ShowConfirmDialog = 2,
    Silent = 3,
}

/// Port of SwkbdResult
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdResult {
    Ok = 0,
    Cancel = 1,
}

/// Port of SwkbdTextDrawType
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SwkbdTextDrawType {
    #[default]
    Line = 0,
    Box = 1,
    DownloadCode = 2,
}

/// Port of SwkbdKeyDisableFlags — u32 bitfield union in C++.
/// Stored as a plain u32 since we only need zero-init for the push-in defaults.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SwkbdKeyDisableFlags(pub u32);

// ---------------------------------------------------------------------------
// Maximum string lengths — match C++ constants in applet_software_keyboard_types.h
// ---------------------------------------------------------------------------
pub const MAX_OK_TEXT_LENGTH: usize = 8;
pub const MAX_HEADER_TEXT_LENGTH: usize = 64;
pub const MAX_SUB_TEXT_LENGTH: usize = 128;
pub const MAX_GUIDE_TEXT_LENGTH: usize = 256;

/// Port of SwkbdConfigCommon — 0x3D4 bytes.
///
/// Field layout matches upstream C++ with `#[repr(C)]`.  Rust inserts the same
/// implicit padding bytes as the C++ compiler:
/// • 1 byte after `use_prediction` (before `key_disable_flags: u32` at +28)
/// • 2 bytes after `guide_text` (before `max_text_length: u32` at +940)
/// • 1 byte after `use_blur_background` (before `initial_string_offset: u32` at +960)
/// • 3 bytes after `use_text_check` (struct total must be multiple of 4)
#[repr(C)]
#[derive(Clone, Copy)]
pub struct SwkbdConfigCommon {
    pub swkbd_type: u32,                        // +0
    pub ok_text: [u16; MAX_OK_TEXT_LENGTH + 1], // +4  (18 bytes)
    pub left_optional_symbol_key: u16,          // +22
    pub right_optional_symbol_key: u16,         // +24
    pub use_prediction: bool,                   // +26
    // 1 byte implicit padding here → key_disable_flags at +28
    pub key_disable_flags: SwkbdKeyDisableFlags, // +28
    pub initial_cursor_position: u32,            // +32
    pub header_text: [u16; MAX_HEADER_TEXT_LENGTH + 1], // +36  (130 bytes)
    pub sub_text: [u16; MAX_SUB_TEXT_LENGTH + 1], // +166 (258 bytes)
    pub guide_text: [u16; MAX_GUIDE_TEXT_LENGTH + 1], // +424 (514 bytes)
    // 2 bytes implicit padding here → max_text_length at +940
    pub max_text_length: u32,       // +940
    pub min_text_length: u32,       // +944
    pub password_mode: u32,         // +948
    pub text_draw_type: u32,        // +952
    pub enable_return_button: bool, // +956
    pub use_utf8: bool,             // +957
    pub use_blur_background: bool,  // +958
    // 1 byte implicit padding here → initial_string_offset at +960
    pub initial_string_offset: u32,   // +960
    pub initial_string_length: u32,   // +964
    pub user_dictionary_offset: u32,  // +968
    pub user_dictionary_entries: u32, // +972
    pub use_text_check: bool,         // +976
                                      // 3 bytes implicit padding (struct alignment to 4)
}
const _: () = assert!(std::mem::size_of::<SwkbdConfigCommon>() == 0x3D4);

impl Default for SwkbdConfigCommon {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// Port of SwkbdConfigNew (SwkbdAppletVersion 0x6000B, 0x8000D) — 0xF4 bytes.
///
/// Upstream uses `#pragma pack(push, 4)` which caps member alignment at 4 bytes.
/// The C++ field `std::array<u64, 24>` therefore has 4-byte (not 8-byte) alignment
/// under pack(4), giving a struct size of 244 = 0xF4.
///
/// In Rust there is no stable `repr(C, packed(4))`, so we replace the
/// `[u64; 24]` field with `[u32; 48]` — identical 192-byte layout, 4-byte
/// alignment — to produce the same struct size.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct SwkbdConfigNew {
    pub text_grouping: [u32; 8],                      // +0   (32 bytes)
    pub customized_dictionary_set_entries: [u32; 48], // +32  (192 bytes; u64×24 under pack(4))
    pub total_customized_dictionary_set_entries: u8,  // +224
    pub disable_cancel_button: bool,                  // +225
    pub _padding: [u8; 18],                           // +226
}
const _: () = assert!(std::mem::size_of::<SwkbdConfigNew>() == 0x4C8 - 0x3D4);

impl Default for SwkbdConfigNew {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}
