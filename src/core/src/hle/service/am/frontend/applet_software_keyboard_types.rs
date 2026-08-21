// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_software_keyboard_types.h

pub const MAX_OK_TEXT_LENGTH: usize = 8;
pub const MAX_HEADER_TEXT_LENGTH: usize = 64;
pub const MAX_SUB_TEXT_LENGTH: usize = 128;
pub const MAX_GUIDE_TEXT_LENGTH: usize = 256;
pub const STRING_BUFFER_SIZE: usize = 0x7D4;

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SwkbdAppletVersion {
    Version5 = 0x5,
    Version65542 = 0x10006,
    Version196615 = 0x30007,
    Version262152 = 0x40008,
    Version327689 = 0x50009,
    Version393227 = 0x6000B,
    Version524301 = 0x8000D,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SwkbdType {
    #[default]
    Normal = 0,
    NumberPad = 1,
    Qwerty = 2,
    Unknown3 = 3,
    Latin = 4,
    SimplifiedChinese = 5,
    TraditionalChinese = 6,
    Korean = 7,
}

impl SwkbdType {
    pub fn from_raw(raw: u32) -> Self {
        match raw {
            1 => Self::NumberPad,
            2 => Self::Qwerty,
            3 => Self::Unknown3,
            4 => Self::Latin,
            5 => Self::SimplifiedChinese,
            6 => Self::TraditionalChinese,
            7 => Self::Korean,
            _ => Self::Normal,
        }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SwkbdInitialCursorPosition {
    #[default]
    Start = 0,
    End = 1,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SwkbdPasswordMode {
    #[default]
    Disabled = 0,
    Enabled = 1,
}

impl SwkbdPasswordMode {
    pub fn from_raw(raw: u32) -> Self {
        if raw == Self::Enabled as u32 {
            Self::Enabled
        } else {
            Self::Disabled
        }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SwkbdTextCheckResult {
    #[default]
    Success = 0,
    Failure = 1,
    Confirm = 2,
    Silent = 3,
}

impl SwkbdTextCheckResult {
    pub fn from_raw(raw: u32) -> Self {
        match raw {
            1 => Self::Failure,
            2 => Self::Confirm,
            3 => Self::Silent,
            _ => Self::Success,
        }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdResult {
    Ok = 0,
    Cancel = 1,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SwkbdTextDrawType {
    #[default]
    Line = 0,
    Box = 1,
    DownloadCode = 2,
}

impl SwkbdTextDrawType {
    pub fn from_raw(raw: u32) -> Self {
        match raw {
            1 => Self::Box,
            2 => Self::DownloadCode,
            _ => Self::Line,
        }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum SwkbdState {
    #[default]
    NotInitialized = 0,
    InitializedIsHidden = 1,
    InitializedIsAppearing = 2,
    InitializedIsShown = 3,
    InitializedIsDisappearing = 4,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdRequestCommand {
    Finalize = 0x4,
    SetUserWordInfo = 0x6,
    SetCustomizeDic = 0x7,
    Calc = 0xA,
    SetCustomizedDictionaries = 0xB,
    UnsetCustomizedDictionaries = 0xC,
    SetChangedStringV2Flag = 0xD,
    SetMovedCursorV2Flag = 0xE,
}

impl SwkbdRequestCommand {
    pub fn from_raw(raw: u32) -> Option<Self> {
        Some(match raw {
            0x4 => Self::Finalize,
            0x6 => Self::SetUserWordInfo,
            0x7 => Self::SetCustomizeDic,
            0xA => Self::Calc,
            0xB => Self::SetCustomizedDictionaries,
            0xC => Self::UnsetCustomizedDictionaries,
            0xD => Self::SetChangedStringV2Flag,
            0xE => Self::SetMovedCursorV2Flag,
            _ => return None,
        })
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwkbdReplyType {
    FinishedInitialize = 0,
    Default = 1,
    ChangedString = 2,
    MovedCursor = 3,
    MovedTab = 4,
    DecidedEnter = 5,
    DecidedCancel = 6,
    ChangedStringUtf8 = 7,
    MovedCursorUtf8 = 8,
    DecidedEnterUtf8 = 9,
    UnsetCustomizeDic = 10,
    ReleasedUserWordInfo = 11,
    UnsetCustomizedDictionaries = 12,
    ChangedStringV2 = 13,
    MovedCursorV2 = 14,
    ChangedStringUtf8V2 = 15,
    MovedCursorUtf8V2 = 16,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SwkbdKeyDisableFlags {
    pub raw: u32,
}
const _: () = assert!(std::mem::size_of::<SwkbdKeyDisableFlags>() == 0x4);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct SwkbdConfigCommon {
    pub swkbd_type: u32,
    pub ok_text: [u16; MAX_OK_TEXT_LENGTH + 1],
    pub left_optional_symbol_key: u16,
    pub right_optional_symbol_key: u16,
    pub use_prediction: bool,
    pub _padding_1: [u8; 1],
    pub key_disable_flags: SwkbdKeyDisableFlags,
    pub initial_cursor_position: u32,
    pub header_text: [u16; MAX_HEADER_TEXT_LENGTH + 1],
    pub sub_text: [u16; MAX_SUB_TEXT_LENGTH + 1],
    pub guide_text: [u16; MAX_GUIDE_TEXT_LENGTH + 1],
    pub max_text_length: u32,
    pub min_text_length: u32,
    pub password_mode: u32,
    pub text_draw_type: u32,
    pub enable_return_button: bool,
    pub use_utf8: bool,
    pub use_blur_background: bool,
    pub _padding_2: [u8; 1],
    pub initial_string_offset: u32,
    pub initial_string_length: u32,
    pub user_dictionary_offset: u32,
    pub user_dictionary_entries: u32,
    pub use_text_check: bool,
    pub _padding_3: [u8; 3],
}
const _: () = assert!(std::mem::size_of::<SwkbdConfigCommon>() == 0x3D4);

impl Default for SwkbdConfigCommon {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

#[repr(C, packed(4))]
#[derive(Clone, Copy, Default)]
pub struct SwkbdConfigOld {
    pub _padding: [u32; 1],
    pub text_check_callback: u64,
}
const _: () = assert!(std::mem::size_of::<SwkbdConfigOld>() == 0x3E0 - 0x3D4);

#[repr(C, packed(4))]
#[derive(Clone, Copy, Default)]
pub struct SwkbdConfigOld2 {
    pub _padding: [u32; 1],
    pub text_check_callback: u64,
    pub text_grouping: [u32; 8],
}
const _: () = assert!(std::mem::size_of::<SwkbdConfigOld2>() == 0x400 - 0x3D4);

#[repr(C, packed(4))]
#[derive(Clone, Copy)]
pub struct SwkbdConfigNew {
    pub text_grouping: [u32; 8],
    pub customized_dictionary_set_entries: [u64; 24],
    pub total_customized_dictionary_set_entries: u8,
    pub disable_cancel_button: bool,
    pub _padding: [u8; 18],
}
const _: () = assert!(std::mem::size_of::<SwkbdConfigNew>() == 0x4C8 - 0x3D4);

impl Default for SwkbdConfigNew {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct SwkbdTextCheck {
    pub text_check_result: u32,
    pub text_check_message: [u16; STRING_BUFFER_SIZE / 2],
}
const _: () = assert!(std::mem::size_of::<SwkbdTextCheck>() == 0x7D8);

impl Default for SwkbdTextCheck {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SwkbdCalcArgFlags {
    pub raw: u64,
}

impl SwkbdCalcArgFlags {
    pub fn set_initialize_arg(self) -> bool {
        self.raw & (1 << 0) != 0
    }
    pub fn appear(self) -> bool {
        self.raw & (1 << 2) != 0
    }
    pub fn set_input_text(self) -> bool {
        self.raw & (1 << 3) != 0
    }
    pub fn set_cursor_position(self) -> bool {
        self.raw & (1 << 4) != 0
    }
    pub fn set_utf8_mode(self) -> bool {
        self.raw & (1 << 5) != 0
    }
    pub fn unset_customize_dic(self) -> bool {
        self.raw & (1 << 6) != 0
    }
    pub fn disappear(self) -> bool {
        self.raw & (1 << 7) != 0
    }
    pub fn unset_user_word_info(self) -> bool {
        self.raw & (1 << 10) != 0
    }
}
const _: () = assert!(std::mem::size_of::<SwkbdCalcArgFlags>() == 0x8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SwkbdInitializeArg {
    pub unknown: u32,
    pub library_applet_mode_flag: bool,
    pub is_above_hos_500: bool,
    pub _padding: [u8; 2],
}
const _: () = assert!(std::mem::size_of::<SwkbdInitializeArg>() == 0x8);

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct SwkbdAppearArgOld {
    pub swkbd_type: u32,
    pub ok_text: [u16; MAX_OK_TEXT_LENGTH + 1],
    pub left_optional_symbol_key: u16,
    pub right_optional_symbol_key: u16,
    pub use_prediction: bool,
    pub disable_cancel_button: bool,
    pub key_disable_flags: SwkbdKeyDisableFlags,
    pub max_text_length: u32,
    pub min_text_length: u32,
    pub enable_return_button: bool,
    pub _padding_1: [u8; 3],
    pub flags: u32,
    pub is_use_save_data: bool,
    pub _padding_2: [u8; 7],
    pub user_id: [u8; 16],
}
const _: () = assert!(std::mem::size_of::<SwkbdAppearArgOld>() == 0x48);

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct SwkbdAppearArgNew {
    pub swkbd_type: u32,
    pub ok_text: [u16; MAX_OK_TEXT_LENGTH + 1],
    pub left_optional_symbol_key: u16,
    pub right_optional_symbol_key: u16,
    pub use_prediction: bool,
    pub disable_cancel_button: bool,
    pub key_disable_flags: SwkbdKeyDisableFlags,
    pub max_text_length: u32,
    pub min_text_length: u32,
    pub enable_return_button: bool,
    pub _padding_1: [u8; 3],
    pub flags: u32,
    pub is_use_save_data: bool,
    pub _padding_2: [u8; 7],
    pub user_id: [u8; 16],
    pub start_sampling_number: u64,
    pub _padding_3: [u32; 8],
}
const _: () = assert!(std::mem::size_of::<SwkbdAppearArgNew>() == 0x70);

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct SwkbdCalcArgCommon {
    pub unknown: u32,
    pub calc_arg_size: u16,
    pub _padding: [u8; 2],
    pub flags: SwkbdCalcArgFlags,
    pub initialize_arg: SwkbdInitializeArg,
}
const _: () = assert!(std::mem::size_of::<SwkbdCalcArgCommon>() == 0x18);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct SwkbdCalcArgOld {
    pub volume: f32,
    pub cursor_position: i32,
    pub appear_arg: SwkbdAppearArgOld,
    pub input_text: [u16; 0x1FA],
    pub utf8_mode: bool,
    pub _padding_1: [u8; 1],
    pub enable_backspace_button: bool,
    pub _padding_2: [u8; 3],
    pub key_top_as_floating: bool,
    pub footer_scalable: bool,
    pub alpha_enabled_in_input_mode: bool,
    pub input_mode_fade_type: u8,
    pub disable_touch: bool,
    pub disable_hardware_keyboard: bool,
    pub _padding_3: [u8; 8],
    pub key_top_scale_x: f32,
    pub key_top_scale_y: f32,
    pub key_top_translate_x: f32,
    pub key_top_translate_y: f32,
    pub key_top_bg_alpha: f32,
    pub footer_bg_alpha: f32,
    pub balloon_scale: f32,
    pub _padding_4: [u32; 4],
    pub se_group: u8,
    pub _padding_5: [u8; 3],
}
const _: () = assert!(std::mem::size_of::<SwkbdCalcArgOld>() == 0x4A0 - 0x18);

impl Default for SwkbdCalcArgOld {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct SwkbdCalcArgNew {
    pub appear_arg: SwkbdAppearArgNew,
    pub volume: f32,
    pub cursor_position: i32,
    pub input_text: [u16; 0x1FA],
    pub utf8_mode: bool,
    pub _padding_1: [u8; 1],
    pub enable_backspace_button: bool,
    pub _padding_2: [u8; 3],
    pub key_top_as_floating: bool,
    pub footer_scalable: bool,
    pub alpha_enabled_in_input_mode: bool,
    pub input_mode_fade_type: u8,
    pub disable_touch: bool,
    pub disable_hardware_keyboard: bool,
    pub _padding_3: [u8; 8],
    pub key_top_scale_x: f32,
    pub key_top_scale_y: f32,
    pub key_top_translate_x: f32,
    pub key_top_translate_y: f32,
    pub key_top_bg_alpha: f32,
    pub footer_bg_alpha: f32,
    pub balloon_scale: f32,
    pub _padding_4: [u32; 4],
    pub se_group: u8,
    pub _padding_5: [u8; 3],
    pub _padding_6: [u32; 8],
}
const _: () = assert!(std::mem::size_of::<SwkbdCalcArgNew>() == 0x4E8 - 0x18);

impl Default for SwkbdCalcArgNew {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SwkbdChangedStringArg {
    pub text_length: u32,
    pub dictionary_start_cursor_position: i32,
    pub dictionary_end_cursor_position: i32,
    pub cursor_position: i32,
}
const _: () = assert!(std::mem::size_of::<SwkbdChangedStringArg>() == 0x10);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SwkbdMovedCursorArg {
    pub text_length: u32,
    pub cursor_position: i32,
}
const _: () = assert!(std::mem::size_of::<SwkbdMovedCursorArg>() == 0x8);

pub type SwkbdMovedTabArg = SwkbdMovedCursorArg;

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SwkbdDecidedEnterArg {
    pub text_length: u32,
}
const _: () = assert!(std::mem::size_of::<SwkbdDecidedEnterArg>() == 0x4);
