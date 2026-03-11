// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/bcat_util.h

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::bcat_result::RESULT_INVALID_ARGUMENT;

/// For a name to be valid it must be non-empty, must have a null terminating character as the final
/// char, can only contain numbers, letters, underscores and a hyphen if directory and a period if
/// file.
pub fn verify_name_valid_internal(name: &[u8; 0x20], match_char: u8) -> ResultCode {
    let null_chars = name.iter().filter(|&&c| c == 0).count();
    let bad_chars = name.iter().filter(|&&c| {
        !c.is_ascii_alphanumeric() && c != b'_' && c != match_char && c != 0
    }).count();

    if null_chars == 0x20 || null_chars == 0 || bad_chars != 0 || name[0x1F] != 0 {
        log::error!("BCAT: Name passed was invalid!");
        return RESULT_INVALID_ARGUMENT;
    }

    RESULT_SUCCESS
}

pub fn verify_name_valid_dir(name: &[u8; 0x20]) -> ResultCode {
    verify_name_valid_internal(name, b'-')
}

pub fn verify_name_valid_file(name: &[u8; 0x20]) -> ResultCode {
    verify_name_valid_internal(name, b'.')
}
