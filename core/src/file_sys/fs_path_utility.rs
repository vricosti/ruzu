// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fs_path_utility.h

use common::ResultCode;

use super::errors::*;
use super::fs_directory::ENTRY_NAME_LENGTH_MAX;
use super::fs_memory_management;
use super::fs_string_util::*;

/// Maximum mount name length.
pub const MOUNT_NAME_LENGTH_MAX: usize = 15;

/// String trait constants used throughout path processing.
/// Corresponds to C++ `StringTraits` namespace.
pub mod string_traits {
    pub const DIRECTORY_SEPARATOR: u8 = b'/';
    pub const DRIVE_SEPARATOR: u8 = b':';
    pub const DOT: u8 = b'.';
    pub const NULL_TERMINATOR: u8 = 0;
    pub const ALTERNATE_DIRECTORY_SEPARATOR: u8 = b'\\';

    pub const INVALID_CHARACTERS: [u8; 6] = [b':', b'*', b'?', b'<', b'>', b'|'];
    pub const INVALID_CHARACTERS_FOR_HOST_NAME: [u8; 6] = [b':', b'*', b'<', b'>', b'|', b'$'];
    pub const INVALID_CHARACTERS_FOR_MOUNT_NAME: [u8; 5] = [b'*', b'?', b'<', b'>', b'|'];

    /// Build a 64-bit bitmask for invalid character detection.
    /// Corresponds to C++ `MakeInvalidCharacterMask`.
    const fn make_invalid_character_mask(chars: &[u8], n: u64) -> u64 {
        let mut mask: u64 = 0;
        let mut i = 0;
        while i < chars.len() {
            if (chars[i] as u64 >> 6) == n {
                mask |= 1u64 << (chars[i] as u64 & 0x3F);
            }
            i += 1;
        }
        mask
    }

    /// Check if a character is in the given invalid set using bitmask lookup.
    fn is_invalid_character_impl(c: u8, chars: &[u8]) -> bool {
        let masks: [u64; 4] = [
            make_invalid_character_mask(chars, 0),
            make_invalid_character_mask(chars, 1),
            make_invalid_character_mask(chars, 2),
            make_invalid_character_mask(chars, 3),
        ];
        let idx = (c as u64) >> 6;
        let bit = (c as u64) & 0x3F;
        (masks[idx as usize] & (1u64 << bit)) != 0
    }

    pub fn is_invalid_character(c: u8) -> bool {
        is_invalid_character_impl(c, &INVALID_CHARACTERS)
    }

    pub fn is_invalid_character_for_host_name(c: u8) -> bool {
        is_invalid_character_impl(c, &INVALID_CHARACTERS_FOR_HOST_NAME)
    }

    pub fn is_invalid_character_for_mount_name(c: u8) -> bool {
        is_invalid_character_impl(c, &INVALID_CHARACTERS_FOR_MOUNT_NAME)
    }
}

use string_traits::*;

// -- Path length constants --

pub const WINDOWS_DRIVE_LENGTH: usize = 2;
pub const UNC_PATH_PREFIX_LENGTH: usize = 2;
pub const DOS_DEVICE_PATH_PREFIX_LENGTH: usize = 4;

// -- PathFlags --

/// Flags controlling what types of paths are allowed during normalization.
/// Corresponds to C++ `PathFlags` class.
#[derive(Debug, Clone, Copy)]
pub struct PathFlags {
    value: u32,
}

impl PathFlags {
    const WINDOWS_PATH_FLAG: u32 = 1 << 0;
    const RELATIVE_PATH_FLAG: u32 = 1 << 1;
    const EMPTY_PATH_FLAG: u32 = 1 << 2;
    const MOUNT_NAME_FLAG: u32 = 1 << 3;
    const BACKSLASH_FLAG: u32 = 1 << 4;
    const ALL_CHARACTERS_FLAG: u32 = 1 << 5;

    pub const fn new() -> Self {
        Self { value: 0 }
    }

    pub const fn is_windows_path_allowed(&self) -> bool {
        (self.value & Self::WINDOWS_PATH_FLAG) != 0
    }
    pub fn allow_windows_path(&mut self) {
        self.value |= Self::WINDOWS_PATH_FLAG;
    }

    pub const fn is_relative_path_allowed(&self) -> bool {
        (self.value & Self::RELATIVE_PATH_FLAG) != 0
    }
    pub fn allow_relative_path(&mut self) {
        self.value |= Self::RELATIVE_PATH_FLAG;
    }

    pub const fn is_empty_path_allowed(&self) -> bool {
        (self.value & Self::EMPTY_PATH_FLAG) != 0
    }
    pub fn allow_empty_path(&mut self) {
        self.value |= Self::EMPTY_PATH_FLAG;
    }

    pub const fn is_mount_name_allowed(&self) -> bool {
        (self.value & Self::MOUNT_NAME_FLAG) != 0
    }
    pub fn allow_mount_name(&mut self) {
        self.value |= Self::MOUNT_NAME_FLAG;
    }

    pub const fn is_backslash_allowed(&self) -> bool {
        (self.value & Self::BACKSLASH_FLAG) != 0
    }
    pub fn allow_backslash(&mut self) {
        self.value |= Self::BACKSLASH_FLAG;
    }

    pub const fn is_all_characters_allowed(&self) -> bool {
        (self.value & Self::ALL_CHARACTERS_FLAG) != 0
    }
    pub fn allow_all_characters(&mut self) {
        self.value |= Self::ALL_CHARACTERS_FLAG;
    }
}

impl Default for PathFlags {
    fn default() -> Self {
        Self::new()
    }
}

// -- Path helper functions --

/// Check if a path starts with a DOS device prefix (`\\.\` or `\\?\`).
/// Corresponds to C++ `IsDosDevicePath`.
pub fn is_dos_device_path(path: &[u8]) -> bool {
    path.len() >= 4
        && path[0] == ALTERNATE_DIRECTORY_SEPARATOR
        && path[1] == ALTERNATE_DIRECTORY_SEPARATOR
        && (path[2] == DOT || path[2] == b'?')
        && (path[3] == DIRECTORY_SEPARATOR || path[3] == ALTERNATE_DIRECTORY_SEPARATOR)
}

/// Check if a path starts with a UNC prefix (`//` or `\\`).
/// Corresponds to C++ `IsUncPath`.
pub fn is_unc_path(path: &[u8], allow_forward_slash: bool, allow_back_slash: bool) -> bool {
    path.len() >= 2
        && ((allow_forward_slash
            && path[0] == DIRECTORY_SEPARATOR
            && path[1] == DIRECTORY_SEPARATOR)
            || (allow_back_slash
                && path[0] == ALTERNATE_DIRECTORY_SEPARATOR
                && path[1] == ALTERNATE_DIRECTORY_SEPARATOR))
}

/// Default UNC check (both slash types allowed).
pub fn is_unc_path_default(path: &[u8]) -> bool {
    is_unc_path(path, true, true)
}

/// Check if a path starts with a Windows drive letter (e.g., `C:`).
/// Corresponds to C++ `IsWindowsDrive`.
pub fn is_windows_drive(path: &[u8]) -> bool {
    path.len() >= 2
        && (path[0].is_ascii_alphabetic())
        && path[1] == DRIVE_SEPARATOR
}

/// Check if a path is a Windows-style path.
/// Corresponds to C++ `IsWindowsPath`.
pub fn is_windows_path(path: &[u8], allow_forward_slash_unc: bool) -> bool {
    is_windows_drive(path)
        || is_dos_device_path(path)
        || is_unc_path(path, allow_forward_slash_unc, true)
}

/// Get the number of leading characters to skip for a Windows path prefix.
/// Corresponds to C++ `GetWindowsSkipLength`.
pub fn get_windows_skip_length(path: &[u8]) -> usize {
    if is_dos_device_path(path) {
        DOS_DEVICE_PATH_PREFIX_LENGTH
    } else if is_windows_drive(path) {
        WINDOWS_DRIVE_LENGTH
    } else if is_unc_path_default(path) {
        UNC_PATH_PREFIX_LENGTH
    } else {
        0
    }
}

/// Check if a path is absolute.
/// Corresponds to C++ `IsPathAbsolute`.
pub fn is_path_absolute(path: &[u8]) -> bool {
    is_windows_path(path, false) || (!path.is_empty() && path[0] == DIRECTORY_SEPARATOR)
}

/// Check if a path is relative.
/// Corresponds to C++ `IsPathRelative`.
pub fn is_path_relative(path: &[u8]) -> bool {
    !path.is_empty() && path[0] != NULL_TERMINATOR && !is_path_absolute(path)
}

/// Check if a path component is the current directory (`.` or `./`).
/// Corresponds to C++ `IsCurrentDirectory`.
pub fn is_current_directory(path: &[u8]) -> bool {
    !path.is_empty()
        && path[0] == DOT
        && (path.len() == 1
            || path[1] == NULL_TERMINATOR
            || path[1] == DIRECTORY_SEPARATOR)
}

/// Check if a path component is the parent directory (`..` or `../`).
/// Corresponds to C++ `IsParentDirectory`.
pub fn is_parent_directory(path: &[u8]) -> bool {
    path.len() >= 2
        && path[0] == DOT
        && path[1] == DOT
        && (path.len() == 2
            || path[2] == NULL_TERMINATOR
            || path[2] == DIRECTORY_SEPARATOR)
}

/// Check if a path starts with current or parent directory.
/// Corresponds to C++ `IsPathStartWithCurrentDirectory`.
pub fn is_path_start_with_current_directory(path: &[u8]) -> bool {
    is_current_directory(path) || is_parent_directory(path)
}

/// Check if one path is a sub-path of the other.
/// Corresponds to C++ `IsSubPath`.
pub fn is_sub_path(lhs: &[u8], rhs: &[u8]) -> bool {
    // Special case UNC paths
    if is_unc_path_default(lhs) && !is_unc_path_default(rhs) {
        return false;
    }
    if !is_unc_path_default(lhs) && is_unc_path_default(rhs) {
        return false;
    }

    // "/" is a sub-path of any non-root path starting with "/"
    let lhs_len = strlen(lhs);
    let rhs_len = strlen(rhs);

    if lhs_len >= 1
        && lhs[0] == DIRECTORY_SEPARATOR
        && (lhs_len == 1 || lhs[1] == NULL_TERMINATOR)
        && rhs_len >= 1
        && rhs[0] == DIRECTORY_SEPARATOR
        && rhs_len >= 2
        && rhs[1] != NULL_TERMINATOR
    {
        return true;
    }
    if rhs_len >= 1
        && rhs[0] == DIRECTORY_SEPARATOR
        && (rhs_len == 1 || rhs[1] == NULL_TERMINATOR)
        && lhs_len >= 1
        && lhs[0] == DIRECTORY_SEPARATOR
        && lhs_len >= 2
        && lhs[1] != NULL_TERMINATOR
    {
        return true;
    }

    // Character-by-character comparison
    let mut i = 0;
    loop {
        let l = if i < lhs.len() { lhs[i] } else { NULL_TERMINATOR };
        let r = if i < rhs.len() { rhs[i] } else { NULL_TERMINATOR };

        if l == NULL_TERMINATOR {
            return r == DIRECTORY_SEPARATOR;
        } else if r == NULL_TERMINATOR {
            return l == DIRECTORY_SEPARATOR;
        } else if l != r {
            return false;
        }
        i += 1;
    }
}

/// Replace occurrences of `old_char` with `new_char` in a mutable byte slice.
/// Corresponds to C++ `Replace`.
pub fn replace(dst: &mut [u8], dst_size: usize, old_char: u8, new_char: u8) {
    let limit = dst_size.min(dst.len());
    for b in dst[..limit].iter_mut() {
        if *b == NULL_TERMINATOR {
            break;
        }
        if *b == old_char {
            *b = new_char;
        }
    }
}

/// Check UTF-8 validity of a null-terminated path.
/// Corresponds to C++ `CheckUtf8`.
pub fn check_utf8(s: &[u8]) -> Result<(), ResultCode> {
    let mut pos = 0;
    while pos < s.len() && s[pos] != NULL_TERMINATOR {
        let (pick_res, utf8_buf, consumed) = pick_out_character_from_utf8_string(s, pos);
        if pick_res != CharacterEncodingResult::Success {
            return Err(RESULT_INVALID_PATH_FORMAT);
        }

        let (cvt_res, _) = convert_character_utf8_to_utf32(&utf8_buf);
        if cvt_res != CharacterEncodingResult::Success {
            return Err(RESULT_INVALID_PATH_FORMAT);
        }

        pos += consumed;
    }

    Ok(())
}

// -- PathNormalizer --

/// Path normalizer implementing path state machine normalization.
/// Corresponds to C++ `PathNormalizer` class.
pub struct PathNormalizer;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PathState {
    Start,
    Normal,
    FirstSeparator,
    Separator,
    CurrentDir,
    ParentDir,
}

impl PathNormalizer {
    /// Replace parent directory path patterns involving backslashes.
    /// Corresponds to C++ `ReplaceParentDirectoryPath`.
    fn replace_parent_directory_path(dst: &mut [u8], src: &[u8]) {
        dst[0] = DIRECTORY_SEPARATOR;

        let mut i: usize = 1;
        while i < src.len() && src[i] != NULL_TERMINATOR {
            if (src[i - 1] == DIRECTORY_SEPARATOR || src[i - 1] == ALTERNATE_DIRECTORY_SEPARATOR)
                && i + 2 < src.len()
                && src[i] == DOT
                && src[i + 1] == DOT
                && (src[i + 2] == DIRECTORY_SEPARATOR || src[i + 2] == ALTERNATE_DIRECTORY_SEPARATOR)
            {
                dst[i - 1] = DIRECTORY_SEPARATOR;
                dst[i] = DOT;
                dst[i + 1] = DOT;
                dst[i + 2] = DIRECTORY_SEPARATOR;
                i += 3;
            } else {
                if src[i - 1] == ALTERNATE_DIRECTORY_SEPARATOR
                    && i + 2 < src.len()
                    && src[i] == DOT
                    && src[i + 1] == DOT
                    && (i + 2 >= src.len() || src[i + 2] == NULL_TERMINATOR)
                {
                    dst[i - 1] = DIRECTORY_SEPARATOR;
                    dst[i] = DOT;
                    dst[i + 1] = DOT;
                    i += 2;
                    break;
                }

                dst[i] = src[i];
                i += 1;
            }
        }

        if i < dst.len() {
            dst[i] = NULL_TERMINATOR;
        }
    }

    /// Check if parent directory path replacement is needed.
    /// Corresponds to C++ `IsParentDirectoryPathReplacementNeeded`.
    pub fn is_parent_directory_path_replacement_needed(path: &[u8]) -> bool {
        if path.is_empty()
            || (path[0] != DIRECTORY_SEPARATOR && path[0] != ALTERNATE_DIRECTORY_SEPARATOR)
        {
            return false;
        }

        if path.len() < 4 {
            return false;
        }

        let mut p = 0;
        while p + 3 < path.len() && path[p + 3] != NULL_TERMINATOR {
            if path[p + 1] == DOT && path[p + 2] == DOT {
                let c0 = path[p];
                let c3 = path[p + 3];

                if c0 == ALTERNATE_DIRECTORY_SEPARATOR
                    && (c3 == DIRECTORY_SEPARATOR
                        || c3 == ALTERNATE_DIRECTORY_SEPARATOR
                        || c3 == NULL_TERMINATOR)
                {
                    return true;
                }

                if c3 == ALTERNATE_DIRECTORY_SEPARATOR
                    && (c0 == DIRECTORY_SEPARATOR || c0 == ALTERNATE_DIRECTORY_SEPARATOR)
                {
                    return true;
                }
            }
            p += 1;
        }

        // Check the last triplet
        if p + 2 < path.len()
            && path[p] == ALTERNATE_DIRECTORY_SEPARATOR
            && path[p + 1] == DOT
            && path[p + 2] == DOT
        {
            return true;
        }

        false
    }

    /// Check if a path is already normalized (basic check without flags).
    /// Returns (is_normalized, path_length).
    /// Corresponds to C++ `PathNormalizer::IsNormalized`.
    pub fn is_normalized(
        path: &[u8],
        allow_all_characters: bool,
    ) -> Result<(bool, usize), ResultCode> {
        let mut state = PathState::Start;
        let mut len: usize = 0;

        while len < path.len() && path[len] != NULL_TERMINATOR {
            let c = path[len];
            len += 1;

            // Check for invalid characters
            if !allow_all_characters && state != PathState::Start {
                if is_invalid_character(c) {
                    return Err(RESULT_INVALID_CHARACTER);
                }
            }

            match state {
                PathState::Start => {
                    if c != DIRECTORY_SEPARATOR {
                        return Err(RESULT_INVALID_PATH_FORMAT);
                    }
                    state = PathState::FirstSeparator;
                }
                PathState::Normal => {
                    if c == DIRECTORY_SEPARATOR {
                        state = PathState::Separator;
                    }
                }
                PathState::FirstSeparator | PathState::Separator => {
                    if c == DIRECTORY_SEPARATOR {
                        return Ok((false, len));
                    }
                    if c == DOT {
                        state = PathState::CurrentDir;
                    } else {
                        state = PathState::Normal;
                    }
                }
                PathState::CurrentDir => {
                    if c == DIRECTORY_SEPARATOR {
                        return Ok((false, len));
                    }
                    if c == DOT {
                        state = PathState::ParentDir;
                    } else {
                        state = PathState::Normal;
                    }
                }
                PathState::ParentDir => {
                    if c == DIRECTORY_SEPARATOR {
                        return Ok((false, len));
                    }
                    state = PathState::Normal;
                }
            }
        }

        // Check final state
        match state {
            PathState::Start => Err(RESULT_INVALID_PATH_FORMAT),
            PathState::Normal | PathState::FirstSeparator => Ok((true, len)),
            PathState::Separator | PathState::CurrentDir | PathState::ParentDir => {
                Ok((false, len))
            }
        }
    }

    /// Normalize a path into `dst`.
    /// Returns the output length.
    /// Corresponds to C++ `PathNormalizer::Normalize`.
    pub fn normalize(
        dst: &mut [u8],
        path: &[u8],
        max_out_size: usize,
        is_windows_path: bool,
        is_drive_relative_path: bool,
        allow_all_characters: bool,
    ) -> Result<usize, ResultCode> {
        let mut cur_path: Vec<u8>;
        let path_ref: &[u8];

        let mut total_len: usize = 0;

        // If path begins with a separator, check that we're not drive relative
        if path.is_empty() || path[0] != DIRECTORY_SEPARATOR {
            if !is_drive_relative_path {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            if total_len < max_out_size {
                dst[total_len] = DIRECTORY_SEPARATOR;
            }
            total_len += 1;
        }

        // Perform path replacement if necessary
        if Self::is_parent_directory_path_replacement_needed(path) {
            cur_path = vec![0u8; ENTRY_NAME_LENGTH_MAX + 1];
            Self::replace_parent_directory_path(&mut cur_path, path);
            path_ref = &cur_path;
        } else {
            cur_path = Vec::new(); // unused
            path_ref = path;
        }
        let _ = cur_path; // suppress warning

        // Iterate, normalizing path components
        let mut skip_next_sep = false;
        let mut i: usize = 0;

        while i < path_ref.len() && path_ref[i] != NULL_TERMINATOR {
            if path_ref[i] == DIRECTORY_SEPARATOR {
                // Swallow separators
                while i < path_ref.len() && path_ref[i] == DIRECTORY_SEPARATOR {
                    i += 1;
                }

                // Check end of string
                if i >= path_ref.len() || path_ref[i] == NULL_TERMINATOR {
                    break;
                }

                if !skip_next_sep {
                    if total_len + 1 == max_out_size {
                        if total_len < dst.len() {
                            dst[total_len] = NULL_TERMINATOR;
                        }
                        return Err(RESULT_TOO_LONG_PATH);
                    }

                    if total_len < dst.len() {
                        dst[total_len] = DIRECTORY_SEPARATOR;
                    }
                    total_len += 1;
                }

                skip_next_sep = false;
            }

            // Get the length of the current directory component
            let mut dir_len: usize = 0;
            while i + dir_len < path_ref.len()
                && path_ref[i + dir_len] != DIRECTORY_SEPARATOR
                && path_ref[i + dir_len] != NULL_TERMINATOR
            {
                if !allow_all_characters && is_invalid_character(path_ref[i + dir_len]) {
                    return Err(RESULT_INVALID_CHARACTER);
                }
                dir_len += 1;
            }

            // Handle current directory component
            if is_current_directory(&path_ref[i..]) {
                skip_next_sep = true;
            } else if is_parent_directory(&path_ref[i..]) {
                // We should have just written a separator
                debug_assert!(total_len > 0 && dst[total_len - 1] == DIRECTORY_SEPARATOR);

                if !is_windows_path {
                    debug_assert!(dst[0] == DIRECTORY_SEPARATOR);
                }

                // Remove the previous component
                if total_len == 1 {
                    if !is_windows_path {
                        return Err(RESULT_DIRECTORY_UNOBTAINABLE);
                    }
                    total_len -= 1;
                } else {
                    total_len -= 2;
                    loop {
                        if dst[total_len] == DIRECTORY_SEPARATOR {
                            break;
                        }
                        if total_len == 0 {
                            break;
                        }
                        total_len -= 1;
                    }
                }

                if !is_windows_path {
                    debug_assert!(dst[total_len] == DIRECTORY_SEPARATOR);
                }
                debug_assert!(total_len < max_out_size);
            } else {
                // Copy, possibly truncating
                if total_len + dir_len + 1 > max_out_size {
                    let copy_len = max_out_size.saturating_sub(total_len + 1);
                    for j in 0..copy_len {
                        if total_len < dst.len() {
                            dst[total_len] = path_ref[i + j];
                        }
                        total_len += 1;
                    }
                    if total_len < dst.len() {
                        dst[total_len] = NULL_TERMINATOR;
                    }
                    return Err(RESULT_TOO_LONG_PATH);
                }

                for j in 0..dir_len {
                    if total_len < dst.len() {
                        dst[total_len] = path_ref[i + j];
                    }
                    total_len += 1;
                }
            }

            i += dir_len;
        }

        if skip_next_sep && total_len > 0 {
            total_len -= 1;
        }

        if total_len == 0 && max_out_size != 0 {
            total_len = 1;
            dst[0] = DIRECTORY_SEPARATOR;
        }

        // NOTE: Probable nintendo bug, as max_out_size must be at least total_len + 1
        if max_out_size < total_len.saturating_sub(1) {
            return Err(RESULT_TOO_LONG_PATH);
        }

        if total_len < dst.len() {
            dst[total_len] = NULL_TERMINATOR;
        }

        // Check that result is normalized
        let (is_norm, _) = Self::is_normalized(dst, allow_all_characters)?;
        debug_assert!(is_norm);

        Ok(total_len)
    }
}

// -- PathFormatter --

/// Path formatter implementing full path normalization with flags.
/// Corresponds to C++ `PathFormatter` class.
pub struct PathFormatter;

impl PathFormatter {
    /// Check shared name validity.
    fn check_shared_name(name: &[u8], len: usize) -> Result<(), ResultCode> {
        if len == 1 {
            if name[0] == DOT {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
        } else if len == 2 && name[0] == DOT && name[1] == DOT {
            return Err(RESULT_INVALID_PATH_FORMAT);
        }

        for i in 0..len {
            if is_invalid_character(name[i]) {
                return Err(RESULT_INVALID_CHARACTER);
            }
        }

        Ok(())
    }

    /// Check host name validity.
    fn check_host_name(name: &[u8], len: usize) -> Result<(), ResultCode> {
        if len == 2 && name[0] == DOT && name[1] == DOT {
            return Err(RESULT_INVALID_PATH_FORMAT);
        }

        for i in 0..len {
            if is_invalid_character_for_host_name(name[i]) {
                return Err(RESULT_INVALID_CHARACTER);
            }
        }

        Ok(())
    }

    /// Check for invalid backslash characters in a path.
    /// Returns whether backslashes were found.
    fn check_invalid_backslash(path: &[u8], allow_backslash: bool) -> Result<bool, ResultCode> {
        let mut contains_backslash = false;
        let mut i = 0;
        while i < path.len() && path[i] != NULL_TERMINATOR {
            if path[i] == ALTERNATE_DIRECTORY_SEPARATOR {
                contains_backslash = true;
                if !allow_backslash {
                    return Err(RESULT_INVALID_CHARACTER);
                }
            }
            i += 1;
        }
        Ok(contains_backslash)
    }

    /// Check path format.
    /// Corresponds to C++ `PathFormatter::CheckPathFormat`.
    pub fn check_path_format(path: &[u8], flags: &PathFlags) -> Result<(), ResultCode> {
        let (_normalized, _len) = Self::is_normalized(path, flags)?;
        Ok(())
    }

    /// Skip past a mount name prefix.
    /// Returns (remaining_path_offset, mount_name_length).
    /// Corresponds to C++ `PathFormatter::SkipMountName`.
    pub fn skip_mount_name(path: &[u8]) -> Result<(usize, usize), ResultCode> {
        Self::parse_mount_name(path, None)
    }

    /// Parse a mount name from the beginning of a path.
    /// If `out_mount_name` is Some, the mount name is copied into it.
    /// Returns (remaining_path_offset, mount_name_length).
    /// Corresponds to C++ `PathFormatter::ParseMountName`.
    pub fn parse_mount_name(
        path: &[u8],
        out_mount_name: Option<&mut Vec<u8>>,
    ) -> Result<(usize, usize), ResultCode> {
        let has_out = out_mount_name.is_some();
        let max_mount_len = if !has_out {
            MOUNT_NAME_LENGTH_MAX + 1
        } else {
            MOUNT_NAME_LENGTH_MAX + 1
        };

        let mut mount_len: usize = 0;
        while mount_len < max_mount_len && mount_len < path.len() && path[mount_len] != NULL_TERMINATOR {
            let c = path[mount_len];

            if c == DRIVE_SEPARATOR {
                mount_len += 1;
                break;
            }

            if c == DIRECTORY_SEPARATOR || c == ALTERNATE_DIRECTORY_SEPARATOR {
                // Not a mount name
                return Ok((0, 0));
            }
            mount_len += 1;
        }

        // Check if we actually have a mount name
        if mount_len <= 2
            || mount_len > path.len()
            || path[mount_len - 1] != DRIVE_SEPARATOR
        {
            return Ok((0, 0));
        }

        // Check all characters are allowable
        for i in 0..mount_len {
            if is_invalid_character_for_mount_name(path[i]) {
                return Err(RESULT_INVALID_CHARACTER);
            }
        }

        // Copy out the mount name
        if let Some(out) = out_mount_name {
            out.clear();
            out.extend_from_slice(&path[..mount_len]);
            out.push(NULL_TERMINATOR);
        }

        Ok((mount_len, mount_len))
    }

    /// Skip past a relative dot path prefix.
    /// Returns (remaining_path_offset, relative_length).
    /// Corresponds to C++ `PathFormatter::SkipRelativeDotPath`.
    pub fn skip_relative_dot_path(path: &[u8]) -> Result<(usize, usize), ResultCode> {
        Self::parse_relative_dot_path(path, None)
    }

    /// Parse a relative dot path prefix.
    /// Returns (remaining_path_offset, relative_length).
    /// Corresponds to C++ `PathFormatter::ParseRelativeDotPath`.
    pub fn parse_relative_dot_path(
        path: &[u8],
        out_relative: Option<&mut Vec<u8>>,
    ) -> Result<(usize, usize), ResultCode> {
        if let Some(out) = out_relative.as_ref() {
            let _ = out;
        }

        // Check if the path is relative
        if !path.is_empty()
            && path[0] == DOT
            && (path.len() == 1
                || path[1] == NULL_TERMINATOR
                || path[1] == DIRECTORY_SEPARATOR
                || path[1] == ALTERNATE_DIRECTORY_SEPARATOR)
        {
            if let Some(out) = out_relative {
                out.clear();
                out.push(DOT);
                out.push(NULL_TERMINATOR);
            }
            return Ok((1, 1));
        }

        // Ensure the path isn't a parent directory
        if path.len() >= 2 && path[0] == DOT && path[1] == DOT {
            return Err(RESULT_DIRECTORY_UNOBTAINABLE);
        }

        Ok((0, 0))
    }

    /// Skip past a Windows path prefix, reporting normalization status.
    /// Returns (remaining_path_offset, windows_path_length, is_normalized).
    /// Corresponds to C++ `PathFormatter::SkipWindowsPath`.
    pub fn skip_windows_path(
        path: &[u8],
        has_mount_name: bool,
    ) -> Result<(usize, usize, bool), ResultCode> {
        match Self::parse_windows_path(path, None, has_mount_name) {
            Ok((offset, len)) => Ok((offset, len, true)),
            Err(e) if e == RESULT_NOT_NORMALIZED => Ok((0, 0, false)),
            Err(e) => Err(e),
        }
    }

    /// Parse a Windows path prefix.
    /// Returns (remaining_path_offset, windows_path_length).
    /// Corresponds to C++ `PathFormatter::ParseWindowsPath`.
    pub fn parse_windows_path(
        path: &[u8],
        out_win: Option<&mut Vec<u8>>,
        has_mount_name: bool,
    ) -> Result<(usize, usize), ResultCode> {
        let has_out = out_win.is_some();

        if let Some(out) = out_win.as_ref() {
            let _ = out;
        }

        // Handle path start
        let mut cur_offset: usize = 0;
        if has_mount_name
            && !path.is_empty()
            && path[0] == DIRECTORY_SEPARATOR
        {
            if path.len() >= 3
                && path[1] == ALTERNATE_DIRECTORY_SEPARATOR
                && path[2] == ALTERNATE_DIRECTORY_SEPARATOR
            {
                if !has_out {
                    return Err(RESULT_NOT_NORMALIZED);
                }
                cur_offset = 1;
            } else if path.len() >= 3 && is_windows_drive(&path[1..]) {
                if !has_out {
                    return Err(RESULT_NOT_NORMALIZED);
                }
                cur_offset = 1;
            }
        }

        let cur_path = &path[cur_offset..];

        // Handle windows drive
        if is_windows_drive(cur_path) {
            let mut win_path_len = WINDOWS_DRIVE_LENGTH;
            while win_path_len < cur_path.len() && cur_path[win_path_len] != NULL_TERMINATOR {
                if is_invalid_character(cur_path[win_path_len]) {
                    return Err(RESULT_INVALID_CHARACTER);
                }
                if cur_path[win_path_len] == DIRECTORY_SEPARATOR
                    || cur_path[win_path_len] == ALTERNATE_DIRECTORY_SEPARATOR
                {
                    break;
                }
                win_path_len += 1;
            }

            if !has_out {
                for i in 0..win_path_len {
                    if cur_path[i] == ALTERNATE_DIRECTORY_SEPARATOR {
                        return Err(RESULT_NOT_NORMALIZED);
                    }
                }
            } else if let Some(out) = out_win {
                out.clear();
                out.extend_from_slice(&cur_path[..win_path_len]);
                out.push(NULL_TERMINATOR);
                replace(out, win_path_len, ALTERNATE_DIRECTORY_SEPARATOR, DIRECTORY_SEPARATOR);
            }

            return Ok((cur_offset + win_path_len, win_path_len));
        }

        // Handle DOS device path
        if is_dos_device_path(cur_path) {
            let mut dos_prefix_len = DOS_DEVICE_PATH_PREFIX_LENGTH;

            if cur_path.len() >= dos_prefix_len + WINDOWS_DRIVE_LENGTH
                && is_windows_drive(&cur_path[dos_prefix_len..])
            {
                dos_prefix_len += WINDOWS_DRIVE_LENGTH;
            } else {
                dos_prefix_len -= 1;
            }

            if let Some(out) = out_win {
                out.clear();
                out.extend_from_slice(&cur_path[..dos_prefix_len]);
                out.push(NULL_TERMINATOR);
                replace(out, dos_prefix_len, DIRECTORY_SEPARATOR, ALTERNATE_DIRECTORY_SEPARATOR);
            }

            return Ok((cur_offset + dos_prefix_len, dos_prefix_len));
        }

        // Handle UNC path
        if is_unc_path(cur_path, false, true) {
            let mut final_offset = 0usize;

            if cur_path.len() > UNC_PATH_PREFIX_LENGTH {
                if cur_path[UNC_PATH_PREFIX_LENGTH] == DIRECTORY_SEPARATOR {
                    return Err(RESULT_INVALID_PATH_FORMAT);
                }
                if cur_path[UNC_PATH_PREFIX_LENGTH] == ALTERNATE_DIRECTORY_SEPARATOR {
                    return Err(RESULT_INVALID_PATH_FORMAT);
                }
            }

            let mut cur_component_offset: usize = 0;
            let mut pos = UNC_PATH_PREFIX_LENGTH;
            while pos < cur_path.len() && cur_path[pos] != NULL_TERMINATOR {
                if cur_path[pos] == DIRECTORY_SEPARATOR
                    || cur_path[pos] == ALTERNATE_DIRECTORY_SEPARATOR
                {
                    if cur_component_offset != 0 {
                        Self::check_shared_name(
                            &cur_path[cur_component_offset..],
                            pos - cur_component_offset,
                        )?;
                        final_offset = pos;
                        break;
                    }

                    if pos + 1 < cur_path.len() && cur_path[pos + 1] == DIRECTORY_SEPARATOR {
                        return Err(RESULT_INVALID_PATH_FORMAT);
                    }
                    if pos + 1 < cur_path.len()
                        && cur_path[pos + 1] == ALTERNATE_DIRECTORY_SEPARATOR
                    {
                        return Err(RESULT_INVALID_PATH_FORMAT);
                    }

                    Self::check_host_name(&cur_path[2..], pos - 2)?;
                    cur_component_offset = pos + 1;
                }
                pos += 1;
            }

            if cur_component_offset == pos {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }

            if cur_component_offset != 0 && final_offset == 0 {
                Self::check_shared_name(
                    &cur_path[cur_component_offset..],
                    pos - cur_component_offset,
                )?;
                final_offset = pos;
            }

            let unc_prefix_len = final_offset;

            if !has_out {
                for i in 0..unc_prefix_len {
                    if cur_path[i] == DIRECTORY_SEPARATOR {
                        return Err(RESULT_NOT_NORMALIZED);
                    }
                }
            } else if let Some(out) = out_win {
                out.clear();
                out.extend_from_slice(&cur_path[..unc_prefix_len]);
                out.push(NULL_TERMINATOR);
                replace(out, unc_prefix_len, DIRECTORY_SEPARATOR, ALTERNATE_DIRECTORY_SEPARATOR);
            }

            return Ok((cur_offset + unc_prefix_len, unc_prefix_len));
        }

        // No windows path
        Ok((0, 0))
    }

    /// Check whether a path is normalized according to the given flags.
    /// Returns (is_normalized, total_path_length).
    /// Corresponds to C++ `PathFormatter::IsNormalized`.
    pub fn is_normalized(
        path: &[u8],
        flags: &PathFlags,
    ) -> Result<(bool, usize), ResultCode> {
        // Verify that the path is valid UTF-8
        check_utf8(path)?;

        // Handle empty path
        if path.is_empty() || path[0] == NULL_TERMINATOR {
            if !flags.is_empty_path_allowed() {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            return Ok((true, 0));
        }

        // Normalized paths usually start with '/'
        if path[0] != DIRECTORY_SEPARATOR {
            if !flags.is_windows_path_allowed()
                && !flags.is_relative_path_allowed()
                && !flags.is_mount_name_allowed()
            {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
        }

        // Check windows path
        if is_windows_path(path, false) && !flags.is_windows_path_allowed() {
            return Err(RESULT_INVALID_PATH_FORMAT);
        }

        // Skip mount name
        let mut total_len: usize = 0;
        let (mount_offset, mount_name_len) = Self::skip_mount_name(path)?;
        let mut cur_path = &path[mount_offset..];

        if mount_name_len > 0 {
            if !flags.is_mount_name_allowed() {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            total_len += mount_name_len;
        }

        // Check path start
        if !cur_path.is_empty()
            && cur_path[0] != NULL_TERMINATOR
            && cur_path[0] != DIRECTORY_SEPARATOR
            && !is_path_start_with_current_directory(cur_path)
            && !is_windows_path(cur_path, false)
        {
            if !flags.is_relative_path_allowed() {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            if is_invalid_character(cur_path[0]) {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            return Ok((false, total_len));
        }

        // Process relative path
        let (rel_offset, relative_len) = Self::skip_relative_dot_path(cur_path)?;
        cur_path = &cur_path[rel_offset..];

        if relative_len > 0 {
            if !flags.is_relative_path_allowed() {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            total_len += relative_len;

            if cur_path.is_empty() || cur_path[0] == NULL_TERMINATOR {
                return Ok((true, total_len));
            }
        }

        // Process windows path
        let (win_offset, windows_len, normalized_win) =
            Self::skip_windows_path(cur_path, mount_name_len > 0)?;

        if !normalized_win {
            if !flags.is_windows_path_allowed() {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            return Ok((false, total_len));
        }

        cur_path = &cur_path[win_offset..];

        if windows_len > 0 {
            if !flags.is_windows_path_allowed() {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            total_len += windows_len;

            if relative_len != 0 {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }

            if cur_path.is_empty() || cur_path[0] == NULL_TERMINATOR {
                return Ok((false, total_len));
            }

            // Check for alternate separators
            let mut i = 0;
            while i < cur_path.len() && cur_path[i] != NULL_TERMINATOR {
                if cur_path[i] == ALTERNATE_DIRECTORY_SEPARATOR {
                    return Ok((false, total_len));
                }
                i += 1;
            }
        }

        // Check parent directory replacement
        if flags.is_backslash_allowed()
            && PathNormalizer::is_parent_directory_path_replacement_needed(cur_path)
        {
            return Ok((false, total_len));
        }

        // Check backslash state
        let is_backslash_contained = Self::check_invalid_backslash(
            cur_path,
            flags.is_windows_path_allowed() || flags.is_backslash_allowed(),
        )?;

        if is_backslash_contained && !flags.is_backslash_allowed() {
            return Ok((false, total_len));
        }

        // Check final normalization
        let (is_norm, normal_len) =
            PathNormalizer::is_normalized(cur_path, flags.is_all_characters_allowed())?;

        total_len += normal_len;
        Ok((is_norm, total_len))
    }

    /// Normalize a path into `dst` according to the given flags.
    /// Corresponds to C++ `PathFormatter::Normalize`.
    pub fn normalize(
        dst: &mut [u8],
        dst_size: usize,
        path: &[u8],
        path_len: usize,
        flags: &PathFlags,
    ) -> Result<(), ResultCode> {
        let mut src = path;
        let mut src_offset: usize = 0;
        let mut cur_pos: usize = 0;
        let mut is_windows_path_found = false;

        // Check empty path
        if src.is_empty() || src[0] == NULL_TERMINATOR {
            if dst_size != 0 && !dst.is_empty() {
                dst[0] = NULL_TERMINATOR;
            }
            if !flags.is_empty_path_allowed() {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            return Ok(());
        }

        // Handle mount name
        let mut mount_name_len: usize = 0;
        if flags.is_mount_name_allowed() {
            let mut mount_buf = Vec::new();
            let (offset, mlen) = Self::parse_mount_name(src, Some(&mut mount_buf))?;
            if mlen > 0 && !mount_buf.is_empty() {
                // Copy mount name into dst
                let copy_len = mlen.min(dst_size.saturating_sub(cur_pos));
                dst[cur_pos..cur_pos + copy_len].copy_from_slice(&mount_buf[..copy_len]);
                cur_pos += mlen;
            }
            mount_name_len = mlen;
            src_offset = offset;
            src = &path[src_offset..];
        }

        // Handle drive-relative prefix
        let mut is_drive_relative = false;
        if !src.is_empty()
            && src[0] != NULL_TERMINATOR
            && src[0] != DIRECTORY_SEPARATOR
            && !is_path_start_with_current_directory(src)
            && !is_windows_path_fn(src, false)
        {
            if !flags.is_relative_path_allowed() {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            if is_invalid_character(src[0]) {
                return Err(RESULT_INVALID_PATH_FORMAT);
            }
            if cur_pos < dst.len() {
                dst[cur_pos] = DOT;
            }
            cur_pos += 1;
            is_drive_relative = true;
        }

        // Handle relative path
        let mut relative_len: usize = 0;
        if flags.is_relative_path_allowed() {
            if cur_pos >= dst_size {
                return Err(RESULT_TOO_LONG_PATH);
            }

            let mut rel_buf = Vec::new();
            let (rel_offset, rlen) = Self::parse_relative_dot_path(src, Some(&mut rel_buf))?;
            if rlen > 0 && !rel_buf.is_empty() {
                let copy_len = (rel_buf.len() - 1).min(dst_size.saturating_sub(cur_pos));
                dst[cur_pos..cur_pos + copy_len].copy_from_slice(&rel_buf[..copy_len]);
            }
            relative_len = rlen;
            cur_pos += rlen;
            src = &src[rel_offset..];

            if src.is_empty() || src[0] == NULL_TERMINATOR {
                if cur_pos >= dst_size {
                    return Err(RESULT_TOO_LONG_PATH);
                }
                if cur_pos < dst.len() {
                    dst[cur_pos] = NULL_TERMINATOR;
                }
                return Ok(());
            }
        }

        // Handle windows path
        if flags.is_windows_path_allowed() {
            let orig_src = src;

            if cur_pos >= dst_size {
                return Err(RESULT_TOO_LONG_PATH);
            }

            let mut win_buf = Vec::new();
            let (win_offset, windows_len) =
                Self::parse_windows_path(src, Some(&mut win_buf), mount_name_len != 0)?;
            if windows_len > 0 && !win_buf.is_empty() {
                let copy_len = (win_buf.len() - 1).min(dst_size.saturating_sub(cur_pos));
                dst[cur_pos..cur_pos + copy_len].copy_from_slice(&win_buf[..copy_len]);
            }
            cur_pos += windows_len;
            src = &src[win_offset..];

            if src.is_empty() || src[0] == NULL_TERMINATOR {
                // NOTE: Bug in original code preserved
                if cur_pos + 1 >= dst_size {
                    return Err(RESULT_TOO_LONG_PATH);
                }
                if cur_pos < dst.len() {
                    dst[cur_pos] = DIRECTORY_SEPARATOR;
                }
                if cur_pos + 1 < dst.len() {
                    dst[cur_pos + 1] = NULL_TERMINATOR;
                }
                return Ok(());
            }

            if win_offset > 0 {
                is_windows_path_found = true;
            }
        }

        // Check invalid backslash
        let backslash_contained = Self::check_invalid_backslash(
            src,
            flags.is_windows_path_allowed() || flags.is_backslash_allowed(),
        )?;

        // Handle backslash replacement
        if backslash_contained && flags.is_windows_path_allowed() {
            let src_len = strlen(src);
            let mut replaced = vec![0u8; src_len + 1];
            replaced[..src_len].copy_from_slice(&src[..src_len]);
            replaced[src_len] = NULL_TERMINATOR;
            replace(&mut replaced, src_len, ALTERNATE_DIRECTORY_SEPARATOR, DIRECTORY_SEPARATOR);

            let _dummy = PathNormalizer::normalize(
                &mut dst[cur_pos..],
                &replaced,
                dst_size.saturating_sub(cur_pos),
                is_windows_path_found,
                is_drive_relative,
                flags.is_all_characters_allowed(),
            )?;
        } else {
            let _dummy = PathNormalizer::normalize(
                &mut dst[cur_pos..],
                src,
                dst_size.saturating_sub(cur_pos),
                is_windows_path_found,
                is_drive_relative,
                flags.is_all_characters_allowed(),
            )?;
        }

        Ok(())
    }
}

/// Wrapper to avoid name collision with `is_windows_path` imported from module level.
fn is_windows_path_fn(path: &[u8], allow_forward_slash_unc: bool) -> bool {
    is_windows_path(path, allow_forward_slash_unc)
}
