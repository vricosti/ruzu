// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/video_helper.cpp
//!
//! Shared helpers for video (VMNMX, VMAD, VSETP) instruction translation.

use super::TranslatorVisitor;
use crate::ir::value::Value;

/// Width selector for video operand extraction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum VideoWidth {
    Byte = 0,
    Unknown = 1,
    Short = 2,
    Word = 3,
}

impl VideoWidth {
    pub fn from_u32(v: u32) -> Self {
        match v {
            0 => VideoWidth::Byte,
            1 => VideoWidth::Unknown,
            2 => VideoWidth::Short,
            3 => VideoWidth::Word,
            _ => unreachable!("2-bit VideoWidth field"),
        }
    }
}

/// Extract a video operand value from a register according to width and selector.
///
/// Mirrors `ExtractVideoOperandValue` in upstream `video_helper.cpp`.
pub fn extract_video_operand_value(
    tv: &mut TranslatorVisitor,
    value: Value,
    width: VideoWidth,
    selector: u32,
    is_signed: bool,
) -> Value {
    match width {
        VideoWidth::Byte | VideoWidth::Unknown => {
            let offset = Value::ImmU32(selector * 8);
            let count = Value::ImmU32(8);
            if is_signed {
                tv.ir.bit_field_s_extract(value, offset, count)
            } else {
                tv.ir.bit_field_u_extract(value, offset, count)
            }
        }
        VideoWidth::Short => {
            let offset = Value::ImmU32(selector * 16);
            let count = Value::ImmU32(16);
            if is_signed {
                tv.ir.bit_field_s_extract(value, offset, count)
            } else {
                tv.ir.bit_field_u_extract(value, offset, count)
            }
        }
        VideoWidth::Word => value,
    }
}

/// Return the effective source width for a video operand.
///
/// Immediates must be 16-bit format.
/// Mirrors `GetVideoSourceWidth` in upstream `video_helper.cpp`.
pub fn get_video_source_width(width: VideoWidth, is_immediate: bool) -> VideoWidth {
    if is_immediate {
        VideoWidth::Short
    } else {
        width
    }
}
