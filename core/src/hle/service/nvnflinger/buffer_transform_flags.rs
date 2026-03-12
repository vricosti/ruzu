// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/buffer_transform_flags.h

bitflags::bitflags! {
    /// Transform flags for buffer rendering.
    ///
    /// Corresponds to `BufferTransformFlags` in the C++ code with
    /// `DECLARE_ENUM_FLAG_OPERATORS`.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct BufferTransformFlags: u32 {
        /// No transform flags are set
        const UNSET = 0x00;
        /// Flip source image horizontally (around the vertical axis)
        const FLIP_H = 0x01;
        /// Flip source image vertically (around the horizontal axis)
        const FLIP_V = 0x02;
        /// Rotate source image 90 degrees clockwise
        const ROTATE_90 = 0x04;
        /// Rotate source image 180 degrees
        const ROTATE_180 = 0x03;
        /// Rotate source image 270 degrees clockwise
        const ROTATE_270 = 0x07;
    }
}
