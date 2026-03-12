// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/pixel_format.h

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PixelFormat {
    NoFormat = 0,
    Rgba8888 = 1,
    Rgbx8888 = 2,
    Rgb888 = 3,
    Rgb565 = 4,
    Bgra8888 = 5,
    Rgba5551 = 6,
    Rgba4444 = 7,
}

impl Default for PixelFormat {
    fn default() -> Self {
        PixelFormat::NoFormat
    }
}
