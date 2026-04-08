// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2007 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/ui/GraphicBuffer.h

//! Port of zuyu/src/core/hle/service/nvnflinger/ui/graphic_buffer.h
//! Port of zuyu/src/core/hle/service/nvnflinger/ui/graphic_buffer.cpp

use std::sync::Arc;

use crate::hle::service::nvdrv::core::nvmap::NvMap;

use super::super::pixel_format::PixelFormat;

/// Raw NV graphic buffer data structure.
///
/// This struct has a specific binary layout that must match upstream exactly
/// for serialization purposes.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct NvGraphicBuffer {
    pub magic: u32,
    pub width: i32,
    pub height: i32,
    pub stride: i32,
    pub format: PixelFormat,
    pub usage: i32,
    _padding0: u32,
    pub index: u32,
    _padding1: [u32; 3],
    pub buffer_id: u32,
    _padding2: [u32; 6],
    pub external_format: PixelFormat,
    _padding3: [u32; 10],
    pub handle: u32,
    pub offset: u32,
    _padding4: [u32; 60],
}
const _: () = assert!(std::mem::size_of::<NvGraphicBuffer>() == 0x16C);

impl Default for NvGraphicBuffer {
    fn default() -> Self {
        // Safety: All fields are integer/enum types where zero is valid.
        unsafe { std::mem::zeroed() }
    }
}

impl NvGraphicBuffer {
    pub fn new(width: u32, height: u32, format: PixelFormat, usage: u32) -> Self {
        let mut buf = Self::default();
        buf.width = width as i32;
        buf.height = height as i32;
        buf.format = format;
        buf.usage = usage as i32;
        buf
    }

    pub fn get_width(&self) -> u32 {
        self.width as u32
    }

    pub fn get_height(&self) -> u32 {
        self.height as u32
    }

    pub fn get_stride(&self) -> u32 {
        self.stride as u32
    }

    pub fn get_usage(&self) -> u32 {
        self.usage as u32
    }

    pub fn get_format(&self) -> PixelFormat {
        self.format
    }

    pub fn get_buffer_id(&self) -> u32 {
        self.buffer_id
    }

    pub fn get_external_format(&self) -> PixelFormat {
        self.external_format
    }

    pub fn get_handle(&self) -> u32 {
        self.handle
    }

    pub fn get_offset(&self) -> u32 {
        self.offset
    }

    pub fn needs_reallocation(
        &self,
        width: u32,
        height: u32,
        format: PixelFormat,
        usage: u32,
    ) -> bool {
        if width as i32 != self.width {
            return true;
        }
        if height as i32 != self.height {
            return true;
        }
        if format != self.format {
            return true;
        }
        if (self.usage as u32 & usage) != usage {
            return true;
        }
        false
    }
}

/// A GraphicBuffer wraps an NvGraphicBuffer and optionally tracks NvMap ownership.
///
/// In upstream C++, GraphicBuffer inherits from NvGraphicBuffer and holds an optional
/// NvMap pointer for cleanup. Here we use composition plus an `Arc<NvMap>` owner.
pub struct GraphicBuffer {
    pub buffer: NvGraphicBuffer,
    nvmap: Option<Arc<NvMap>>,
}

impl GraphicBuffer {
    pub fn new(width: u32, height: u32, format: PixelFormat, usage: u32) -> Self {
        Self {
            buffer: NvGraphicBuffer::new(width, height, format, usage),
            nvmap: None,
        }
    }

    pub fn from_nv_buffer(nvmap: Arc<NvMap>, buffer: NvGraphicBuffer) -> Self {
        if buffer.get_buffer_id() > 0 {
            nvmap.duplicate_handle(buffer.get_buffer_id(), true);
            nvmap.pin_handle(buffer.get_buffer_id(), false);
        }

        Self {
            buffer,
            nvmap: Some(nvmap),
        }
    }

    pub fn from_optional_nv_buffer(nvmap: Arc<NvMap>, buffer: Option<&NvGraphicBuffer>) -> Self {
        Self::from_nv_buffer(nvmap, buffer.copied().unwrap_or_default())
    }

    // Delegate accessors to inner buffer
    pub fn get_width(&self) -> u32 {
        self.buffer.get_width()
    }
    pub fn get_height(&self) -> u32 {
        self.buffer.get_height()
    }
    pub fn get_stride(&self) -> u32 {
        self.buffer.get_stride()
    }
    pub fn get_usage(&self) -> u32 {
        self.buffer.get_usage()
    }
    pub fn get_format(&self) -> PixelFormat {
        self.buffer.get_format()
    }
    pub fn get_buffer_id(&self) -> u32 {
        self.buffer.get_buffer_id()
    }
    pub fn get_external_format(&self) -> PixelFormat {
        self.buffer.get_external_format()
    }
    pub fn get_handle(&self) -> u32 {
        self.buffer.get_handle()
    }
    pub fn get_offset(&self) -> u32 {
        self.buffer.get_offset()
    }

    pub fn needs_reallocation(
        &self,
        width: u32,
        height: u32,
        format: PixelFormat,
        usage: u32,
    ) -> bool {
        self.buffer.needs_reallocation(width, height, format, usage)
    }
}

impl Drop for GraphicBuffer {
    fn drop(&mut self) {
        let Some(nvmap) = &self.nvmap else {
            return;
        };

        if self.buffer.get_buffer_id() > 0 {
            nvmap.unpin_handle(self.buffer.get_buffer_id());
            let _ = nvmap.free_handle(self.buffer.get_buffer_id(), true);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::hle::service::nvdrv::core::container::Container;

    use super::*;

    fn test_nvmap() -> Arc<NvMap> {
        Container::new().get_nv_map_file_handle()
    }

    #[test]
    fn from_optional_nv_buffer_none_keeps_zero_initialized_payload() {
        let graphic_buffer = GraphicBuffer::from_optional_nv_buffer(test_nvmap(), None);

        assert_eq!(graphic_buffer.get_width(), 0);
        assert_eq!(graphic_buffer.get_height(), 0);
        assert_eq!(graphic_buffer.get_buffer_id(), 0);
        assert_eq!(graphic_buffer.get_handle(), 0);
        assert_eq!(graphic_buffer.get_offset(), 0);
    }
}
