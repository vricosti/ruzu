// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2012 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/ui/Fence.h

//! Port of zuyu/src/core/hle/service/nvnflinger/ui/fence.h

use crate::hle::service::nvdrv::nvdata::NvFence;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Fence {
    pub num_fences: u32,
    pub fences: [NvFence; 4],
}
const _: () = assert!(std::mem::size_of::<Fence>() == 36);

impl Default for Fence {
    fn default() -> Self {
        Self {
            num_fences: 0,
            fences: [NvFence::default(); 4],
        }
    }
}

impl Fence {
    pub fn no_fence() -> Self {
        Self {
            num_fences: 0,
            fences: [
                NvFence { id: -1, value: 0 },
                NvFence { id: -1, value: 0 },
                NvFence { id: -1, value: 0 },
                NvFence { id: -1, value: 0 },
            ],
        }
    }
}
