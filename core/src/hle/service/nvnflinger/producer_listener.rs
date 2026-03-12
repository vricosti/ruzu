// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/gui/IProducerListener.h

//! Port of zuyu/src/core/hle/service/nvnflinger/producer_listener.h

pub trait IProducerListener: Send + Sync {
    fn on_buffer_released(&self);
}
