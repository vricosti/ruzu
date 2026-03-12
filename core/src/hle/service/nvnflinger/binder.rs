// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/binder/IBinder.h

//! Port of zuyu/src/core/hle/service/nvnflinger/binder.h

/// The IBinder trait corresponds to the C++ IBinder abstract class.
///
/// In upstream, IBinder has:
///   virtual void Transact(u32 code, span<const u8> parcel_data, span<u8> parcel_reply, u32 flags)
///   virtual KReadableEvent* GetNativeHandle(u32 type_id)
///
/// We model GetNativeHandle as returning an optional handle index since the actual
/// kernel event infrastructure depends on the kernel subsystem.
pub trait IBinder: Send + Sync {
    fn transact(&self, code: u32, parcel_data: &[u8], parcel_reply: &mut [u8], flags: u32);
    fn get_native_handle(&self, type_id: u32) -> Option<u32>;
}
