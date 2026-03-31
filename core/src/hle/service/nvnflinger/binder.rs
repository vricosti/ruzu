// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2014 The Android Open Source Project
// SPDX-License-Identifier: GPL-3.0-or-later
// Parts of this implementation were based on:
// https://cs.android.com/android/platform/superproject/+/android-5.1.1_r38:frameworks/native/include/binder/IBinder.h

//! Port of zuyu/src/core/hle/service/nvnflinger/binder.h

use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_scheduler::KScheduler;

/// The IBinder trait corresponds to the C++ IBinder abstract class.
///
/// In upstream, IBinder has:
///   virtual void Transact(u32 code, span<const u8> parcel_data, span<u8> parcel_reply, u32 flags)
///   virtual KReadableEvent* GetNativeHandle(u32 type_id)
pub trait IBinder: Send + Sync {
    fn transact(&self, code: u32, parcel_data: &[u8], parcel_reply: &mut [u8], flags: u32);
    fn get_native_handle(&self, type_id: u32) -> Option<Arc<Mutex<KReadableEvent>>>;

    /// Rust-only adapter: lets binder owners learn which process/scheduler currently owns
    /// the copied handle so later event signaling can wake WaitSynchronization waiters.
    fn register_native_handle_owner(
        &self,
        _process: Arc<Mutex<KProcess>>,
        _scheduler: Arc<Mutex<KScheduler>>,
    ) {
    }
}
