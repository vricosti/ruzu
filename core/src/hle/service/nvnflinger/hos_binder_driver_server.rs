// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/hos_binder_driver_server.h
//! Port of zuyu/src/core/hle/service/nvnflinger/hos_binder_driver_server.cpp
//!
//! The HOS binder driver server manages a collection of binder objects
//! that can be looked up by ID. Full implementation depends on the
//! binder/IBinder infrastructure.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use super::binder::IBinder;

/// Manages registered binder objects by ID.
pub struct HosBinderDriverServer {
    inner: Mutex<HosBinderDriverServerInner>,
}

struct HosBinderDriverServerInner {
    binders: HashMap<i32, Arc<dyn IBinder>>,
    next_id: i32,
}

impl HosBinderDriverServer {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            inner: Mutex::new(HosBinderDriverServerInner {
                binders: HashMap::new(),
                next_id: 0,
            }),
        })
    }

    /// Register a new binder and return its ID.
    pub fn register_binder(&self, binder: Arc<dyn IBinder>) -> i32 {
        let mut inner = self.inner.lock().unwrap();
        let id = inner.next_id;
        inner.next_id += 1;
        inner.binders.insert(id, binder);
        id
    }

    /// Unregister a binder by ID.
    pub fn unregister_binder(&self, id: i32) {
        let mut inner = self.inner.lock().unwrap();
        inner.binders.remove(&id);
    }

    /// Try to get a binder by ID.
    pub fn try_get_binder(&self, id: i32) -> Option<Arc<dyn IBinder>> {
        let inner = self.inner.lock().unwrap();
        inner.binders.get(&id).cloned()
    }

    /// Perform a binder transaction.
    pub fn transact(&self, id: i32, code: u32, parcel_data: &[u8], parcel_reply: &mut [u8], flags: u32) {
        if let Some(binder) = self.try_get_binder(id) {
            binder.transact(code, parcel_data, parcel_reply, flags);
        } else {
            log::error!("HosBinderDriverServer: binder {} not found", id);
        }
    }

    /// Get the native handle from a binder.
    pub fn get_native_handle(&self, id: i32, type_id: u32) -> Option<u32> {
        if let Some(binder) = self.try_get_binder(id) {
            binder.get_native_handle(type_id)
        } else {
            log::error!("HosBinderDriverServer: binder {} not found", id);
            None
        }
    }
}

impl Default for HosBinderDriverServer {
    fn default() -> Self {
        Self {
            inner: Mutex::new(HosBinderDriverServerInner {
                binders: HashMap::new(),
                next_id: 0,
            }),
        }
    }
}
