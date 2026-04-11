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

use crate::hle::kernel::k_readable_event::KReadableEvent;

use super::binder::IBinder;
use super::buffer_queue_producer::BufferQueueProducer;

/// Manages registered binder objects by ID.
pub struct HosBinderDriverServer {
    inner: Mutex<HosBinderDriverServerInner>,
}

struct HosBinderDriverServerInner {
    binders: HashMap<i32, Arc<dyn IBinder>>,
    buffer_queue_producers: HashMap<i32, Arc<BufferQueueProducer>>,
    next_id: i32,
}

impl HosBinderDriverServer {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            inner: Mutex::new(HosBinderDriverServerInner {
                binders: HashMap::new(),
                buffer_queue_producers: HashMap::new(),
                next_id: 1,
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

    /// Register a producer binder and remember its concrete type, mirroring upstream
    /// `static_pointer_cast<BufferQueueProducer>(TryGetBinder(...))` without rebuilding an Arc
    /// from a raw pointer.
    pub fn register_buffer_queue_producer(&self, binder: Arc<BufferQueueProducer>) -> i32 {
        let binder_trait: Arc<dyn IBinder> = binder.clone();
        let mut inner = self.inner.lock().unwrap();
        let id = inner.next_id;
        inner.next_id += 1;
        inner.binders.insert(id, binder_trait);
        inner.buffer_queue_producers.insert(id, binder);
        id
    }

    /// Unregister a binder by ID.
    pub fn unregister_binder(&self, id: i32) {
        let mut inner = self.inner.lock().unwrap();
        inner.binders.remove(&id);
        inner.buffer_queue_producers.remove(&id);
    }

    /// Try to get a binder by ID.
    pub fn try_get_binder(&self, id: i32) -> Option<Arc<dyn IBinder>> {
        let inner = self.inner.lock().unwrap();
        inner.binders.get(&id).cloned()
    }

    pub fn try_get_buffer_queue_producer(&self, id: i32) -> Option<Arc<BufferQueueProducer>> {
        let inner = self.inner.lock().unwrap();
        inner.buffer_queue_producers.get(&id).cloned()
    }

    /// Perform a binder transaction.
    pub fn transact(
        &self,
        id: i32,
        code: u32,
        parcel_data: &[u8],
        parcel_reply: &mut [u8],
        flags: u32,
    ) {
        if let Some(binder) = self.try_get_binder(id) {
            binder.transact(code, parcel_data, parcel_reply, flags);
        } else {
            log::error!("HosBinderDriverServer: binder {} not found", id);
        }
    }

    /// Get the native handle from a binder.
    pub fn get_native_handle(&self, id: i32, type_id: u32) -> Option<Arc<Mutex<KReadableEvent>>> {
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
                buffer_queue_producers: HashMap::new(),
                next_id: 1,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use super::HosBinderDriverServer;
    use crate::hle::kernel::k_readable_event::KReadableEvent;
    use crate::hle::service::nvnflinger::binder::IBinder;

    struct DummyBinder;

    impl IBinder for DummyBinder {
        fn transact(&self, _code: u32, _parcel_data: &[u8], _parcel_reply: &mut [u8], _flags: u32) {
        }

        fn get_native_handle(&self, _type_id: u32) -> Option<Arc<Mutex<KReadableEvent>>> {
            None
        }

        fn as_any(&self) -> &dyn std::any::Any {
            self
        }
    }

    #[test]
    fn register_binder_starts_ids_at_one() {
        let server = HosBinderDriverServer::new();

        let first = server.register_binder(Arc::new(DummyBinder));
        let second = server.register_binder(Arc::new(DummyBinder));

        assert_eq!(first, 1);
        assert_eq!(second, 2);
    }

    #[test]
    fn unregister_binder_clears_buffer_queue_producer_marker() {
        let server = HosBinderDriverServer::new();
        let producer = Arc::new(
            crate::hle::service::nvnflinger::buffer_queue_producer::BufferQueueProducer::new(
                crate::hle::service::nvnflinger::buffer_queue_core::BufferQueueCore::new(),
                Arc::new(crate::hle::service::nvdrv::core::nvmap::NvMap::new()),
            ),
        );

        let id = server.register_buffer_queue_producer(producer);
        assert!(server.try_get_buffer_queue_producer(id).is_some());

        server.unregister_binder(id);
        assert!(server.try_get_buffer_queue_producer(id).is_none());
    }
}
