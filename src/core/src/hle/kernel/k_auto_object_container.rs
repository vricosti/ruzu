//! Port of zuyu/src/core/hle/kernel/k_auto_object_container.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! KAutoObjectWithListContainer: a thread-safe container for KAutoObjectWithList
//! objects, backed by a sorted collection. Upstream uses a boost intrusive
//! red-black tree; here we use a BTreeSet behind a Mutex.

use std::collections::BTreeSet;
use std::sync::Mutex;

/// Represents an entry in the auto-object container.
/// Upstream this is a KAutoObjectWithList stored in a boost intrusive rbtree.
/// Here we use a simple u64 identifier as a placeholder for the object pointer.
pub type ObjectId = u64;

/// Thread-safe container for registered kernel auto-objects.
///
/// Maps to upstream KAutoObjectWithListContainer.
pub struct KAutoObjectWithListContainer {
    m_lock: Mutex<()>,
    m_object_list: Mutex<BTreeSet<ObjectId>>,
}

impl KAutoObjectWithListContainer {
    pub fn new() -> Self {
        Self {
            m_lock: Mutex::new(()),
            m_object_list: Mutex::new(BTreeSet::new()),
        }
    }

    pub fn initialize(&self) {}
    pub fn finalize(&self) {}

    pub fn register(&self, obj_id: ObjectId) {
        let _lk = self.m_lock.lock().unwrap();
        self.m_object_list.lock().unwrap().insert(obj_id);
    }

    pub fn unregister(&self, obj_id: ObjectId) {
        let _lk = self.m_lock.lock().unwrap();
        self.m_object_list.lock().unwrap().remove(&obj_id);
    }

    /// Returns the number of objects owned by a given process identifier.
    /// Upstream filters by owner pointer; here we accept a predicate.
    pub fn get_owned_count<F>(&self, predicate: F) -> usize
    where
        F: Fn(ObjectId) -> bool,
    {
        let _lk = self.m_lock.lock().unwrap();
        self.m_object_list
            .lock()
            .unwrap()
            .iter()
            .filter(|&&id| predicate(id))
            .count()
    }
}

impl Default for KAutoObjectWithListContainer {
    fn default() -> Self {
        Self::new()
    }
}
