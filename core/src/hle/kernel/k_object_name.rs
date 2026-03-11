//! Port of zuyu/src/core/hle/kernel/k_object_name.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KObjectName: named kernel object registration for cross-process
//! discovery. Upstream uses slab allocation and an intrusive list.

use std::sync::Mutex;

/// Maximum length of an object name.
pub const NAME_LENGTH_MAX: usize = 12;

/// A named kernel object entry.
pub struct KObjectName {
    m_name: [u8; NAME_LENGTH_MAX],
    /// Placeholder for the KAutoObject pointer.
    m_object: usize,
}

impl KObjectName {
    pub fn new() -> Self {
        Self {
            m_name: [0u8; NAME_LENGTH_MAX],
            m_object: 0,
        }
    }

    fn initialize(&mut self, obj: usize, name: &str) {
        self.m_object = obj;
        let bytes = name.as_bytes();
        let copy_len = bytes.len().min(NAME_LENGTH_MAX - 1);
        self.m_name[..copy_len].copy_from_slice(&bytes[..copy_len]);
        self.m_name[copy_len] = 0;
    }

    fn matches_name(&self, name: &str) -> bool {
        let bytes = name.as_bytes();
        let name_len = bytes.len().min(NAME_LENGTH_MAX);
        // Find the null terminator in m_name
        let stored_len = self
            .m_name
            .iter()
            .position(|&b| b == 0)
            .unwrap_or(NAME_LENGTH_MAX);
        if stored_len != name_len {
            return false;
        }
        self.m_name[..stored_len] == bytes[..name_len]
    }

    pub fn get_object(&self) -> usize {
        self.m_object
    }
}

impl Default for KObjectName {
    fn default() -> Self {
        Self::new()
    }
}

/// Global data for KObjectName: a lock and a list of named objects.
pub struct KObjectNameGlobalData {
    m_object_list_lock: Mutex<()>,
    m_object_list: Mutex<Vec<KObjectName>>,
}

impl KObjectNameGlobalData {
    pub fn new() -> Self {
        Self {
            m_object_list_lock: Mutex::new(()),
            m_object_list: Mutex::new(Vec::new()),
        }
    }

    /// Register a new named object. Returns an error if the name already exists.
    pub fn new_from_name(&self, obj: usize, name: &str) -> Result<(), ()> {
        let _lk = self.m_object_list_lock.lock().unwrap();
        let mut list = self.m_object_list.lock().unwrap();

        // Check if name already exists.
        if list.iter().any(|entry| entry.matches_name(name)) {
            return Err(());
        }

        let mut entry = KObjectName::new();
        entry.initialize(obj, name);
        list.push(entry);
        Ok(())
    }

    /// Delete a named object entry.
    pub fn delete(&self, obj: usize, name: &str) -> Result<(), ()> {
        let _lk = self.m_object_list_lock.lock().unwrap();
        let mut list = self.m_object_list.lock().unwrap();

        if let Some(pos) = list
            .iter()
            .position(|entry| entry.matches_name(name) && entry.get_object() == obj)
        {
            list.remove(pos);
            Ok(())
        } else {
            Err(())
        }
    }

    /// Find a named object by name. Returns the object identifier if found.
    pub fn find(&self, name: &str) -> Option<usize> {
        let _lk = self.m_object_list_lock.lock().unwrap();
        let list = self.m_object_list.lock().unwrap();

        list.iter()
            .find(|entry| entry.matches_name(name))
            .map(|entry| entry.get_object())
    }
}

impl Default for KObjectNameGlobalData {
    fn default() -> Self {
        Self::new()
    }
}
