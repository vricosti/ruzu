// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ruzu_common::error;
use ruzu_common::{Handle, ResultCode, INVALID_HANDLE};
use std::collections::HashMap;

use crate::objects::KernelObject;

/// Maximum number of handles per process.
const MAX_HANDLES: usize = 1024;

/// Handle table: maps Handle -> KernelObject.
/// Handles are allocated starting from 1 (0 is invalid).
pub struct HandleTable {
    objects: HashMap<Handle, KernelObject>,
    next_handle: Handle,
}

impl HandleTable {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            next_handle: 1,
        }
    }

    /// Add an object and return its handle.
    pub fn add(&mut self, object: KernelObject) -> Result<Handle, ResultCode> {
        if self.objects.len() >= MAX_HANDLES {
            return Err(error::HANDLE_TABLE_FULL);
        }

        let handle = self.next_handle;
        self.next_handle += 1;

        // Skip zero (invalid handle)
        if self.next_handle == INVALID_HANDLE {
            self.next_handle = 1;
        }

        self.objects.insert(handle, object);
        Ok(handle)
    }

    /// Look up an object by handle.
    pub fn get(&self, handle: Handle) -> Result<&KernelObject, ResultCode> {
        self.objects.get(&handle).ok_or(error::INVALID_HANDLE)
    }

    /// Look up an object by handle (mutable).
    pub fn get_mut(&mut self, handle: Handle) -> Result<&mut KernelObject, ResultCode> {
        self.objects.get_mut(&handle).ok_or(error::INVALID_HANDLE)
    }

    /// Remove an object by handle.
    pub fn close(&mut self, handle: Handle) -> Result<KernelObject, ResultCode> {
        self.objects.remove(&handle).ok_or(error::INVALID_HANDLE)
    }

    /// Check if a handle exists.
    pub fn contains(&self, handle: Handle) -> bool {
        self.objects.contains_key(&handle)
    }

    /// Number of active handles.
    pub fn len(&self) -> usize {
        self.objects.len()
    }

    /// Whether the table is empty.
    pub fn is_empty(&self) -> bool {
        self.objects.is_empty()
    }
}

impl Default for HandleTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::objects::KernelObject;

    #[test]
    fn test_add_and_get() {
        let mut table = HandleTable::new();
        let obj = KernelObject::Event(crate::objects::KEvent::new());
        let handle = table.add(obj).unwrap();
        assert_ne!(handle, INVALID_HANDLE);
        assert!(table.get(handle).is_ok());
    }

    #[test]
    fn test_close() {
        let mut table = HandleTable::new();
        let obj = KernelObject::Event(crate::objects::KEvent::new());
        let handle = table.add(obj).unwrap();
        assert!(table.close(handle).is_ok());
        assert!(table.get(handle).is_err());
    }

    #[test]
    fn test_invalid_handle() {
        let table = HandleTable::new();
        assert!(table.get(42).is_err());
    }
}
