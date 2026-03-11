// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/library_applet_storage.h
//! Port of zuyu/src/core/hle/service/am/library_applet_storage.cpp

use super::am_results;
use crate::hle::result::ResultCode;

fn validate_offset(offset: i64, size: usize, data_size: usize) -> Result<(), ResultCode> {
    if offset < 0 {
        return Err(am_results::RESULT_INVALID_OFFSET);
    }
    let begin = offset as usize;
    let end = begin + size;
    if begin > end || end > data_size {
        return Err(am_results::RESULT_INVALID_OFFSET);
    }
    Ok(())
}

/// Port of LibraryAppletStorage (abstract base)
pub trait LibraryAppletStorage: Send + Sync {
    fn read(&self, offset: i64, buffer: &mut [u8]) -> Result<(), ResultCode>;
    fn write(&mut self, offset: i64, buffer: &[u8]) -> Result<(), ResultCode>;
    fn get_size(&self) -> i64;
    fn has_handle(&self) -> bool {
        false
    }

    fn get_data(&self) -> Vec<u8> {
        let size = self.get_size() as usize;
        let mut data = vec![0u8; size];
        let _ = self.read(0, &mut data);
        data
    }
}

/// Port of BufferLibraryAppletStorage
pub struct BufferLibraryAppletStorage {
    data: Vec<u8>,
}

impl BufferLibraryAppletStorage {
    pub fn new(data: Vec<u8>) -> Self {
        Self { data }
    }
}

impl LibraryAppletStorage for BufferLibraryAppletStorage {
    fn read(&self, offset: i64, buffer: &mut [u8]) -> Result<(), ResultCode> {
        validate_offset(offset, buffer.len(), self.data.len())?;
        let start = offset as usize;
        buffer.copy_from_slice(&self.data[start..start + buffer.len()]);
        Ok(())
    }

    fn write(&mut self, offset: i64, buffer: &[u8]) -> Result<(), ResultCode> {
        validate_offset(offset, buffer.len(), self.data.len())?;
        let start = offset as usize;
        self.data[start..start + buffer.len()].copy_from_slice(buffer);
        Ok(())
    }

    fn get_size(&self) -> i64 {
        self.data.len() as i64
    }
}

/// Create a buffer-backed storage.
pub fn create_storage(data: Vec<u8>) -> Box<dyn LibraryAppletStorage> {
    Box::new(BufferLibraryAppletStorage::new(data))
}

// TODO: TransferMemoryLibraryAppletStorage and HandleLibraryAppletStorage
// require kernel KTransferMemory integration
