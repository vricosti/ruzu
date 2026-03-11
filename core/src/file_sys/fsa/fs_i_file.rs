// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fsa/fs_i_file.h

use crate::file_sys::fs_file::ReadOption;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

/// File abstraction for filesystem access.
/// Corresponds to upstream `FileSys::Fsa::IFile`.
pub struct IFile {
    backend: VirtualFile,
}

impl IFile {
    pub fn new(backend: VirtualFile) -> Self {
        Self { backend }
    }

    pub fn read(&self, offset: i64, buffer: &mut [u8], _option: &ReadOption) -> Result<usize, ResultCode> {
        if buffer.is_empty() { return Ok(0); }
        if offset < 0 { return Err(crate::file_sys::errors::RESULT_OUT_OF_RANGE); }
        let read = self.backend.read(buffer, buffer.len(), offset as usize);
        Ok(read)
    }

    pub fn get_size(&self) -> Result<i64, ResultCode> {
        Ok(self.backend.get_size() as i64)
    }

    pub fn flush(&self) -> Result<(), ResultCode> { Ok(()) }

    pub fn write(&self, offset: i64, buffer: &[u8]) -> Result<(), ResultCode> {
        if buffer.is_empty() { return Ok(()); }
        if offset < 0 { return Err(crate::file_sys::errors::RESULT_OUT_OF_RANGE); }
        let written = self.backend.write(buffer, buffer.len(), offset as usize);
        assert_eq!(written, buffer.len(), "Could not write all bytes (requested={}, actual={}).", buffer.len(), written);
        Ok(())
    }

    pub fn set_size(&self, size: i64) -> Result<(), ResultCode> {
        if size < 0 { return Err(crate::file_sys::errors::RESULT_OUT_OF_RANGE); }
        self.backend.resize(size as usize);
        Ok(())
    }
}
