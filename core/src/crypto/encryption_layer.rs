// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/crypto/encryption_layer.h and encryption_layer.cpp
//! VFS encryption layer base class.
//!
//! Basically non-functional class that implements all of the methods that are irrelevant
//! to an EncryptionLayer. Reduces duplicate code.
//!
//! In Rust, this is a trait + struct approach rather than C++ inheritance.

use std::sync::Arc;

// TODO: These types should come from the file_sys module once fully ported.
// For now, we define minimal trait/struct placeholders.

/// Trait representing a virtual file (VfsFile equivalent).
/// Port of FileSys::VfsFile interface as used by EncryptionLayer.
pub trait VfsFile: Send + Sync {
    fn read(&self, data: &mut [u8], offset: usize) -> usize;
    fn get_name(&self) -> String;
    fn get_size(&self) -> usize;
    fn resize(&self, _new_size: usize) -> bool {
        false
    }
    fn is_writable(&self) -> bool {
        false
    }
    fn is_readable(&self) -> bool {
        true
    }
    fn write(&self, _data: &[u8], _offset: usize) -> usize {
        0
    }
    fn rename(&self, _name: &str) -> bool {
        false
    }
    fn read_all_bytes(&self) -> Vec<u8> {
        let size = self.get_size();
        let mut buf = vec![0u8; size];
        self.read(&mut buf, 0);
        buf
    }
    fn read_bytes(&self, length: usize, offset: usize) -> Vec<u8> {
        let mut buf = vec![0u8; length];
        self.read(&mut buf, offset);
        buf
    }
}

/// Type alias for a shared virtual file.
pub type VirtualFile = Arc<dyn VfsFile>;

/// Adapter wrapping a `file_sys::vfs::vfs::VfsFile` as a `crypto::encryption_layer::VfsFile`.
/// Bridges the two VfsFile traits so crypto layers can operate on file_sys VirtualFiles.
pub struct FsVfsFileAdapter {
    inner: crate::file_sys::vfs::vfs_types::VirtualFile,
}

impl FsVfsFileAdapter {
    pub fn new(inner: crate::file_sys::vfs::vfs_types::VirtualFile) -> Self {
        Self { inner }
    }
}

impl VfsFile for FsVfsFileAdapter {
    fn read(&self, data: &mut [u8], offset: usize) -> usize {
        let len = data.len();
        self.inner.read(data, len, offset)
    }
    fn get_name(&self) -> String {
        self.inner.get_name()
    }
    fn get_size(&self) -> usize {
        self.inner.get_size()
    }
    fn resize(&self, new_size: usize) -> bool {
        self.inner.resize(new_size)
    }
    fn is_writable(&self) -> bool {
        self.inner.is_writable()
    }
    fn is_readable(&self) -> bool {
        self.inner.is_readable()
    }
    fn write(&self, data: &[u8], offset: usize) -> usize {
        let len = data.len();
        self.inner.write(data, len, offset)
    }
    fn rename(&self, name: &str) -> bool {
        self.inner.rename(name)
    }
}

/// Base encryption layer that wraps a VirtualFile and delegates non-crypto
/// methods to the underlying file. Port of Core::Crypto::EncryptionLayer.
pub struct EncryptionLayerBase {
    pub base: VirtualFile,
}

impl EncryptionLayerBase {
    pub fn new(base: VirtualFile) -> Self {
        Self { base }
    }

    pub fn get_name(&self) -> String {
        self.base.get_name()
    }

    pub fn get_size(&self) -> usize {
        self.base.get_size()
    }

    pub fn resize(&self, _new_size: usize) -> bool {
        false
    }

    pub fn is_writable(&self) -> bool {
        false
    }

    pub fn is_readable(&self) -> bool {
        true
    }

    pub fn write(&self, _data: &[u8], _offset: usize) -> usize {
        0
    }

    pub fn rename(&self, name: &str) -> bool {
        self.base.rename(name)
    }
}
