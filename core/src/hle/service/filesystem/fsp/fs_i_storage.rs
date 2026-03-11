//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_storage.h and fs_i_storage.cpp
//!
//! IStorage service.

/// IPC command table for IStorage:
///
/// | Cmd | Name         |
/// |-----|--------------|
/// | 0   | Read         |
/// | 1   | Write        |
/// | 2   | Flush        |
/// | 3   | SetSize      |
/// | 4   | GetSize      |
/// | 5   | OperateRange |
pub struct IStorage {
    // backend: VirtualFile,
}

impl IStorage {
    pub fn new() -> Self {
        Self {}
    }
}
