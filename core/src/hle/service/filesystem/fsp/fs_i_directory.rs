//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_directory.h and fs_i_directory.cpp
//!
//! IDirectory service.

/// IPC command table for IDirectory:
///
/// | Cmd | Name            |
/// |-----|-----------------|
/// | 0   | Read            |
/// | 1   | GetEntryCount   |
pub struct IDirectory {
    // backend: FileSys::Fsa::IDirectory,
}

impl IDirectory {
    pub fn new() -> Self {
        Self {}
    }
}
