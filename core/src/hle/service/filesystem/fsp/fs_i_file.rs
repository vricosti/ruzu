//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_file.h and fs_i_file.cpp
//!
//! IFile service.

/// IPC command table for IFile:
///
/// | Cmd | Name                |
/// |-----|---------------------|
/// | 0   | Read                |
/// | 1   | Write               |
/// | 2   | Flush               |
/// | 3   | SetSize             |
/// | 4   | GetSize             |
/// | 5   | OperateRange        |
/// | 6   | OperateRangeWithBuffer |
pub struct IFile {
    // backend: FileSys::Fsa::IFile,
}

impl IFile {
    pub fn new() -> Self {
        Self {}
    }
}
