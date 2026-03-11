//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_multi_commit_manager.h and .cpp
//!
//! IMultiCommitManager service.

/// IPC command table for IMultiCommitManager:
///
/// | Cmd | Name   |
/// |-----|--------|
/// | 1   | Add    |
/// | 2   | Commit |
pub struct IMultiCommitManager;

impl IMultiCommitManager {
    pub fn new() -> Self {
        Self
    }
}
