//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_filesystem.h and .cpp
//!
//! IFileSystem service.

/// IPC command table for IFileSystem:
///
/// | Cmd | Name                        |
/// |-----|-----------------------------|
/// | 0   | CreateFile                  |
/// | 1   | DeleteFile                  |
/// | 2   | CreateDirectory             |
/// | 3   | DeleteDirectory             |
/// | 4   | DeleteDirectoryRecursively  |
/// | 5   | RenameFile                  |
/// | 6   | RenameDirectory             |
/// | 7   | GetEntryType                |
/// | 8   | OpenFile                    |
/// | 9   | OpenDirectory               |
/// | 10  | Commit                      |
/// | 11  | GetFreeSpaceSize            |
/// | 12  | GetTotalSpaceSize           |
/// | 13  | CleanDirectoryRecursively   |
/// | 14  | GetFileTimeStampRaw         |
/// | 15  | QueryEntry                  |
/// | 16  | GetFileSystemAttribute      |
pub struct IFileSystem {
    // backend: FileSys::Fsa::IFileSystem,
    // size_getter: SizeGetter,
}

impl IFileSystem {
    pub fn new() -> Self {
        Self {}
    }
}
