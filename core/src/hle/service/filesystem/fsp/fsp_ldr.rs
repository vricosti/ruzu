//! Port of zuyu/src/core/hle/service/filesystem/fsp/fsp_ldr.h and fsp_ldr.cpp
//!
//! FSP_LDR service ("fsp:ldr").

/// IPC command table for FSP_LDR ("fsp:ldr"):
///
/// | Cmd | Name                |
/// |-----|---------------------|
/// | 0   | OpenCodeFileSystem  |
/// | 1   | IsArchivedProgram   |
/// | 2   | SetCurrentProcess   |
pub struct FspLdr;

impl FspLdr {
    pub fn new() -> Self {
        Self
    }
}
