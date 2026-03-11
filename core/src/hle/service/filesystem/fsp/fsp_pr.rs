//! Port of zuyu/src/core/hle/service/filesystem/fsp/fsp_pr.h and fsp_pr.cpp
//!
//! FSP_PR service ("fsp:pr").

/// IPC command table for FSP_PR ("fsp:pr"):
///
/// | Cmd | Name                            |
/// |-----|---------------------------------|
/// | 0   | RegisterProgram                 |
/// | 1   | UnregisterProgram               |
/// | 2   | SetCurrentProcess               |
/// | 256 | SetEnabledProgramVerification   |
pub struct FspPr;

impl FspPr {
    pub fn new() -> Self {
        Self
    }
}
