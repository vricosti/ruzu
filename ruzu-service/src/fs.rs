// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! `fsp-srv` -- Filesystem service.
//!
//! Service hierarchy:
//!   fsp-srv -> IStorage (romfs data, read-only)
//!           -> IFileSystem (sd card stub)

use std::sync::Arc;

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};
use ruzu_common::error;

// ── fsp-srv ──────────────────────────────────────────────────────────────────

pub struct FspSrvService {
    romfs_data: Option<Arc<Vec<u8>>>,
}

impl FspSrvService {
    pub fn new() -> Self {
        Self { romfs_data: None }
    }

    /// Create with romfs data backing for IStorage.
    pub fn new_with_romfs(romfs_data: Option<Arc<Vec<u8>>>) -> Self {
        Self { romfs_data }
    }

    /// Get the romfs data (for creating the StorageService externally).
    pub fn romfs_data(&self) -> Option<Arc<Vec<u8>>> {
        self.romfs_data.clone()
    }
}

impl Default for FspSrvService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for FspSrvService {
    fn service_name(&self) -> &str {
        "fsp-srv"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("fsp-srv: cmd_id={}", cmd_id);

        match cmd_id {
            // SetCurrentProcess
            1 => {
                log::info!("fsp-srv: SetCurrentProcess");
                IpcResponse::success()
            }

            // OpenSdCardFileSystem → IFileSystem
            18 => {
                log::info!("fsp-srv: OpenSdCardFileSystem");
                IpcResponse::success().with_move_handle(0)
            }

            // OpenSaveDataFileSystem
            51 => {
                log::info!("fsp-srv: OpenSaveDataFileSystem (NOT_FOUND)");
                IpcResponse::error(error::NOT_FOUND)
            }

            // OpenDataStorageByCurrentProcess → IStorage
            200 => {
                if self.romfs_data.is_some() {
                    log::info!("fsp-srv: OpenDataStorageByCurrentProcess (romfs available)");
                    IpcResponse::success().with_move_handle(0)
                } else {
                    log::info!("fsp-srv: OpenDataStorageByCurrentProcess (no romfs)");
                    IpcResponse::error(error::NOT_FOUND)
                }
            }

            // OpenDataStorageByDataId
            202 => {
                log::info!("fsp-srv: OpenDataStorageByDataId (NOT_FOUND)");
                IpcResponse::error(error::NOT_FOUND)
            }

            // GetGlobalAccessLogMode
            1006 => {
                log::info!("fsp-srv: GetGlobalAccessLogMode (disabled)");
                IpcResponse::success_with_data(vec![0])
            }

            _ => {
                log::warn!("fsp-srv: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── fsp:IStorage ─────────────────────────────────────────────────────────────

/// Read-only storage backed by romfs data in memory.
pub struct StorageService {
    data: Arc<Vec<u8>>,
}

impl StorageService {
    pub fn new(data: Arc<Vec<u8>>) -> Self {
        Self { data }
    }

    /// Create an empty storage (no data).
    pub fn new_empty() -> Self {
        Self {
            data: Arc::new(Vec::new()),
        }
    }
}

impl ServiceHandler for StorageService {
    fn service_name(&self) -> &str {
        "fsp:IStorage"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("fsp:IStorage: cmd_id={}", cmd_id);
        match cmd_id {
            // Read — offset(u64), size(u64) from raw_data
            0 => {
                let (offset, size) = parse_read_params(&command.raw_data);
                log::info!(
                    "fsp:IStorage: Read(offset=0x{:X}, size=0x{:X}, total=0x{:X})",
                    offset, size, self.data.len()
                );

                if offset as usize >= self.data.len() {
                    return IpcResponse::success_with_data(vec![]);
                }

                let start = offset as usize;
                let end = (start + size as usize).min(self.data.len());
                let slice = &self.data[start..end];

                // Convert bytes to u32 words (pad last word if needed).
                let words = bytes_to_words(slice);
                IpcResponse::success_with_data(words)
            }

            // GetSize — total size as u64
            4 => {
                let total = self.data.len() as u64;
                let lo = total as u32;
                let hi = (total >> 32) as u32;
                log::info!("fsp:IStorage: GetSize (0x{:X})", total);
                IpcResponse::success_with_data(vec![lo, hi])
            }

            _ => {
                log::warn!("fsp:IStorage: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

/// Parse Read parameters: offset(u64) and size(u64) from raw data words.
fn parse_read_params(raw_data: &[u32]) -> (u64, u64) {
    let offset = if raw_data.len() >= 2 {
        raw_data[0] as u64 | ((raw_data[1] as u64) << 32)
    } else {
        0
    };
    let size = if raw_data.len() >= 4 {
        raw_data[2] as u64 | ((raw_data[3] as u64) << 32)
    } else {
        0
    };
    (offset, size)
}

/// Convert a byte slice to u32 words (little-endian, zero-padded).
fn bytes_to_words(bytes: &[u8]) -> Vec<u32> {
    let mut words = Vec::with_capacity((bytes.len() + 3) / 4);
    for chunk in bytes.chunks(4) {
        let mut buf = [0u8; 4];
        buf[..chunk.len()].copy_from_slice(chunk);
        words.push(u32::from_le_bytes(buf));
    }
    words
}

// ── fsp:IFileSystem ──────────────────────────────────────────────────────────

/// Stub filesystem for sd card access.
pub struct FileSystemService;

impl FileSystemService {
    pub fn new() -> Self {
        Self
    }
}

impl Default for FileSystemService {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceHandler for FileSystemService {
    fn service_name(&self) -> &str {
        "fsp:IFileSystem"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("fsp:IFileSystem: cmd_id={}", cmd_id);
        match cmd_id {
            // CreateFile
            0 => {
                log::info!("fsp:IFileSystem: CreateFile");
                IpcResponse::success()
            }
            // DeleteFile
            1 => {
                log::info!("fsp:IFileSystem: DeleteFile");
                IpcResponse::success()
            }
            // GetEntryType — not found
            7 => {
                log::info!("fsp:IFileSystem: GetEntryType (NOT_FOUND)");
                IpcResponse::error(error::NOT_FOUND)
            }
            // OpenFile — not found
            8 => {
                log::info!("fsp:IFileSystem: OpenFile (NOT_FOUND)");
                IpcResponse::error(error::NOT_FOUND)
            }
            // OpenDirectory — not found
            9 => {
                log::info!("fsp:IFileSystem: OpenDirectory (NOT_FOUND)");
                IpcResponse::error(error::NOT_FOUND)
            }
            // Commit
            10 => {
                log::info!("fsp:IFileSystem: Commit");
                IpcResponse::success()
            }
            // GetFreeSpaceSize — 256MB
            12 => {
                log::info!("fsp:IFileSystem: GetFreeSpaceSize (256MB)");
                IpcResponse::success_with_data(vec![0x10000000, 0])
            }
            // GetTotalSpaceSize — 1GB
            13 => {
                log::info!("fsp:IFileSystem: GetTotalSpaceSize (1GB)");
                IpcResponse::success_with_data(vec![0x40000000, 0])
            }
            _ => {
                log::warn!("fsp:IFileSystem: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::CommandType;

    fn make_command(cmd_id: u32) -> IpcCommand {
        IpcCommand {
            command_type: CommandType::Request,
            data_size: 0,
            num_x_bufs: 0,
            num_a_bufs: 0,
            num_b_bufs: 0,
            has_handle_descriptor: false,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            send_pid: false,
            cmif_magic: 0x49434653,
            command_id: cmd_id,
            raw_data: Vec::new(),
        }
    }

    fn make_command_with_data(cmd_id: u32, raw_data: Vec<u32>) -> IpcCommand {
        IpcCommand {
            command_type: CommandType::Request,
            data_size: 0,
            num_x_bufs: 0,
            num_a_bufs: 0,
            num_b_bufs: 0,
            has_handle_descriptor: false,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            send_pid: false,
            cmif_magic: 0x49434653,
            command_id: cmd_id,
            raw_data,
        }
    }

    #[test]
    fn test_set_current_process() {
        let mut svc = FspSrvService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
    }

    #[test]
    fn test_open_save_data_returns_not_found() {
        let mut svc = FspSrvService::new();
        let cmd = make_command(51);
        let resp = svc.handle_request(51, &cmd);
        assert!(resp.result.is_error());
        assert_eq!(resp.result, error::NOT_FOUND);
    }

    #[test]
    fn test_open_data_storage_with_romfs() {
        let romfs = Arc::new(vec![0xDE, 0xAD, 0xBE, 0xEF]);
        let mut svc = FspSrvService::new_with_romfs(Some(romfs));
        let cmd = make_command(200);
        let resp = svc.handle_request(200, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }

    #[test]
    fn test_open_data_storage_without_romfs() {
        let mut svc = FspSrvService::new();
        let cmd = make_command(200);
        let resp = svc.handle_request(200, &cmd);
        assert!(resp.result.is_error());
    }

    #[test]
    fn test_open_sd_card() {
        let mut svc = FspSrvService::new();
        let cmd = make_command(18);
        let resp = svc.handle_request(18, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }

    #[test]
    fn test_storage_read() {
        let data = Arc::new(vec![0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]);
        let mut svc = StorageService::new(data);

        // Read offset=0, size=4
        let cmd = make_command_with_data(0, vec![0, 0, 4, 0]);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![u32::from_le_bytes([0x11, 0x22, 0x33, 0x44])]);
    }

    #[test]
    fn test_storage_get_size() {
        let data = Arc::new(vec![0u8; 256]);
        let mut svc = StorageService::new(data);
        let cmd = make_command(4);
        let resp = svc.handle_request(4, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![256, 0]);
    }

    #[test]
    fn test_filesystem_get_entry_type_not_found() {
        let mut svc = FileSystemService::new();
        let cmd = make_command(7);
        let resp = svc.handle_request(7, &cmd);
        assert!(resp.result.is_error());
    }

    #[test]
    fn test_filesystem_free_space() {
        let mut svc = FileSystemService::new();
        let cmd = make_command(12);
        let resp = svc.handle_request(12, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0x10000000, 0]);
    }

    #[test]
    fn test_bytes_to_words() {
        assert_eq!(
            bytes_to_words(&[0x11, 0x22, 0x33, 0x44]),
            vec![u32::from_le_bytes([0x11, 0x22, 0x33, 0x44])]
        );
        // Partial last word (3 bytes → padded with zero)
        assert_eq!(
            bytes_to_words(&[0xAA, 0xBB, 0xCC]),
            vec![u32::from_le_bytes([0xAA, 0xBB, 0xCC, 0x00])]
        );
        assert_eq!(bytes_to_words(&[]), vec![]);
    }

    #[test]
    fn test_unhandled_returns_success() {
        let mut svc = FspSrvService::new();
        let cmd = make_command(9999);
        let resp = svc.handle_request(9999, &cmd);
        assert!(resp.result.is_success());
    }
}
