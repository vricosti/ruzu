// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Game scanner for building a library from a games directory.
//!
//! Scans `.nsp`, `.xci`, and `.nro` files, extracting metadata (title, developer,
//! version, title ID) for display in the game list UI.

use crate::nacp::Nacp;
use crate::nca;
use crate::nro;
use crate::pfs::Pfs;
use crate::vfs::{RealFile, VecFile, VfsFile};
use ruzu_crypto::key_manager::KeyManager;
use ruzu_crypto::ticket::Ticket;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use thiserror::Error;

/// Supported game formats.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameFormat {
    Nsp,
    Xci,
    Nro,
}

impl GameFormat {
    /// Detect format from file extension.
    pub fn from_extension(path: &Path) -> Option<Self> {
        match path
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.to_lowercase())
            .as_deref()
        {
            Some("nsp") => Some(Self::Nsp),
            Some("xci") => Some(Self::Xci),
            Some("nro") => Some(Self::Nro),
            _ => None,
        }
    }

    /// Display name for the format.
    pub fn label(&self) -> &'static str {
        match self {
            Self::Nsp => "NSP",
            Self::Xci => "XCI",
            Self::Nro => "NRO",
        }
    }
}

/// Metadata for a single game.
#[derive(Debug, Clone)]
pub struct GameInfo {
    /// Full path to the game file.
    pub path: PathBuf,
    /// Detected format.
    pub format: GameFormat,
    /// Title ID (0 if unknown).
    pub title_id: u64,
    /// Game title.
    pub title: String,
    /// Developer/publisher.
    pub developer: String,
    /// Version string.
    pub version: String,
    /// Raw icon data (PNG), if available.
    pub icon_data: Option<Vec<u8>>,
}

/// Errors from game scanning.
#[derive(Debug, Error)]
pub enum ScanError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("unsupported format: {0}")]
    UnsupportedFormat(String),
}

/// Scan a single game file and extract metadata.
///
/// This does a lightweight scan — reads headers and NACP data but doesn't
/// fully decrypt/decompress code segments.
pub fn scan_game(path: &Path, keys: &mut KeyManager) -> Result<GameInfo, ScanError> {
    let format = GameFormat::from_extension(path)
        .ok_or_else(|| ScanError::UnsupportedFormat(path.display().to_string()))?;

    let filename = path
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy()
        .into_owned();

    match format {
        GameFormat::Nro => scan_nro(path, &filename),
        GameFormat::Nsp => scan_nsp(path, keys, &filename),
        GameFormat::Xci => scan_xci(path, keys, &filename),
    }
}

/// Scan an NRO file for metadata.
fn scan_nro(path: &Path, fallback_name: &str) -> Result<GameInfo, ScanError> {
    let data = std::fs::read(path)?;

    let mut title = fallback_name.to_string();
    let mut developer = String::new();
    let mut version = String::new();
    let mut icon_data = None;
    let mut title_id = 0u64;

    // Try to parse NRO and extract ASET metadata
    if let Ok(code_set) = nro::load_nro(&data) {
        if let Some(asset) = &code_set.asset_header {
            let nro_file_size = data.len().min(
                code_set.segments[0].offset as usize
                    + code_set.segments.iter().map(|s| s.size).sum::<u32>() as usize,
            );
            let asset_base = nro_file_size;

            // Read NACP from ASET
            if asset.nacp.size > 0 {
                let nacp_start = asset_base + asset.nacp.offset as usize;
                let nacp_end = nacp_start + asset.nacp.size as usize;
                if nacp_end <= data.len() {
                    if let Ok(nacp) = Nacp::parse(&data[nacp_start..nacp_end]) {
                        title = nacp.get_application_name();
                        developer = nacp.get_developer();
                        version = nacp.version;
                        title_id = nacp.title_id;
                    }
                }
            }

            // Read icon from ASET
            if asset.icon.size > 0 {
                let icon_start = asset_base + asset.icon.offset as usize;
                let icon_end = icon_start + asset.icon.size as usize;
                if icon_end <= data.len() {
                    icon_data = Some(data[icon_start..icon_end].to_vec());
                }
            }
        }
    }

    Ok(GameInfo {
        path: path.to_path_buf(),
        format: GameFormat::Nro,
        title_id,
        title,
        developer,
        version,
        icon_data,
    })
}

/// Scan an NSP file for metadata.
fn scan_nsp(path: &Path, keys: &mut KeyManager, fallback_name: &str) -> Result<GameInfo, ScanError> {
    let file: Arc<dyn VfsFile> = Arc::new(RealFile::open(path)?);

    let mut title = fallback_name.to_string();
    let mut developer = String::new();
    let mut version = String::new();
    let mut title_id = 0u64;

    // Parse NSP PFS0
    if let Ok(pfs) = Pfs::parse(file.as_ref(), 0) {
        // Register ticket keys
        for tik_entry in pfs.files_with_extension("tik") {
            if let Some(tik_file) = pfs.open_file(file.clone(), &tik_entry.name) {
                if let Ok(tik_data) = tik_file.read_all() {
                    if let Ok(ticket) = Ticket::parse(&tik_data) {
                        let _ = ticket.register_title_key(keys);
                    }
                }
            }
        }

        // Try each NCA to find Control type for metadata
        for nca_entry in pfs.files_with_extension("nca") {
            if let Some(nca_file) = pfs.open_file(file.clone(), &nca_entry.name) {
                let mut header_buf = vec![0u8; nca::NCA_HEADER_SIZE];
                if nca_file.read(0, &mut header_buf).unwrap_or(0) < nca::NCA_HEADER_SIZE {
                    continue;
                }

                if let Ok(header) = nca::parse_nca_header(&header_buf, keys) {
                    if title_id == 0 {
                        title_id = header.title_id;
                    }

                    if header.content_type == nca::NcaContentType::Control {
                        if let Ok(Some(data)) =
                            nca::open_control(nca_file.as_ref(), &header, keys)
                        {
                            if let Some(nacp) = try_parse_nacp_from_control(&data) {
                                title = nacp.get_application_name();
                                developer = nacp.get_developer();
                                version = nacp.version;
                                if nacp.title_id != 0 {
                                    title_id = nacp.title_id;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(GameInfo {
        path: path.to_path_buf(),
        format: GameFormat::Nsp,
        title_id,
        title,
        developer,
        version,
        icon_data: None,
    })
}

/// Scan an XCI file for metadata.
fn scan_xci(path: &Path, keys: &mut KeyManager, fallback_name: &str) -> Result<GameInfo, ScanError> {
    let file: Arc<dyn VfsFile> = Arc::new(RealFile::open(path)?);

    let mut title = fallback_name.to_string();
    let mut developer = String::new();
    let mut version = String::new();
    let mut title_id = 0u64;

    // Parse XCI header to find root HFS0
    if let Ok(xci_header) = crate::xci::parse_xci_header(file.as_ref()) {
        // Parse root HFS0
        if let Ok(root_pfs) = Pfs::parse(file.as_ref(), xci_header.root_partition_offset) {
            // Find secure partition
            if let Some(secure_entry) = root_pfs
                .entries
                .iter()
                .find(|e| e.name.to_lowercase() == "secure")
            {
                let secure_offset = root_pfs.data_offset + secure_entry.data_offset;
                let secure_file: Arc<dyn VfsFile> = Arc::new(crate::vfs::OffsetFile::new(
                    file.clone(),
                    "secure".to_string(),
                    secure_offset,
                    secure_entry.data_size,
                ));

                // Parse secure partition as PFS0 (like NSP)
                if let Ok(secure_pfs) = Pfs::parse(secure_file.as_ref(), 0) {
                    for nca_entry in secure_pfs.files_with_extension("nca") {
                        if let Some(nca_file) =
                            secure_pfs.open_file(secure_file.clone(), &nca_entry.name)
                        {
                            let mut header_buf = vec![0u8; nca::NCA_HEADER_SIZE];
                            if nca_file.read(0, &mut header_buf).unwrap_or(0)
                                < nca::NCA_HEADER_SIZE
                            {
                                continue;
                            }

                            if let Ok(header) = nca::parse_nca_header(&header_buf, keys) {
                                if title_id == 0 {
                                    title_id = header.title_id;
                                }

                                if header.content_type == nca::NcaContentType::Control {
                                    if let Ok(Some(data)) =
                                        nca::open_control(nca_file.as_ref(), &header, keys)
                                    {
                                        if let Some(nacp) = try_parse_nacp_from_control(&data) {
                                            title = nacp.get_application_name();
                                            developer = nacp.get_developer();
                                            version = nacp.version;
                                            if nacp.title_id != 0 {
                                                title_id = nacp.title_id;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(GameInfo {
        path: path.to_path_buf(),
        format: GameFormat::Xci,
        title_id,
        title,
        developer,
        version,
        icon_data: None,
    })
}

/// Try to parse NACP from control data (PFS0 or raw).
fn try_parse_nacp_from_control(data: &[u8]) -> Option<Nacp> {
    // Try PFS0 first
    let vec_file = Arc::new(VecFile::new("control".to_string(), data.to_vec()));
    if let Ok(pfs) = Pfs::parse(vec_file.as_ref(), 0) {
        if let Some(nacp_file) = pfs.open_file(vec_file.clone(), "control.nacp") {
            if let Ok(nacp_data) = nacp_file.read_all() {
                if let Ok(nacp) = Nacp::parse(&nacp_data) {
                    return Some(nacp);
                }
            }
        }
    }

    // Try raw NACP
    if data.len() >= crate::nacp::NACP_SIZE {
        Nacp::parse(data).ok()
    } else {
        None
    }
}

/// Scan a directory for game files and return sorted metadata.
pub fn scan_directory(dir: &Path, keys: &mut KeyManager) -> Vec<GameInfo> {
    let mut games = Vec::new();

    let entries = match std::fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(e) => {
            log::warn!("Failed to read games directory {}: {}", dir.display(), e);
            return games;
        }
    };

    for entry in entries {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue,
        };

        let path = entry.path();
        if !path.is_file() {
            continue;
        }

        if GameFormat::from_extension(&path).is_none() {
            continue;
        }

        match scan_game(&path, keys) {
            Ok(info) => {
                log::info!(
                    "Found game: {} ({}) [{}]",
                    info.title,
                    info.format.label(),
                    path.display()
                );
                games.push(info);
            }
            Err(e) => {
                log::warn!("Failed to scan {}: {}", path.display(), e);
            }
        }
    }

    // Sort by title
    games.sort_by(|a, b| a.title.to_lowercase().cmp(&b.title.to_lowercase()));

    games
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_game_format_from_extension() {
        assert_eq!(
            GameFormat::from_extension(Path::new("game.nsp")),
            Some(GameFormat::Nsp)
        );
        assert_eq!(
            GameFormat::from_extension(Path::new("game.NSP")),
            Some(GameFormat::Nsp)
        );
        assert_eq!(
            GameFormat::from_extension(Path::new("game.xci")),
            Some(GameFormat::Xci)
        );
        assert_eq!(
            GameFormat::from_extension(Path::new("game.nro")),
            Some(GameFormat::Nro)
        );
        assert_eq!(GameFormat::from_extension(Path::new("game.txt")), None);
        assert_eq!(GameFormat::from_extension(Path::new("noext")), None);
    }

    #[test]
    fn test_game_format_label() {
        assert_eq!(GameFormat::Nsp.label(), "NSP");
        assert_eq!(GameFormat::Xci.label(), "XCI");
        assert_eq!(GameFormat::Nro.label(), "NRO");
    }

    #[test]
    fn test_scan_directory_empty() {
        let dir = tempfile::tempdir().unwrap();
        let mut keys = KeyManager::new();
        let games = scan_directory(dir.path(), &mut keys);
        assert!(games.is_empty());
    }

    #[test]
    fn test_scan_directory_nonexistent() {
        let mut keys = KeyManager::new();
        let games = scan_directory(Path::new("/nonexistent/path"), &mut keys);
        assert!(games.is_empty());
    }
}
