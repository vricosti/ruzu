// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! NSP (Nintendo Submission Package) loader.
//!
//! An NSP is a PFS0 container holding `.nca` and `.tik` (ticket) files.
//! Loading an NSP involves:
//! 1. Parse the outer PFS0
//! 2. Register any ticket title keys
//! 3. Identify the Program NCA and Control NCA
//! 4. Decrypt NCA headers to determine content types
//! 5. Extract ExeFS from the Program NCA → load NSO (`main`)

use crate::nca::{self, NcaContentType, NcaError, NcaHeader};
use crate::nro::CodeSet;
use crate::nso::{self, NsoError};
use crate::pfs::{Pfs, PfsError};
use crate::vfs::VfsFile;
use ruzu_crypto::key_manager::KeyManager;
use ruzu_crypto::ticket::{Ticket, TicketError};
use std::sync::Arc;
use thiserror::Error;

/// Errors from NSP loading.
#[derive(Debug, Error)]
pub enum NspError {
    #[error("PFS error: {0}")]
    Pfs(#[from] PfsError),

    #[error("NCA error: {0}")]
    Nca(#[from] NcaError),

    #[error("NSO error: {0}")]
    Nso(#[from] NsoError),

    #[error("ticket error: {0}")]
    Ticket(#[from] TicketError),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("no Program NCA found in NSP")]
    NoProgramNca,

    #[error("no 'main' NSO found in ExeFS")]
    NoMainNso,

    #[error("ExeFS not found in Program NCA")]
    NoExeFs,
}

/// Result of loading an NSP.
pub struct NspLoadResult {
    /// Loaded executable modules (name, code_set) in load order.
    /// Matches zuyu: rtld, main, subsdk0-9, sdk.
    pub modules: Vec<(String, CodeSet)>,
    /// Title ID from the NCA header.
    pub title_id: u64,
    /// Whether the program targets AArch64 (true) or AArch32 (false).
    /// Determined from the NPDM flags byte (bit 0).
    pub is_64bit: bool,
    /// Application name (from Control NCA NACP, if available).
    pub title: Option<String>,
    /// Developer name (from Control NCA NACP, if available).
    pub developer: Option<String>,
    /// Version string (from Control NCA NACP, if available).
    pub version: Option<String>,
    /// Raw RomFS data from the Program NCA (section 1), if available.
    pub romfs_data: Option<Vec<u8>>,
}

/// Module names in zuyu's loading order (deconstructed_rom_directory.cpp).
const STATIC_MODULES: &[&str] = &[
    "rtld", "main", "subsdk0", "subsdk1", "subsdk2", "subsdk3", "subsdk4",
    "subsdk5", "subsdk6", "subsdk7", "subsdk8", "subsdk9", "sdk",
];

/// Parsed NCA info for an NSP.
pub struct NspNcaInfo {
    /// The PFS0 entry name.
    pub name: String,
    /// Parsed NCA header.
    pub header: NcaHeader,
    /// The VfsFile for this NCA.
    pub file: Arc<dyn VfsFile>,
}

/// Parse an NSP and identify all NCAs.
///
/// Returns the parsed PFS0, registered ticket keys, and NCA info.
pub fn parse_nsp(
    file: Arc<dyn VfsFile>,
    keys: &mut KeyManager,
) -> Result<(Pfs, Vec<NspNcaInfo>), NspError> {
    // Parse outer PFS0
    let pfs = Pfs::parse(file.as_ref(), 0)?;
    log::info!("NSP contains {} files", pfs.entries.len());

    // Register ticket title keys
    let tik_entries = pfs.files_with_extension("tik");
    for tik_entry in &tik_entries {
        if let Some(tik_file) = pfs.open_file(file.clone(), &tik_entry.name) {
            let tik_data = tik_file.read_all().map_err(NspError::Io)?;
            match Ticket::parse(&tik_data) {
                Ok(ticket) => {
                    if let Err(e) = ticket.register_title_key(keys) {
                        log::warn!(
                            "Failed to register title key from {}: {}",
                            tik_entry.name,
                            e
                        );
                    }
                }
                Err(e) => {
                    log::warn!("Failed to parse ticket {}: {}", tik_entry.name, e);
                }
            }
        }
    }

    // Parse NCA headers
    let nca_entries = pfs.files_with_extension("nca");
    let mut nca_infos = Vec::new();

    for nca_entry in &nca_entries {
        if let Some(nca_file) = pfs.open_file(file.clone(), &nca_entry.name) {
            // Read at least the header
            let header_data = {
                let mut buf = vec![0u8; nca::NCA_HEADER_SIZE];
                let n = nca_file.read(0, &mut buf).map_err(NspError::Io)?;
                if n < nca::NCA_HEADER_SIZE {
                    log::warn!(
                        "NCA {} too small for header ({} bytes), skipping",
                        nca_entry.name,
                        n
                    );
                    continue;
                }
                buf
            };

            match nca::parse_nca_header(&header_data, keys) {
                Ok(header) => {
                    log::info!(
                        "NCA {}: type={:?}, title_id=0x{:016X}",
                        nca_entry.name,
                        header.content_type,
                        header.title_id
                    );
                    nca_infos.push(NspNcaInfo {
                        name: nca_entry.name.clone(),
                        header,
                        file: nca_file,
                    });
                }
                Err(e) => {
                    log::warn!("Failed to parse NCA header for {}: {}", nca_entry.name, e);
                }
            }
        }
    }

    Ok((pfs, nca_infos))
}

/// Load an NSP: parse container, decrypt NCA, extract NSO, produce CodeSet.
pub fn load_nsp(
    file: Arc<dyn VfsFile>,
    keys: &mut KeyManager,
) -> Result<NspLoadResult, NspError> {
    let (_pfs, nca_infos) = parse_nsp(file, keys)?;

    // Find the Program NCA
    let program_nca = nca_infos
        .iter()
        .find(|info| info.header.content_type == NcaContentType::Program)
        .ok_or(NspError::NoProgramNca)?;

    // Try to extract metadata from Control NCA
    let mut title = None;
    let mut developer = None;
    let mut version = None;

    if let Some(control_nca) = nca_infos
        .iter()
        .find(|info| info.header.content_type == NcaContentType::Control)
    {
        match extract_nacp_from_control(&control_nca.file, &control_nca.header, keys) {
            Ok(Some(nacp)) => {
                title = Some(nacp.get_application_name());
                developer = Some(nacp.get_developer());
                version = Some(nacp.version.clone());
            }
            Ok(None) => {}
            Err(e) => {
                log::warn!("Failed to extract NACP from Control NCA: {}", e);
            }
        }
    }

    // Open ExeFS from Program NCA (small, needed for boot)
    let (exefs_pfs, exefs_data) = nca::open_exefs(program_nca.file.as_ref(), &program_nca.header, keys)?
        .ok_or(NspError::NoExeFs)?;

    // Log ExeFS file listing for diagnostics.
    let exefs_vfs = Arc::new(crate::vfs::VecFile::new("exefs".to_string(), exefs_data));
    log::info!("ExeFS PFS0 contains {} files:", exefs_pfs.entries.len());
    for entry in &exefs_pfs.entries {
        log::info!("  '{}' (offset=0x{:X}, size=0x{:X})", entry.name, entry.data_offset, entry.data_size);
    }

    // Check for main.npdm (architecture metadata).
    // Default to 64-bit if NPDM is missing (most Switch titles are AArch64).
    let mut is_64bit = true;
    if let Some(npdm_file) = exefs_pfs.open_file(exefs_vfs.clone(), "main.npdm") {
        let npdm_data = npdm_file.read_all().map_err(NspError::Io)?;
        if npdm_data.len() >= 0x10 {
            let magic = &npdm_data[0..4];
            let flags = npdm_data[0x0C];
            is_64bit = flags & 1 != 0;
            log::info!(
                "NPDM: magic={:?}, flags=0x{:02X}, is_64bit={}",
                std::str::from_utf8(magic).unwrap_or("????"),
                flags,
                is_64bit,
            );
        } else {
            log::warn!("NPDM file too small ({} bytes)", npdm_data.len());
        }
    } else {
        log::warn!("No 'main.npdm' found in ExeFS");
    }

    // Load all NSO modules from ExeFS in zuyu's order.
    let mut modules: Vec<(String, CodeSet)> = Vec::new();

    for &module_name in STATIC_MODULES {
        if let Some(module_file) = exefs_pfs.open_file(exefs_vfs.clone(), module_name) {
            let nso_data = module_file.read_all().map_err(NspError::Io)?;
            match nso::load_nso(&nso_data) {
                Ok(code_set) => {
                    log::info!(
                        "Loaded ExeFS module '{}': text=0x{:X}+0x{:X}, rodata=0x{:X}+0x{:X}, data=0x{:X}+0x{:X}, bss=0x{:X}",
                        module_name,
                        code_set.segments[0].offset, code_set.segments[0].size,
                        code_set.segments[1].offset, code_set.segments[1].size,
                        code_set.segments[2].offset, code_set.segments[2].size,
                        code_set.bss_size,
                    );
                    modules.push((module_name.to_string(), code_set));
                }
                Err(e) => {
                    log::warn!("Failed to load NSO '{}': {}", module_name, e);
                }
            }
        }
    }

    if modules.is_empty() {
        return Err(NspError::NoMainNso);
    }
    log::info!("Loaded {} ExeFS modules", modules.len());

    // Skip eager RomFS loading — it can be GBs and should be accessed lazily.
    // TODO: implement lazy VFS-based RomFS access instead of loading into memory.
    let romfs_data = None;
    if let Some(romfs_section) = program_nca
        .header
        .sections
        .iter()
        .find(|s| s.fs_type == nca::NcaSectionFsType::RomFs)
    {
        let size = romfs_section.end_offset - romfs_section.start_offset;
        log::info!(
            "RomFS section found: {} bytes (deferred, not loaded into memory)",
            size
        );
    } else {
        log::info!("No RomFS section found in Program NCA");
    }

    Ok(NspLoadResult {
        modules,
        title_id: program_nca.header.title_id,
        is_64bit,
        title,
        developer,
        version,
        romfs_data,
    })
}

/// Try to extract NACP from a Control NCA.
fn extract_nacp_from_control(
    file: &Arc<dyn VfsFile>,
    header: &NcaHeader,
    keys: &KeyManager,
) -> Result<Option<crate::nacp::Nacp>, NcaError> {
    let control_data = nca::open_control(file.as_ref(), header, keys)?;
    if let Some(data) = control_data {
        // Control data may be a PFS0 containing control.nacp, or raw NACP
        // Try PFS0 first
        let vec_file = Arc::new(crate::vfs::VecFile::new("control".to_string(), data.clone()));
        if let Ok(pfs) = Pfs::parse(vec_file.as_ref(), 0) {
            if let Some(nacp_file) = pfs.open_file(vec_file.clone(), "control.nacp") {
                let nacp_data = nacp_file.read_all().map_err(NcaError::Io)?;
                if let Ok(nacp) = crate::nacp::Nacp::parse(&nacp_data) {
                    return Ok(Some(nacp));
                }
            }
        }

        // Try parsing as raw NACP
        if data.len() >= crate::nacp::NACP_SIZE {
            if let Ok(nacp) = crate::nacp::Nacp::parse(&data) {
                return Ok(Some(nacp));
            }
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nsp_error_display() {
        let err = NspError::NoProgramNca;
        assert_eq!(err.to_string(), "no Program NCA found in NSP");

        let err = NspError::NoMainNso;
        assert_eq!(err.to_string(), "no 'main' NSO found in ExeFS");
    }
}
