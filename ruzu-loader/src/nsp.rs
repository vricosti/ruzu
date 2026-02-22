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
    /// Loaded executable code.
    pub code_set: CodeSet,
    /// Title ID from the NCA header.
    pub title_id: u64,
    /// Application name (from Control NCA NACP, if available).
    pub title: Option<String>,
    /// Developer name (from Control NCA NACP, if available).
    pub developer: Option<String>,
    /// Version string (from Control NCA NACP, if available).
    pub version: Option<String>,
}

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

    // Open ExeFS from Program NCA
    let (exefs_pfs, exefs_data) = nca::open_exefs(program_nca.file.as_ref(), &program_nca.header, keys)?
        .ok_or(NspError::NoExeFs)?;

    // Find 'main' NSO in ExeFS
    let exefs_vfs = Arc::new(crate::vfs::VecFile::new("exefs".to_string(), exefs_data));
    let main_file = exefs_pfs
        .open_file(exefs_vfs, "main")
        .ok_or(NspError::NoMainNso)?;

    let nso_data = main_file.read_all().map_err(NspError::Io)?;
    let code_set = nso::load_nso(&nso_data)?;

    Ok(NspLoadResult {
        code_set,
        title_id: program_nca.header.title_id,
        title,
        developer,
        version,
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
