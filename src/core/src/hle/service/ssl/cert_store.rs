// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ssl/cert_store.h
//! Port of zuyu/src/core/hle/service/ssl/cert_store.cpp

use std::collections::BTreeMap;

use crate::core::SystemRef;
use crate::file_sys::nca_metadata::ContentRecordType;
use crate::file_sys::registered_cache::ContentProvider;
use crate::file_sys::romfs::extract_romfs;
use crate::hle::result::{ResultCode, RESULT_UNKNOWN};

use super::ssl_types::{BuiltInCertificateInfo, CaCertificateId, TrustedCertStatus};

struct Certificate {
    status: TrustedCertStatus,
    der_data: Vec<u8>,
}

/// CertStore manages trusted root certificates.
///
pub struct CertStore {
    certs: BTreeMap<CaCertificateId, Certificate>,
}

impl CertStore {
    pub fn new(system: SystemRef) -> Self {
        const CERT_STORE_DATA_ID: u64 = 0x0100_0000_0000_0800;
        const HEADER_SIZE: usize = 0x8;
        const ENTRY_SIZE: usize = 0x10;

        let mut store = Self {
            certs: BTreeMap::new(),
        };
        let filesystem_controller = system.get().get_filesystem_controller();
        let controller = filesystem_controller.lock().unwrap();
        let Some(nca) = controller
            .get_system_nand_contents()
            .and_then(|contents| contents.get_entry(CERT_STORE_DATA_ID, ContentRecordType::Data))
        else {
            return store;
        };
        drop(controller);
        let Some(romfs) = nca.get_romfs() else {
            return store;
        };
        let Some(extracted) = extract_romfs(Some(romfs)) else {
            log::error!("CertStore could not be extracted, corrupt RomFS?");
            return store;
        };
        let Some(file) = extracted.get_file("ssl_TrustedCerts.bdf") else {
            log::error!("Failed to find trusted certificates in CertStore");
            return store;
        };

        let header = file.read_bytes(HEADER_SIZE, 0);
        if header.len() != HEADER_SIZE {
            return store;
        }
        let magic = u32::from_le_bytes(header[0..4].try_into().unwrap());
        let num_entries = u32::from_le_bytes(header[4..8].try_into().unwrap()) as usize;
        if magic != u32::from_le_bytes(*b"sslT") {
            log::error!("Invalid certificate store magic");
            return store;
        }
        let expected_size = HEADER_SIZE.saturating_add(ENTRY_SIZE.saturating_mul(num_entries));
        if file.get_size() < expected_size {
            log::error!(
                "Size mismatch, expected at least {} bytes, got {}",
                expected_size,
                file.get_size()
            );
            return store;
        }

        let entries = file.read_bytes(ENTRY_SIZE.saturating_mul(num_entries), HEADER_SIZE);
        for entry in entries.chunks_exact(ENTRY_SIZE) {
            let certificate_id =
                CaCertificateId::from_raw(i32::from_le_bytes(entry[0..4].try_into().unwrap()));
            let status =
                TrustedCertStatus::from_raw(i32::from_le_bytes(entry[4..8].try_into().unwrap()));
            let der_size = u32::from_le_bytes(entry[8..12].try_into().unwrap()) as usize;
            let der_offset = u32::from_le_bytes(entry[12..16].try_into().unwrap()) as usize;
            let der_data = file.read_bytes(der_size, HEADER_SIZE.saturating_add(der_offset));
            store
                .certs
                .insert(certificate_id, Certificate { status, der_data });
        }
        store
    }

    /// Get certificates matching the given IDs.
    ///
    /// Writes certificate data to out_data and returns the number of entries.
    pub fn get_certificates(
        &self,
        out_data: &mut [u8],
        certificate_ids: &[CaCertificateId],
    ) -> Result<u32, ResultCode> {
        let (required_size, num_entries) = self.get_certificate_buf_size(certificate_ids)?;
        if out_data.len() < required_size as usize {
            return Err(RESULT_UNKNOWN);
        }
        let info_size = std::mem::size_of::<BuiltInCertificateInfo>();
        let der_data_offset = (num_entries as usize + 1) * info_size;
        let mut current_der_offset = der_data_offset;
        let mut info_offset = 0usize;
        self.for_each_certificate(certificate_ids, |id, cert| {
            write_certificate_info(
                &mut out_data[info_offset..info_offset + info_size],
                id,
                cert.status,
                cert.der_data.len() as u64,
                current_der_offset as u64,
            );
            info_offset += info_size;
            let end = current_der_offset + cert.der_data.len();
            out_data[current_der_offset..end].copy_from_slice(&cert.der_data);
            current_der_offset = end;
        });
        write_certificate_info(
            &mut out_data[info_offset..info_offset + info_size],
            CaCertificateId::All,
            TrustedCertStatus::Invalid,
            0,
            0,
        );
        Ok(num_entries)
    }

    /// Get the total buffer size needed for the given certificate IDs.
    pub fn get_certificate_buf_size(
        &self,
        certificate_ids: &[CaCertificateId],
    ) -> Result<(u32, u32), ResultCode> {
        let mut total_size = std::mem::size_of::<BuiltInCertificateInfo>() as u32;
        let mut num_entries = 0u32;

        self.for_each_certificate(certificate_ids, |_id, cert| {
            total_size += std::mem::size_of::<BuiltInCertificateInfo>() as u32;
            total_size += common::alignment::align_up(cert.der_data.len() as u64, 4) as u32;
            num_entries += 1;
        });

        Ok((total_size, num_entries))
    }

    fn for_each_certificate<F>(&self, ids: &[CaCertificateId], mut f: F)
    where
        F: FnMut(CaCertificateId, &Certificate),
    {
        for &id in ids {
            if ids.len() == 1 && id == CaCertificateId::All {
                for (&cert_id, cert) in &self.certs {
                    f(cert_id, cert);
                }
                return;
            }

            if let Some(cert) = self.certs.get(&id) {
                f(id, cert);
            }
        }
    }
}

fn write_certificate_info(
    output: &mut [u8],
    id: CaCertificateId,
    status: TrustedCertStatus,
    der_size: u64,
    der_offset: u64,
) {
    output[0..4].copy_from_slice(&id.raw().to_le_bytes());
    output[4..8].copy_from_slice(&status.raw().to_le_bytes());
    output[8..16].copy_from_slice(&der_size.to_le_bytes());
    output[16..24].copy_from_slice(&der_offset.to_le_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_i32(bytes: &[u8], offset: usize) -> i32 {
        i32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap())
    }

    fn read_u64(bytes: &[u8], offset: usize) -> u64 {
        u64::from_le_bytes(bytes[offset..offset + 8].try_into().unwrap())
    }

    fn test_store() -> CertStore {
        CertStore {
            certs: BTreeMap::from([
                (
                    CaCertificateId::from_raw(1),
                    Certificate {
                        status: TrustedCertStatus::from_raw(1),
                        der_data: vec![0x11, 0x12, 0x13],
                    },
                ),
                (
                    CaCertificateId::from_raw(2),
                    Certificate {
                        status: TrustedCertStatus::from_raw(3),
                        der_data: vec![0x21, 0x22, 0x23, 0x24],
                    },
                ),
            ]),
        }
    }

    #[test]
    fn all_certificates_match_upstream_layout() {
        let store = test_store();
        let ids = [CaCertificateId::All];
        let (size, count) = store.get_certificate_buf_size(&ids).unwrap();
        assert_eq!((size, count), (80, 2));

        let mut output = vec![0xCC; size as usize];
        assert_eq!(store.get_certificates(&mut output, &ids).unwrap(), 2);
        assert_eq!(read_i32(&output, 0), 1);
        assert_eq!(read_i32(&output, 4), 1);
        assert_eq!(read_u64(&output, 8), 3);
        assert_eq!(read_u64(&output, 16), 72);
        assert_eq!(read_i32(&output, 24), 2);
        assert_eq!(read_i32(&output, 28), 3);
        assert_eq!(read_u64(&output, 32), 4);
        assert_eq!(read_u64(&output, 40), 75);
        assert_eq!(read_i32(&output, 48), -1);
        assert_eq!(read_i32(&output, 52), -1);
        assert_eq!(&output[72..79], &[0x11, 0x12, 0x13, 0x21, 0x22, 0x23, 0x24]);
        assert_eq!(output[79], 0xCC);
    }

    #[test]
    fn all_is_special_only_when_it_is_the_only_requested_id() {
        let store = test_store();
        let ids = [CaCertificateId::All, CaCertificateId::from_raw(2)];
        let (size, count) = store.get_certificate_buf_size(&ids).unwrap();
        assert_eq!((size, count), (52, 1));
        let mut output = vec![0; size as usize];
        assert_eq!(store.get_certificates(&mut output, &ids).unwrap(), 1);
        assert_eq!(read_i32(&output, 0), 2);
        assert_eq!(read_u64(&output, 16), 48);
        assert_eq!(&output[48..52], &[0x21, 0x22, 0x23, 0x24]);
    }

    #[test]
    fn rejects_output_smaller_than_reported_size() {
        let store = test_store();
        let mut output = vec![0; 79];
        assert_eq!(
            store.get_certificates(&mut output, &[CaCertificateId::All]),
            Err(RESULT_UNKNOWN)
        );
    }
}
