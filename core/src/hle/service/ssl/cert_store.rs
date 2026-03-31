// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ssl/cert_store.h
//! Port of zuyu/src/core/hle/service/ssl/cert_store.cpp

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;

use super::ssl_types::{CaCertificateId, TrustedCertStatus};

struct Certificate {
    status: TrustedCertStatus,
    der_data: Vec<u8>,
}

/// CertStore manages trusted root certificates.
///
/// In upstream C++, the CertStore reads certificates from the system
/// NAND partition. Here we provide the structure and methods, but
/// certificate loading depends on the filesystem infrastructure.
pub struct CertStore {
    certs: BTreeMap<CaCertificateId, Certificate>,
}

impl CertStore {
    pub fn new() -> Self {
        // In upstream, this loads certificates from the system partition.
        Self {
            certs: BTreeMap::new(),
        }
    }

    /// Get certificates matching the given IDs.
    ///
    /// Writes certificate data to out_data and returns the number of entries.
    pub fn get_certificates(
        &self,
        out_data: &mut [u8],
        certificate_ids: &[CaCertificateId],
    ) -> Result<u32, ResultCode> {
        let mut num_entries = 0u32;
        let mut offset = 0usize;

        self.for_each_certificate(certificate_ids, |_id, cert| {
            if offset + cert.der_data.len() <= out_data.len() {
                out_data[offset..offset + cert.der_data.len()].copy_from_slice(&cert.der_data);
                offset += cert.der_data.len();
                num_entries += 1;
            }
        });

        Ok(num_entries)
    }

    /// Get the total buffer size needed for the given certificate IDs.
    pub fn get_certificate_buf_size(
        &self,
        certificate_ids: &[CaCertificateId],
    ) -> Result<(u32, u32), ResultCode> {
        let mut total_size = 0u32;
        let mut num_entries = 0u32;

        self.for_each_certificate(certificate_ids, |_id, cert| {
            total_size += cert.der_data.len() as u32;
            num_entries += 1;
        });

        Ok((total_size, num_entries))
    }

    fn for_each_certificate<F>(&self, ids: &[CaCertificateId], mut f: F)
    where
        F: FnMut(CaCertificateId, &Certificate),
    {
        for &id in ids {
            if id == CaCertificateId::All {
                // Return all certificates
                for (&cert_id, cert) in &self.certs {
                    if cert.status == TrustedCertStatus::EnabledTrusted {
                        f(cert_id, cert);
                    }
                }
                return;
            }

            if let Some(cert) = self.certs.get(&id) {
                if cert.status == TrustedCertStatus::EnabledTrusted {
                    f(id, cert);
                }
            }
        }
    }
}

impl Default for CertStore {
    fn default() -> Self {
        Self::new()
    }
}
