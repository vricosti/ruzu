// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ssl/ssl.h
//! Port of zuyu/src/core/hle/service/ssl/ssl.cpp

/// nn::ssl::sf::CertificateFormat
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CertificateFormat {
    Pem = 1,
    Der = 2,
}

/// nn::ssl::sf::ContextOption
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContextOption {
    None = 0,
    CrlImportDateCheckEnable = 1,
}

/// nn::ssl::Connection::IoMode
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IoMode {
    Blocking = 1,
    NonBlocking = 2,
}

/// nn::ssl::sf::OptionType
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptionType {
    DoNotCloseSocket = 0,
    GetServerCertChain = 1,
}

/// nn::ssl::sf::SslVersion
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SslVersion {
    pub raw: u32,
}

impl SslVersion {
    pub fn tls_auto(&self) -> bool {
        (self.raw & 1) != 0
    }

    pub fn tls_v10(&self) -> bool {
        (self.raw >> 3) & 1 != 0
    }

    pub fn tls_v11(&self) -> bool {
        (self.raw >> 4) & 1 != 0
    }

    pub fn tls_v12(&self) -> bool {
        (self.raw >> 5) & 1 != 0
    }

    pub fn tls_v13(&self) -> bool {
        (self.raw >> 6) & 1 != 0
    }

    pub fn api_version(&self) -> u32 {
        (self.raw >> 24) & 0x7F
    }
}

pub struct SslContextSharedData {
    pub connection_count: u32,
}

impl Default for SslContextSharedData {
    fn default() -> Self {
        Self {
            connection_count: 0,
        }
    }
}

/// Serialize server certificate chain for DoHandshakeGetServerCert.
///
/// If get_server_cert_chain is false, returns just the first cert.
/// Otherwise returns a structured buffer with magic header.
pub fn serialize_server_certs(certs: &[Vec<u8>], get_server_cert_chain: bool) -> Vec<u8> {
    if !get_server_cert_chain {
        // Just return the first one, unencoded.
        assert!(!certs.is_empty(), "Should be at least one server cert");
        return certs[0].clone();
    }

    let mut ret = Vec::new();

    // Header: magic (8 bytes) + count (4 bytes) + pad (4 bytes)
    let magic: u64 = 0x4E4D684374726543;
    ret.extend_from_slice(&magic.to_le_bytes());
    ret.extend_from_slice(&(certs.len() as u32).to_le_bytes());
    ret.extend_from_slice(&0u32.to_le_bytes());

    // Entry headers: size (4 bytes) + offset (4 bytes) each
    let header_size = 16 + certs.len() * 8;
    let mut data_offset = header_size;
    for cert in certs {
        ret.extend_from_slice(&(cert.len() as u32).to_le_bytes());
        ret.extend_from_slice(&(data_offset as u32).to_le_bytes());
        data_offset += cert.len();
    }

    // Certificate data
    for cert in certs {
        ret.extend_from_slice(cert);
    }

    ret
}
