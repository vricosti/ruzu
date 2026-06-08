// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ssl/ssl.h
//! Port of zuyu/src/core/hle/service/ssl/ssl.cpp

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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

#[repr(C)]
#[derive(Clone, Copy, Default)]
struct CreateContextParameters {
    ssl_version: SslVersion,
    _padding: u32,
    pid_placeholder: u64,
}

const _: () = assert!(std::mem::size_of::<CreateContextParameters>() == 0x10);

#[repr(C)]
#[derive(Clone, Copy, Default)]
struct ContextOptionParameters {
    option: u32,
    value: i32,
}

const _: () = assert!(std::mem::size_of::<ContextOptionParameters>() == 0x8);

pub struct ISslContext {
    ssl_version: SslVersion,
    shared_data: std::sync::Mutex<SslContextSharedData>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISslContext {
    pub fn new(ssl_version: SslVersion) -> Self {
        Self {
            ssl_version,
            shared_data: std::sync::Mutex::new(SslContextSharedData::default()),
            handlers: build_handler_map(&[
                (0, Some(ISslContext::set_option_handler), "SetOption"),
                (1, None, "GetOption"),
                (2, None, "CreateConnection"),
                (
                    3,
                    Some(ISslContext::get_connection_count_handler),
                    "GetConnectionCount",
                ),
                (
                    4,
                    Some(ISslContext::import_server_pki_handler),
                    "ImportServerPki",
                ),
                (
                    5,
                    Some(ISslContext::import_client_pki_handler),
                    "ImportClientPki",
                ),
                (6, None, "RemoveServerPki"),
                (7, None, "RemoveClientPki"),
                (8, None, "RegisterInternalPki"),
                (9, None, "AddPolicyOid"),
                (10, None, "ImportCrl"),
                (11, None, "RemoveCrl"),
                (12, None, "ImportClientCertKeyPki"),
                (13, None, "GeneratePrivateKeyAndCert"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn set_option(&self, option: u32, value: i32) {
        log::warn!(
            "ISslContext::SetOption (STUBBED) called, option={}, value={}",
            option,
            value
        );
    }

    fn connection_count(&self) -> u32 {
        self.shared_data.lock().unwrap().connection_count
    }

    fn set_option_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISslContext) };
        let mut rp = RequestParser::new(ctx);
        let params = rp.pop_raw::<ContextOptionParameters>();
        service.set_option(params.option, params.value);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_connection_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISslContext) };
        log::debug!(
            "ISslContext::GetConnectionCount connection_count={}",
            service.connection_count()
        );
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(service.connection_count());
    }

    fn import_server_pki_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = unsafe { &*(this as *const dyn ServiceFramework as *const ISslContext) };
        let mut rp = RequestParser::new(ctx);
        let certificate_format = rp.pop_u32();
        let _pkcs_12_certificates = ctx.read_buffer(0);
        log::warn!(
            "ISslContext::ImportServerPki (STUBBED) called, certificate_format={}",
            certificate_format
        );

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0);
    }

    fn import_client_pki_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let _service = unsafe { &*(this as *const dyn ServiceFramework as *const ISslContext) };
        let _pkcs_12_certificate = ctx.read_buffer(0);
        let _ascii_password = if ctx.can_read_buffer(1) {
            ctx.read_buffer(1)
        } else {
            Vec::new()
        };
        log::warn!("ISslContext::ImportClientPki (STUBBED) called");

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0);
    }
}

impl SessionRequestHandler for ISslContext {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ISslContext"
    }
}

impl ServiceFramework for ISslContext {
    fn get_service_name(&self) -> &str {
        "ISslContext"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

pub struct ISslService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISslService {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    0,
                    Some(ISslService::create_context_handler),
                    "CreateContext",
                ),
                (1, None, "GetContextCount"),
                (2, None, "GetCertificates"),
                (3, None, "GetCertificateBufSize"),
                (4, None, "DebugIoctl"),
                (
                    5,
                    Some(ISslService::set_interface_version_handler),
                    "SetInterfaceVersion",
                ),
                (6, None, "FlushSessionCache"),
                (7, None, "SetDebugOption"),
                (8, None, "GetDebugOption"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn create_context(&self, ssl_version: SslVersion, pid_placeholder: u64) -> ISslContext {
        log::warn!(
            "ISslService::CreateContext (STUBBED) called, api_version={}, pid_placeholder={}",
            ssl_version.api_version(),
            pid_placeholder
        );
        ISslContext::new(ssl_version)
    }

    fn set_interface_version(&self, ssl_version: u32) {
        log::debug!(
            "ISslService::SetInterfaceVersion called, ssl_version={}",
            ssl_version
        );
    }

    fn create_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISslService) };
        let mut rp = RequestParser::new(ctx);
        let params = rp.pop_raw::<CreateContextParameters>();
        let context = service.create_context(params.ssl_version, params.pid_placeholder);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(context));
    }

    fn set_interface_version_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const ISslService) };
        let mut rp = RequestParser::new(ctx);
        service.set_interface_version(rp.pop_u32());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for ISslService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ssl"
    }
}

impl ServiceFramework for ISslService {
    fn get_service_name(&self) -> &str {
        "ssl"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers the "ssl" service.
///
/// Corresponds to `Service::SSL::LoopProcess` in upstream `ssl.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);
    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "ssl",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(ISslService::new()) }),
            64,
        );
    }
    ServerManager::run_server_shared(server_manager);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ssl_service_handler_table_matches_upstream_slice() {
        let service = ISslService::new();
        assert_eq!(service.handlers().len(), 9);
        assert!(service.handlers().contains_key(&0));
        assert!(service.handlers().contains_key(&5));
    }

    #[test]
    fn ssl_context_handler_table_matches_upstream_slice() {
        let context = ISslContext::new(SslVersion::default());
        assert_eq!(context.handlers().len(), 14);
        assert!(context.handlers().contains_key(&0));
        assert!(context.handlers().contains_key(&2));
        assert!(context.handlers().contains_key(&5));
    }

    #[test]
    fn ssl_create_context_payload_layout_matches_upstream() {
        assert_eq!(std::mem::size_of::<CreateContextParameters>(), 0x10);
        assert_eq!(std::mem::size_of::<ContextOptionParameters>(), 0x8);
    }
}
