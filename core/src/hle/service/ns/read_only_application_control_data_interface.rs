// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/read_only_application_control_data_interface.h
//! Port of zuyu/src/core/hle/service/ns/read_only_application_control_data_interface.cpp
//!
//! IReadOnlyApplicationControlDataInterface — reads NACP and icon data.

use std::collections::BTreeMap;

use crate::file_sys::control_metadata::RawNACP;
use crate::file_sys::patch_manager::PatchManager;
use crate::hle::result::RESULT_SUCCESS;
use crate::hle::result::ResultCode;
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::ns::language::{
    convert_to_application_language, convert_to_language_code,
    get_application_language_priority_list, get_supported_language_flag, ApplicationLanguage,
};
use crate::hle::service::ns::ns_results::RESULT_APPLICATION_LANGUAGE_NOT_FOUND;
use crate::hle::service::set::settings_server::get_language_code_from_index;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Application control source.
///
/// Corresponds to `ApplicationControlSource` in upstream.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ApplicationControlSource {
    CacheOnly = 0,
    Storage = 1,
    StorageOnly = 2,
}

/// IPC command table for IReadOnlyApplicationControlDataInterface.
///
/// Corresponds to the function table in upstream.
pub mod commands {
    pub const GET_APPLICATION_CONTROL_DATA: u32 = 0;
    pub const GET_APPLICATION_DESIRED_LANGUAGE: u32 = 1;
    pub const CONVERT_APPLICATION_LANGUAGE_TO_LANGUAGE_CODE: u32 = 2;
    pub const CONVERT_LANGUAGE_CODE_TO_APPLICATION_LANGUAGE: u32 = 3;
    pub const SELECT_APPLICATION_DESIRED_LANGUAGE: u32 = 4;
}

/// IReadOnlyApplicationControlDataInterface.
///
/// Corresponds to `IReadOnlyApplicationControlDataInterface` in upstream.
pub struct IReadOnlyApplicationControlDataInterface {
    system: crate::core::SystemRef,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IReadOnlyApplicationControlDataInterface {
    pub fn new(system: crate::core::SystemRef) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::GET_APPLICATION_CONTROL_DATA,
                Some(Self::get_application_control_data_handler),
                "GetApplicationControlData",
            ),
            (
                commands::GET_APPLICATION_DESIRED_LANGUAGE,
                Some(Self::get_application_desired_language_handler),
                "GetApplicationDesiredLanguage",
            ),
            (
                commands::CONVERT_APPLICATION_LANGUAGE_TO_LANGUAGE_CODE,
                Some(Self::convert_application_language_to_language_code_handler),
                "ConvertApplicationLanguageToLanguageCode",
            ),
            (
                commands::CONVERT_LANGUAGE_CODE_TO_APPLICATION_LANGUAGE,
                None,
                "ConvertLanguageCodeToApplicationLanguage",
            ),
            (
                commands::SELECT_APPLICATION_DESIRED_LANGUAGE,
                None,
                "SelectApplicationDesiredLanguage",
            ),
        ]);
        Self {
            system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// GetApplicationControlData (cmd 0).
    ///
    /// Returns NACP data + icon for a given application_id.
    /// Corresponds to upstream `GetApplicationControlData`.
    pub fn get_application_control_data(
        &self,
        _source: ApplicationControlSource,
        application_id: u64,
        out_buffer: &mut [u8],
    ) -> Result<u32, ResultCode> {
        log::info!(
            "GetApplicationControlData called, application_id={:#018x}",
            application_id
        );

        const NACP_SIZE: usize = 0x4000;
        debug_assert_eq!(NACP_SIZE, std::mem::size_of::<RawNACP>());

        let metadata = if self.system.is_null() {
            (None, None)
        } else {
            let system = self.system.get();
            let fs_controller = system.get_filesystem_controller();
            let fs_controller = fs_controller.lock().unwrap();
            let provider = system.get_content_provider();
            match provider {
                Some(provider) => {
                    let provider = provider.lock().unwrap();
                    let patch_manager = PatchManager::new(application_id, &fs_controller, &*provider);
                    patch_manager.get_control_metadata()
                }
                None => (None, None),
            }
        };

        let icon_size = metadata.1.as_ref().map(|icon| icon.get_size()).unwrap_or(0);
        let total_size = NACP_SIZE + icon_size;

        if out_buffer.len() < total_size {
            log::error!(
                "output buffer is too small! (actual={:#x}, expected_min={:#x})",
                out_buffer.len(),
                total_size
            );
            return Err(ResultCode::new(1)); // ResultUnknown
        }

        if let Some(nacp) = metadata.0 {
            let bytes = nacp.get_raw_bytes();
            out_buffer[..NACP_SIZE].copy_from_slice(&bytes[..NACP_SIZE]);
        } else {
            log::warn!(
                "missing NACP data for application_id={:#018x}, defaulting to zero",
                application_id
            );
            out_buffer[..NACP_SIZE].fill(0);
        }

        if let Some(icon) = metadata.1 {
            let icon_size = icon.get_size();
            let read = icon.read(&mut out_buffer[NACP_SIZE..NACP_SIZE + icon_size], icon_size, 0);
            debug_assert_eq!(read, icon_size);
        } else {
            log::warn!("missing icon data for application_id={:#018x}", application_id);
        }

        Ok(total_size as u32)
    }

    /// GetApplicationDesiredLanguage (cmd 1).
    ///
    /// Corresponds to upstream `GetApplicationDesiredLanguage`.
    pub fn get_application_desired_language(
        &self,
        supported_languages: u32,
    ) -> Result<ApplicationLanguage, ResultCode> {
        log::info!(
            "GetApplicationDesiredLanguage called, supported_languages={:#010x}",
            supported_languages
        );

        let language_index = *common::settings::values().language_index.get_value() as usize;
        let language_code = get_language_code_from_index(language_index);
        let application_language = convert_to_application_language(language_code).ok_or_else(|| {
            log::error!(
                "Could not convert application language! language_code={:#018x}",
                language_code as u64
            );
            RESULT_APPLICATION_LANGUAGE_NOT_FOUND
        })?;
        let priority_list =
            get_application_language_priority_list(application_language).ok_or_else(|| {
                log::error!(
                    "Could not find application language priorities! application_language={:?}",
                    application_language
                );
                RESULT_APPLICATION_LANGUAGE_NOT_FOUND
            })?;

        for &lang in priority_list {
            let supported_flag = get_supported_language_flag(lang);
            if supported_languages == 0 || (supported_languages & supported_flag) == supported_flag {
                return Ok(lang);
            }
        }

        log::error!(
            "Could not find a valid language! supported_languages={:#010x}",
            supported_languages
        );
        Err(RESULT_APPLICATION_LANGUAGE_NOT_FOUND)
    }

    /// ConvertApplicationLanguageToLanguageCode (cmd 2).
    ///
    /// Corresponds to upstream `ConvertApplicationLanguageToLanguageCode`.
    pub fn convert_application_language_to_language_code(
        &self,
        application_language: ApplicationLanguage,
    ) -> Result<u64, ResultCode> {
        convert_to_language_code(application_language)
            .map(|language_code| language_code as u64)
            .ok_or_else(|| {
                log::error!(
                    "Language not found! application_language={:?}",
                    application_language
                );
                RESULT_APPLICATION_LANGUAGE_NOT_FOUND
            })
    }

    fn get_application_control_data_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const IReadOnlyApplicationControlDataInterface)
        };
        let mut rp = RequestParser::new(ctx);
        let source = match rp.pop_u8() {
            0 => ApplicationControlSource::CacheOnly,
            1 => ApplicationControlSource::Storage,
            2 => ApplicationControlSource::StorageOnly,
            other => {
                log::error!("Invalid ApplicationControlSource {}", other);
                ApplicationControlSource::Storage
            }
        };
        let application_id = rp.pop_u64();
        let mut out_buffer = vec![0u8; ctx.get_write_buffer_size(0)];
        match service.get_application_control_data(source, application_id, &mut out_buffer) {
            Ok(actual_size) => {
                ctx.write_buffer(&out_buffer, 0);
                let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u32(actual_size);
            }
            Err(result) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(result);
            }
        }
    }

    fn get_application_desired_language_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const IReadOnlyApplicationControlDataInterface)
        };
        let mut rp = RequestParser::new(ctx);
        let supported_languages = rp.pop_u32();
        match service.get_application_desired_language(supported_languages) {
            Ok(language) => {
                let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u32(language as u32);
            }
            Err(result) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(result);
            }
        }
    }

    fn convert_application_language_to_language_code_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const IReadOnlyApplicationControlDataInterface)
        };
        let mut rp = RequestParser::new(ctx);
        let application_language = match rp.pop_u8() {
            0 => ApplicationLanguage::AmericanEnglish,
            1 => ApplicationLanguage::BritishEnglish,
            2 => ApplicationLanguage::Japanese,
            3 => ApplicationLanguage::French,
            4 => ApplicationLanguage::German,
            5 => ApplicationLanguage::LatinAmericanSpanish,
            6 => ApplicationLanguage::Spanish,
            7 => ApplicationLanguage::Italian,
            8 => ApplicationLanguage::Dutch,
            9 => ApplicationLanguage::CanadianFrench,
            10 => ApplicationLanguage::Portuguese,
            11 => ApplicationLanguage::Russian,
            12 => ApplicationLanguage::Korean,
            13 => ApplicationLanguage::TraditionalChinese,
            14 => ApplicationLanguage::SimplifiedChinese,
            15 => ApplicationLanguage::BrazilianPortuguese,
            _ => ApplicationLanguage::Count,
        };
        match service.convert_application_language_to_language_code(application_language) {
            Ok(language_code) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(language_code);
            }
            Err(result) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(result);
            }
        }
    }
}

impl SessionRequestHandler for IReadOnlyApplicationControlDataInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::IReadOnlyApplicationControlDataInterface"
    }
}

impl ServiceFramework for IReadOnlyApplicationControlDataInterface {
    fn get_service_name(&self) -> &str {
        "ns::IReadOnlyApplicationControlDataInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::settings::Language;
    use crate::core::SystemRef;

    #[test]
    fn desired_language_uses_upstream_priority_list() {
        let previous = *common::settings::values().language_index.get_value();
        common::settings::values_mut()
            .language_index
            .set_value(Language::EnglishAmerican);

        let service =
            IReadOnlyApplicationControlDataInterface::new(crate::core::SystemRef::null());
        let supported = get_supported_language_flag(ApplicationLanguage::BritishEnglish)
            | get_supported_language_flag(ApplicationLanguage::French);
        let result = service.get_application_desired_language(supported).unwrap();

        common::settings::values_mut()
            .language_index
            .set_value(previous);

        assert_eq!(result, ApplicationLanguage::BritishEnglish);
    }

    #[test]
    fn convert_application_language_to_language_code_uses_ns_language_owner() {
        let service =
            IReadOnlyApplicationControlDataInterface::new(crate::core::SystemRef::null());
        let result = service
            .convert_application_language_to_language_code(ApplicationLanguage::BrazilianPortuguese)
            .unwrap();
        assert_eq!(
            result,
            crate::hle::service::set::settings_types::LanguageCode::PtBr as u64
        );
    }

    #[test]
    fn get_application_control_data_null_system_zero_fills_nacp() {
        let service = IReadOnlyApplicationControlDataInterface::new(SystemRef::null());
        let mut buffer = vec![0xAA; 0x4000];

        let size = service
            .get_application_control_data(ApplicationControlSource::Storage, 0x0100, &mut buffer)
            .unwrap();

        assert_eq!(size, 0x4000);
        assert!(buffer.iter().all(|b| *b == 0));
    }

    #[test]
    fn get_application_control_data_rejects_too_small_buffer() {
        let service = IReadOnlyApplicationControlDataInterface::new(SystemRef::null());
        let mut buffer = vec![0u8; 0x3FFF];

        let result =
            service.get_application_control_data(ApplicationControlSource::Storage, 0x0100, &mut buffer);

        assert!(result.is_err());
    }

    #[test]
    fn exercised_cmif_handlers_are_registered() {
        let service = IReadOnlyApplicationControlDataInterface::new(SystemRef::null());
        assert!(
            service
                .handlers()
                .get(&commands::GET_APPLICATION_CONTROL_DATA)
                .and_then(|fi| fi.handler_callback)
                .is_some()
        );
        assert!(
            service
                .handlers()
                .get(&commands::GET_APPLICATION_DESIRED_LANGUAGE)
                .and_then(|fi| fi.handler_callback)
                .is_some()
        );
        assert!(
            service
                .handlers()
                .get(&commands::CONVERT_APPLICATION_LANGUAGE_TO_LANGUAGE_CODE)
                .and_then(|fi| fi.handler_callback)
                .is_some()
        );
    }
}
