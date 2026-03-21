// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/read_only_application_control_data_interface.h
//! Port of zuyu/src/core/hle/service/ns/read_only_application_control_data_interface.cpp
//!
//! IReadOnlyApplicationControlDataInterface — reads NACP and icon data.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
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

/// Application language (used in GetApplicationDesiredLanguage).
///
/// Corresponds to `ApplicationLanguage` in upstream ns/language.h.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ApplicationLanguage {
    AmericanEnglish = 0,
    BritishEnglish = 1,
    Japanese = 2,
    French = 3,
    German = 4,
    LatinAmericanSpanish = 5,
    Spanish = 6,
    Italian = 7,
    Dutch = 8,
    CanadianFrench = 9,
    Portuguese = 10,
    Russian = 11,
    Korean = 12,
    TraditionalChinese = 13,
    SimplifiedChinese = 14,
    BrazilianPortuguese = 15,
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
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IReadOnlyApplicationControlDataInterface {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::GET_APPLICATION_CONTROL_DATA, None, "GetApplicationControlData"),
            (commands::GET_APPLICATION_DESIRED_LANGUAGE, None, "GetApplicationDesiredLanguage"),
            (commands::CONVERT_APPLICATION_LANGUAGE_TO_LANGUAGE_CODE, None, "ConvertApplicationLanguageToLanguageCode"),
            (commands::CONVERT_LANGUAGE_CODE_TO_APPLICATION_LANGUAGE, None, "ConvertLanguageCodeToApplicationLanguage"),
            (commands::SELECT_APPLICATION_DESIRED_LANGUAGE, None, "SelectApplicationDesiredLanguage"),
        ]);
        Self {
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

        // TODO: Read NACP + icon from PatchManager.
        // For now, zero-fill the NACP area (0x4000 bytes) as a stub.
        const NACP_SIZE: usize = 0x4000;
        if out_buffer.len() < NACP_SIZE {
            log::error!(
                "output buffer is too small! (actual={:#x}, expected_min={:#x})",
                out_buffer.len(),
                NACP_SIZE
            );
            return Err(ResultCode::new(1)); // ResultUnknown
        }

        // Zero-fill NACP.
        out_buffer[..NACP_SIZE].fill(0);

        Ok(NACP_SIZE as u32)
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

        // Default to AmericanEnglish.
        // TODO: Properly implement language priority list matching upstream.
        if supported_languages == 0 || (supported_languages & (1 << 0)) != 0 {
            return Ok(ApplicationLanguage::AmericanEnglish);
        }

        // Find first supported language.
        for i in 0..16u32 {
            if (supported_languages & (1 << i)) != 0 {
                // Safety: i is 0..15, all valid ApplicationLanguage variants.
                return Ok(unsafe { std::mem::transmute(i as u8) });
            }
        }

        log::error!(
            "Could not find a valid language! supported_languages={:#010x}",
            supported_languages
        );
        Err(ResultCode::new(1)) // ResultApplicationLanguageNotFound
    }

    /// ConvertApplicationLanguageToLanguageCode (cmd 2).
    ///
    /// Corresponds to upstream `ConvertApplicationLanguageToLanguageCode`.
    pub fn convert_application_language_to_language_code(
        &self,
        application_language: ApplicationLanguage,
    ) -> Result<u64, ResultCode> {
        use crate::hle::service::set::settings_types::LanguageCode;

        let language_code = match application_language {
            ApplicationLanguage::AmericanEnglish => LanguageCode::EnUs,
            ApplicationLanguage::BritishEnglish => LanguageCode::EnGb,
            ApplicationLanguage::Japanese => LanguageCode::Ja,
            ApplicationLanguage::French => LanguageCode::Fr,
            ApplicationLanguage::German => LanguageCode::De,
            ApplicationLanguage::LatinAmericanSpanish => LanguageCode::Es419,
            ApplicationLanguage::Spanish => LanguageCode::Es,
            ApplicationLanguage::Italian => LanguageCode::It,
            ApplicationLanguage::Dutch => LanguageCode::Nl,
            ApplicationLanguage::CanadianFrench => LanguageCode::FrCa,
            ApplicationLanguage::Portuguese => LanguageCode::Pt,
            ApplicationLanguage::Russian => LanguageCode::Ru,
            ApplicationLanguage::Korean => LanguageCode::Ko,
            ApplicationLanguage::TraditionalChinese => LanguageCode::ZhHant,
            ApplicationLanguage::SimplifiedChinese => LanguageCode::ZhHans,
            ApplicationLanguage::BrazilianPortuguese => LanguageCode::PtBr,
        };

        Ok(language_code as u64)
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
