// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/language.h and language.cpp
//!
//! nn::ns::detail::ApplicationLanguage and conversion utilities.

use crate::hle::service::set::settings_types::LanguageCode;

/// nn::ns::detail::ApplicationLanguage
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
    Count = 16,
}

pub const APPLICATION_LANGUAGE_COUNT: usize = ApplicationLanguage::Count as usize;

pub type ApplicationLanguagePriorityList = [ApplicationLanguage; APPLICATION_LANGUAGE_COUNT];

pub const fn get_supported_language_flag(lang: ApplicationLanguage) -> u32 {
    1u32 << (lang as u32)
}

const PRIORITY_LIST_AMERICAN_ENGLISH: ApplicationLanguagePriorityList = [
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_BRITISH_ENGLISH: ApplicationLanguagePriorityList = [
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_JAPANESE: ApplicationLanguagePriorityList = [
    ApplicationLanguage::Japanese,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_FRENCH: ApplicationLanguagePriorityList = [
    ApplicationLanguage::French,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_GERMAN: ApplicationLanguagePriorityList = [
    ApplicationLanguage::German,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::French,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_LATIN_AMERICAN_SPANISH: ApplicationLanguagePriorityList = [
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::French,
    ApplicationLanguage::Italian,
    ApplicationLanguage::German,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Russian,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_SPANISH: ApplicationLanguagePriorityList = [
    ApplicationLanguage::Spanish,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_ITALIAN: ApplicationLanguagePriorityList = [
    ApplicationLanguage::Italian,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_DUTCH: ApplicationLanguagePriorityList = [
    ApplicationLanguage::Dutch,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_CANADIAN_FRENCH: ApplicationLanguagePriorityList = [
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::French,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::German,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_PORTUGUESE: ApplicationLanguagePriorityList = [
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Russian,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_RUSSIAN: ApplicationLanguagePriorityList = [
    ApplicationLanguage::Russian,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_KOREAN: ApplicationLanguagePriorityList = [
    ApplicationLanguage::Korean,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_TRADITIONAL_CHINESE: ApplicationLanguagePriorityList = [
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_SIMPLIFIED_CHINESE: ApplicationLanguagePriorityList = [
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::Russian,
    ApplicationLanguage::Korean,
    ApplicationLanguage::BrazilianPortuguese,
];

const PRIORITY_LIST_BRAZILIAN_PORTUGUESE: ApplicationLanguagePriorityList = [
    ApplicationLanguage::BrazilianPortuguese,
    ApplicationLanguage::Portuguese,
    ApplicationLanguage::LatinAmericanSpanish,
    ApplicationLanguage::AmericanEnglish,
    ApplicationLanguage::BritishEnglish,
    ApplicationLanguage::Japanese,
    ApplicationLanguage::French,
    ApplicationLanguage::German,
    ApplicationLanguage::Spanish,
    ApplicationLanguage::Italian,
    ApplicationLanguage::Dutch,
    ApplicationLanguage::CanadianFrench,
    ApplicationLanguage::Russian,
    ApplicationLanguage::Korean,
    ApplicationLanguage::SimplifiedChinese,
    ApplicationLanguage::TraditionalChinese,
];

pub fn get_application_language_priority_list(
    lang: ApplicationLanguage,
) -> Option<&'static ApplicationLanguagePriorityList> {
    match lang {
        ApplicationLanguage::AmericanEnglish => Some(&PRIORITY_LIST_AMERICAN_ENGLISH),
        ApplicationLanguage::BritishEnglish => Some(&PRIORITY_LIST_BRITISH_ENGLISH),
        ApplicationLanguage::Japanese => Some(&PRIORITY_LIST_JAPANESE),
        ApplicationLanguage::French => Some(&PRIORITY_LIST_FRENCH),
        ApplicationLanguage::German => Some(&PRIORITY_LIST_GERMAN),
        ApplicationLanguage::LatinAmericanSpanish => Some(&PRIORITY_LIST_LATIN_AMERICAN_SPANISH),
        ApplicationLanguage::Spanish => Some(&PRIORITY_LIST_SPANISH),
        ApplicationLanguage::Italian => Some(&PRIORITY_LIST_ITALIAN),
        ApplicationLanguage::Dutch => Some(&PRIORITY_LIST_DUTCH),
        ApplicationLanguage::CanadianFrench => Some(&PRIORITY_LIST_CANADIAN_FRENCH),
        ApplicationLanguage::Portuguese => Some(&PRIORITY_LIST_PORTUGUESE),
        ApplicationLanguage::Russian => Some(&PRIORITY_LIST_RUSSIAN),
        ApplicationLanguage::Korean => Some(&PRIORITY_LIST_KOREAN),
        ApplicationLanguage::TraditionalChinese => Some(&PRIORITY_LIST_TRADITIONAL_CHINESE),
        ApplicationLanguage::SimplifiedChinese => Some(&PRIORITY_LIST_SIMPLIFIED_CHINESE),
        ApplicationLanguage::BrazilianPortuguese => Some(&PRIORITY_LIST_BRAZILIAN_PORTUGUESE),
        _ => None,
    }
}

pub fn convert_to_application_language(language_code: LanguageCode) -> Option<ApplicationLanguage> {
    match language_code {
        LanguageCode::EnUs => Some(ApplicationLanguage::AmericanEnglish),
        LanguageCode::EnGb => Some(ApplicationLanguage::BritishEnglish),
        LanguageCode::Ja => Some(ApplicationLanguage::Japanese),
        LanguageCode::Fr => Some(ApplicationLanguage::French),
        LanguageCode::De => Some(ApplicationLanguage::German),
        LanguageCode::Es419 => Some(ApplicationLanguage::LatinAmericanSpanish),
        LanguageCode::Es => Some(ApplicationLanguage::Spanish),
        LanguageCode::It => Some(ApplicationLanguage::Italian),
        LanguageCode::Nl => Some(ApplicationLanguage::Dutch),
        LanguageCode::FrCa => Some(ApplicationLanguage::CanadianFrench),
        LanguageCode::Pt => Some(ApplicationLanguage::Portuguese),
        LanguageCode::Ru => Some(ApplicationLanguage::Russian),
        LanguageCode::Ko => Some(ApplicationLanguage::Korean),
        LanguageCode::ZhTw | LanguageCode::ZhHant => {
            Some(ApplicationLanguage::TraditionalChinese)
        }
        LanguageCode::ZhCn | LanguageCode::ZhHans => Some(ApplicationLanguage::SimplifiedChinese),
        LanguageCode::PtBr => Some(ApplicationLanguage::BrazilianPortuguese),
        _ => None,
    }
}

pub fn convert_to_language_code(lang: ApplicationLanguage) -> Option<LanguageCode> {
    match lang {
        ApplicationLanguage::AmericanEnglish => Some(LanguageCode::EnUs),
        ApplicationLanguage::BritishEnglish => Some(LanguageCode::EnGb),
        ApplicationLanguage::Japanese => Some(LanguageCode::Ja),
        ApplicationLanguage::French => Some(LanguageCode::Fr),
        ApplicationLanguage::German => Some(LanguageCode::De),
        ApplicationLanguage::LatinAmericanSpanish => Some(LanguageCode::Es419),
        ApplicationLanguage::Spanish => Some(LanguageCode::Es),
        ApplicationLanguage::Italian => Some(LanguageCode::It),
        ApplicationLanguage::Dutch => Some(LanguageCode::Nl),
        ApplicationLanguage::CanadianFrench => Some(LanguageCode::FrCa),
        ApplicationLanguage::Portuguese => Some(LanguageCode::Pt),
        ApplicationLanguage::Russian => Some(LanguageCode::Ru),
        ApplicationLanguage::Korean => Some(LanguageCode::Ko),
        ApplicationLanguage::TraditionalChinese => Some(LanguageCode::ZhHant),
        ApplicationLanguage::SimplifiedChinese => Some(LanguageCode::ZhHans),
        ApplicationLanguage::BrazilianPortuguese => Some(LanguageCode::PtBr),
        _ => None,
    }
}
