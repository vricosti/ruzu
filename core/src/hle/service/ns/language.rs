// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/language.h and language.cpp
//!
//! nn::ns::detail::ApplicationLanguage and conversion utilities.

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

/// LanguageCode values matching upstream Set::LanguageCode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
#[allow(non_camel_case_types)]
pub enum LanguageCode {
    JA = 0x000000000000616A,
    EN_US = 0x00000053552D6E65,
    FR = 0x0000000000007266,
    DE = 0x0000000000006564,
    IT = 0x0000000000007469,
    ES = 0x0000000000007365,
    ZH_CN = 0x0000004E432D687A,
    KO = 0x00000000000006F6B,
    NL = 0x0000000000006C6E,
    PT = 0x0000000000007470,
    RU = 0x0000000000007572,
    ZH_TW = 0x00000057542D687A,
    EN_GB = 0x00000042472D6E65,
    FR_CA = 0x00000041432D7266,
    ES_419 = 0x0039_3134_2D73_65,
    ZH_HANS = 0x00736E61482D687A,
    ZH_HANT = 0x00746E61482D687A,
    PT_BR = 0x00000052422D7470,
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
        LanguageCode::EN_US => Some(ApplicationLanguage::AmericanEnglish),
        LanguageCode::EN_GB => Some(ApplicationLanguage::BritishEnglish),
        LanguageCode::JA => Some(ApplicationLanguage::Japanese),
        LanguageCode::FR => Some(ApplicationLanguage::French),
        LanguageCode::DE => Some(ApplicationLanguage::German),
        LanguageCode::ES_419 => Some(ApplicationLanguage::LatinAmericanSpanish),
        LanguageCode::ES => Some(ApplicationLanguage::Spanish),
        LanguageCode::IT => Some(ApplicationLanguage::Italian),
        LanguageCode::NL => Some(ApplicationLanguage::Dutch),
        LanguageCode::FR_CA => Some(ApplicationLanguage::CanadianFrench),
        LanguageCode::PT => Some(ApplicationLanguage::Portuguese),
        LanguageCode::RU => Some(ApplicationLanguage::Russian),
        LanguageCode::KO => Some(ApplicationLanguage::Korean),
        LanguageCode::ZH_TW | LanguageCode::ZH_HANT => {
            Some(ApplicationLanguage::TraditionalChinese)
        }
        LanguageCode::ZH_CN | LanguageCode::ZH_HANS => Some(ApplicationLanguage::SimplifiedChinese),
        LanguageCode::PT_BR => Some(ApplicationLanguage::BrazilianPortuguese),
        _ => None,
    }
}

pub fn convert_to_language_code(lang: ApplicationLanguage) -> Option<LanguageCode> {
    match lang {
        ApplicationLanguage::AmericanEnglish => Some(LanguageCode::EN_US),
        ApplicationLanguage::BritishEnglish => Some(LanguageCode::EN_GB),
        ApplicationLanguage::Japanese => Some(LanguageCode::JA),
        ApplicationLanguage::French => Some(LanguageCode::FR),
        ApplicationLanguage::German => Some(LanguageCode::DE),
        ApplicationLanguage::LatinAmericanSpanish => Some(LanguageCode::ES_419),
        ApplicationLanguage::Spanish => Some(LanguageCode::ES),
        ApplicationLanguage::Italian => Some(LanguageCode::IT),
        ApplicationLanguage::Dutch => Some(LanguageCode::NL),
        ApplicationLanguage::CanadianFrench => Some(LanguageCode::FR_CA),
        ApplicationLanguage::Portuguese => Some(LanguageCode::PT),
        ApplicationLanguage::Russian => Some(LanguageCode::RU),
        ApplicationLanguage::Korean => Some(LanguageCode::KO),
        ApplicationLanguage::TraditionalChinese => Some(LanguageCode::ZH_HANT),
        ApplicationLanguage::SimplifiedChinese => Some(LanguageCode::ZH_HANS),
        ApplicationLanguage::BrazilianPortuguese => Some(LanguageCode::PT_BR),
        _ => None,
    }
}
