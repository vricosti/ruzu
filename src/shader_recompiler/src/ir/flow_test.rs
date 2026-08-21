// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/ir/flow_test.h` and `frontend/ir/flow_test.cpp`
//!
//! Flow test conditions used in Maxwell branch instructions.

use std::fmt;

/// Flow test conditions for Maxwell branch instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u64)]
pub enum FlowTest {
    F = 0,
    LT = 1,
    EQ = 2,
    LE = 3,
    GT = 4,
    NE = 5,
    GE = 6,
    NUM = 7,
    NaN = 8,
    LTU = 9,
    EQU = 10,
    LEU = 11,
    GTU = 12,
    NEU = 13,
    GEU = 14,
    T = 15,
    OFF = 16,
    LO = 17,
    SFF = 18,
    LS = 19,
    HI = 20,
    SFT = 21,
    HS = 22,
    OFT = 23,
    CSM_TA = 24,
    CSM_TR = 25,
    CSM_MX = 26,
    FCSM_TA = 27,
    FCSM_TR = 28,
    FCSM_MX = 29,
    RLE = 30,
    RGT = 31,
}

impl FlowTest {
    pub fn from_u64(val: u64) -> Option<Self> {
        match val {
            0 => Some(FlowTest::F),
            1 => Some(FlowTest::LT),
            2 => Some(FlowTest::EQ),
            3 => Some(FlowTest::LE),
            4 => Some(FlowTest::GT),
            5 => Some(FlowTest::NE),
            6 => Some(FlowTest::GE),
            7 => Some(FlowTest::NUM),
            8 => Some(FlowTest::NaN),
            9 => Some(FlowTest::LTU),
            10 => Some(FlowTest::EQU),
            11 => Some(FlowTest::LEU),
            12 => Some(FlowTest::GTU),
            13 => Some(FlowTest::NEU),
            14 => Some(FlowTest::GEU),
            15 => Some(FlowTest::T),
            16 => Some(FlowTest::OFF),
            17 => Some(FlowTest::LO),
            18 => Some(FlowTest::SFF),
            19 => Some(FlowTest::LS),
            20 => Some(FlowTest::HI),
            21 => Some(FlowTest::SFT),
            22 => Some(FlowTest::HS),
            23 => Some(FlowTest::OFT),
            24 => Some(FlowTest::CSM_TA),
            25 => Some(FlowTest::CSM_TR),
            26 => Some(FlowTest::CSM_MX),
            27 => Some(FlowTest::FCSM_TA),
            28 => Some(FlowTest::FCSM_TR),
            29 => Some(FlowTest::FCSM_MX),
            30 => Some(FlowTest::RLE),
            31 => Some(FlowTest::RGT),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            FlowTest::F => "F",
            FlowTest::LT => "LT",
            FlowTest::EQ => "EQ",
            FlowTest::LE => "LE",
            FlowTest::GT => "GT",
            FlowTest::NE => "NE",
            FlowTest::GE => "GE",
            FlowTest::NUM => "NUM",
            FlowTest::NaN => "NAN",
            FlowTest::LTU => "LTU",
            FlowTest::EQU => "EQU",
            FlowTest::LEU => "LEU",
            FlowTest::GTU => "GTU",
            FlowTest::NEU => "NEU",
            FlowTest::GEU => "GEU",
            FlowTest::T => "T",
            FlowTest::OFF => "OFF",
            FlowTest::LO => "LO",
            FlowTest::SFF => "SFF",
            FlowTest::LS => "LS",
            FlowTest::HI => "HI",
            FlowTest::SFT => "SFT",
            FlowTest::HS => "HS",
            FlowTest::OFT => "OFT",
            FlowTest::CSM_TA => "CSM_TA",
            FlowTest::CSM_TR => "CSM_TR",
            FlowTest::CSM_MX => "CSM_MX",
            FlowTest::FCSM_TA => "FCSM_TA",
            FlowTest::FCSM_TR => "FCSM_TR",
            FlowTest::FCSM_MX => "FCSM_MX",
            FlowTest::RLE => "RLE",
            FlowTest::RGT => "RGT",
        }
    }
}

impl fmt::Display for FlowTest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}
