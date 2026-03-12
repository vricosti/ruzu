// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/ir/condition.h` and `frontend/ir/condition.cpp`
//!
//! Condition combining a flow test and a predicate, used for
//! conditional execution in Maxwell control flow.

use std::fmt;

use super::flow_test::FlowTest;

/// Number of user predicates (P0-P6).
pub const NUM_USER_PREDS: usize = 7;
/// Total number of predicates (P0-P6 + PT).
pub const NUM_PREDS: usize = 8;

/// Predicate register index (P0-P6, PT=7).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum IrPred {
    P0 = 0,
    P1 = 1,
    P2 = 2,
    P3 = 3,
    P4 = 4,
    P5 = 5,
    P6 = 6,
    PT = 7,
}

impl IrPred {
    pub fn from_u8(val: u8) -> Self {
        match val & 0x7 {
            0 => IrPred::P0,
            1 => IrPred::P1,
            2 => IrPred::P2,
            3 => IrPred::P3,
            4 => IrPred::P4,
            5 => IrPred::P5,
            6 => IrPred::P6,
            7 => IrPred::PT,
            _ => unreachable!(),
        }
    }

    pub fn index(self) -> usize {
        self as usize
    }
}

impl fmt::Display for IrPred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrPred::PT => write!(f, "PT"),
            _ => write!(f, "P{}", *self as u8),
        }
    }
}

/// Condition combining a flow test and a predicate.
///
/// Matches upstream `IR::Condition` packed representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Condition {
    flow_test: u16,
    pred: u8,
    pred_negated: u8,
}

impl Condition {
    pub fn new(flow_test: FlowTest, pred: IrPred, pred_negated: bool) -> Self {
        Self {
            flow_test: flow_test as u16,
            pred: pred as u8,
            pred_negated: if pred_negated { 1 } else { 0 },
        }
    }

    pub fn from_pred(pred: IrPred, pred_negated: bool) -> Self {
        Self::new(FlowTest::T, pred, pred_negated)
    }

    pub fn from_bool(value: bool) -> Self {
        Self::from_pred(IrPred::PT, !value)
    }

    pub fn get_flow_test(&self) -> FlowTest {
        FlowTest::from_u64(self.flow_test as u64).unwrap_or(FlowTest::T)
    }

    pub fn get_pred(&self) -> (IrPred, bool) {
        (IrPred::from_u8(self.pred), self.pred_negated != 0)
    }
}

impl Default for Condition {
    fn default() -> Self {
        Self {
            flow_test: 0,
            pred: 0,
            pred_negated: 0,
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let flow_test = self.get_flow_test();
        let (pred, negated) = self.get_pred();
        let mut result = String::new();
        if flow_test != FlowTest::T {
            result = flow_test.to_string();
        }
        if !result.is_empty() {
            result.push('&');
        }
        if negated {
            result.push('!');
        }
        result.push_str(&pred.to_string());
        write!(f, "{}", result)
    }
}
