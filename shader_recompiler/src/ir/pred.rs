// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! IR Predicate register type — maps to zuyu's `frontend/ir/pred.h`.
//!
//! Defines predicate registers P0..P6 and PT (always-true).

use std::fmt;

/// Maxwell predicate register (P0-P6, PT=7 always true).
///
/// Matches upstream `Shader::IR::Pred` enum with values P0=0 through PT=7.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pred(pub u64);

impl Pred {
    pub const P0: Pred = Pred(0);
    pub const P1: Pred = Pred(1);
    pub const P2: Pred = Pred(2);
    pub const P3: Pred = Pred(3);
    pub const P4: Pred = Pred(4);
    pub const P5: Pred = Pred(5);
    pub const P6: Pred = Pred(6);
    pub const PT: Pred = Pred(7);

    pub const NUM_USER_PREDS: usize = 7;
    pub const NUM_PREDS: usize = 8;

    /// Construct from a u8 index.
    pub fn from_index(index: u8) -> Self {
        debug_assert!((index as usize) < Self::NUM_PREDS);
        Pred(index as u64)
    }

    /// Get the predicate index.
    pub fn index(self) -> usize {
        self.0 as usize
    }

    /// Whether this is the always-true predicate (PT).
    pub fn is_true(self) -> bool {
        self.0 == 7
    }
}

impl fmt::Debug for Pred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_true() {
            write!(f, "PT")
        } else {
            write!(f, "P{}", self.0)
        }
    }
}

impl fmt::Display for Pred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pt_is_true() {
        assert!(Pred::PT.is_true());
        assert!(!Pred::P0.is_true());
        assert!(!Pred::P6.is_true());
    }

    #[test]
    fn test_pred_index() {
        assert_eq!(Pred::P0.index(), 0);
        assert_eq!(Pred::P6.index(), 6);
        assert_eq!(Pred::PT.index(), 7);
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Pred::PT), "PT");
        assert_eq!(format!("{}", Pred::P3), "P3");
    }
}
