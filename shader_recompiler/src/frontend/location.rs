// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/location.h`
//!
//! Instruction location (program counter) with Maxwell alignment handling.
//! Maxwell instructions are 8 bytes each with control codes every 4th slot
//! (every 32 bytes the first 8 bytes are a control code).

use std::fmt;

/// Instruction location in Maxwell shader binary.
///
/// Handles the Maxwell alignment scheme where every 32-byte group
/// starts with an 8-byte control code, so instructions are at offsets
/// 8, 16, 24 within each group.
/// DIVERGENCE FROM UPSTREAM (documented): upstream `Location` assumes the
/// sched-word grid is anchored at absolute offset 0 (`offset % 32 == 0`).
/// Empirically (MK8D dumps, 2026-07-09) the grid is anchored at the START OF
/// THE SHADER CODE (SPH base + 0x50): the first code word is always a sched
/// word, then every 4th word. Games whose code starts 32-byte aligned match
/// upstream's absolute grid, but MK8D also ships shaders whose code starts at
/// `offset % 32 == 16`; the absolute grid then skips real instructions and
/// decodes sched words. `phase` anchors the grid to the code start
/// (`phase = code_start % 32`); phase 0 is exactly upstream behaviour.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    offset: u32,
    phase: u32,
}

const VIRTUAL_BIAS: u32 = 4;

impl Location {
    /// Create a new location from a byte offset.
    ///
    /// The offset must be a multiple of 8.
    pub fn new(initial_offset: u32) -> Self {
        Self::new_with_phase(initial_offset, 0)
    }

    /// Create the location of the first instruction of a shader whose code
    /// begins at byte offset `code_start`: the sched grid is anchored there.
    pub fn new_code_start(code_start: u32) -> Self {
        Self::new_with_phase(code_start, code_start % 32)
    }

    fn new_with_phase(initial_offset: u32, phase: u32) -> Self {
        assert!(
            initial_offset % 8 == 0,
            "initial_offset={} is not a multiple of 8",
            initial_offset
        );
        let mut loc = Self {
            offset: initial_offset,
            phase,
        };
        loc.align();
        loc
    }

    /// A location at `offset` on the same sched grid as `self` (used for
    /// branch targets, which live in the same shader).
    pub fn with_offset(&self, offset: u32) -> Self {
        Self::new_with_phase(offset, self.phase)
    }

    /// Create a virtual location (offset by VIRTUAL_BIAS).
    pub fn virtual_loc(&self) -> Self {
        Self {
            offset: self.offset - VIRTUAL_BIAS,
            phase: self.phase,
        }
    }

    /// Get the byte offset.
    pub fn offset(&self) -> u32 {
        self.offset
    }

    /// Check if this is a virtual location.
    pub fn is_virtual(&self) -> bool {
        self.offset % 8 == VIRTUAL_BIAS
    }

    /// Advance to the next instruction.
    pub fn step(&mut self) {
        self.offset += 8 + if self.grid_offset() % 32 == 24 { 8 } else { 0 };
    }

    /// Return the next instruction location.
    pub fn next(mut self) -> Self {
        self.step();
        self
    }

    /// Go back to the previous instruction.
    pub fn back(&mut self) {
        self.offset -= 8 + if self.grid_offset() % 32 == 8 { 8 } else { 0 };
    }

    /// Return the previous instruction location.
    pub fn prev(mut self) -> Self {
        self.back();
        self
    }

    /// Port of upstream `Location::operator+(int)`.
    pub fn add_instructions(mut self, mut count: i32) -> Self {
        while count > 0 {
            count -= 1;
            self.step();
        }
        while count < 0 {
            count += 1;
            self.back();
        }
        self
    }

    fn align(&mut self) {
        self.offset += if self.grid_offset() % 32 == 0 { 8 } else { 0 };
    }

    /// Offset in the sched-grid space (grid anchored at `phase`).
    fn grid_offset(&self) -> u32 {
        self.offset.wrapping_sub(self.phase)
    }
}

impl Default for Location {
    fn default() -> Self {
        Self {
            offset: 0xcccccccc,
            phase: 0,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:04x}", self.offset)
    }
}
