// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `frontend/maxwell/indirect_branch_table_track.cpp`
//!
//! Tracks indirect branch tables by analyzing the instruction sequence
//! that loads a branch target from a table. Used by the CFG builder to
//! resolve indirect branches (e.g., switch statements).

/// Result of tracking an indirect branch table.
#[derive(Debug, Clone)]
pub struct IndirectBranchTableInfo {
    /// Constant buffer index containing the branch table.
    pub cbuf_index: u32,
    /// Byte offset of the table start in the constant buffer.
    pub cbuf_offset: u32,
    /// Number of entries in the branch table.
    pub num_entries: u32,
    /// Branch targets (byte offsets from shader start).
    pub targets: Vec<u32>,
}

/// Attempt to track an indirect branch table from the instruction stream.
///
/// Analyzes backward from the BRX instruction to find the pattern:
///   LDC Rx, c[cbuf][offset + Ry * 4]
///   BRX Rx
///
/// Not yet implemented: requires backward analysis of the instruction stream
/// and LDC pattern recognition.
///
/// Returns `None` if the pattern is not recognized (or not yet implemented).
pub fn track_indirect_branch_table(
    _read_insn: &dyn Fn(u32) -> u64,
    _brx_address: u32,
) -> Option<IndirectBranchTableInfo> {
    log::warn!("TrackIndirectBranchTable not yet implemented — returning None");
    None
}
