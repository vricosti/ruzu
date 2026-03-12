// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/spirv/emit_spirv_warp.cpp`
//!
//! SPIR-V emission for warp-level (subgroup) operations.

use super::spirv_context::EmitContext;

/// Emit SPIR-V for warp vote all.
pub fn emit_vote_all(_ctx: &mut EmitContext) {
    todo!("EmitVoteAll")
}

/// Emit SPIR-V for warp vote any.
pub fn emit_vote_any(_ctx: &mut EmitContext) {
    todo!("EmitVoteAny")
}

/// Emit SPIR-V for warp vote equal.
pub fn emit_vote_equal(_ctx: &mut EmitContext) {
    todo!("EmitVoteEqual")
}

/// Emit SPIR-V for warp ballot.
pub fn emit_subgroup_ballot(_ctx: &mut EmitContext) {
    todo!("EmitSubgroupBallot")
}

/// Emit SPIR-V for subgroup EQ mask.
pub fn emit_subgroup_eq_mask(_ctx: &mut EmitContext) {
    todo!("EmitSubgroupEqMask")
}

/// Emit SPIR-V for subgroup LT mask.
pub fn emit_subgroup_lt_mask(_ctx: &mut EmitContext) {
    todo!("EmitSubgroupLtMask")
}

/// Emit SPIR-V for subgroup LE mask.
pub fn emit_subgroup_le_mask(_ctx: &mut EmitContext) {
    todo!("EmitSubgroupLeMask")
}

/// Emit SPIR-V for subgroup GT mask.
pub fn emit_subgroup_gt_mask(_ctx: &mut EmitContext) {
    todo!("EmitSubgroupGtMask")
}

/// Emit SPIR-V for subgroup GE mask.
pub fn emit_subgroup_ge_mask(_ctx: &mut EmitContext) {
    todo!("EmitSubgroupGeMask")
}

/// Emit SPIR-V for shuffle index.
pub fn emit_shuffle_index(_ctx: &mut EmitContext) {
    todo!("EmitShuffleIndex")
}

/// Emit SPIR-V for shuffle up.
pub fn emit_shuffle_up(_ctx: &mut EmitContext) {
    todo!("EmitShuffleUp")
}

/// Emit SPIR-V for shuffle down.
pub fn emit_shuffle_down(_ctx: &mut EmitContext) {
    todo!("EmitShuffleDown")
}

/// Emit SPIR-V for shuffle butterfly.
pub fn emit_shuffle_butterfly(_ctx: &mut EmitContext) {
    todo!("EmitShuffleButterfly")
}

/// Emit SPIR-V for FSWZADD (float swizzled add).
pub fn emit_fswzadd(_ctx: &mut EmitContext) {
    todo!("EmitFSwizzleAdd")
}
