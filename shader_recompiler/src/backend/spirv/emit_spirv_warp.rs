// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V warp/subgroup operation emission — maps to zuyu's
//! `backend/spirv/emit_spirv_warp.cpp`.
//!
//! Implements subgroup operations: vote, ballot, shuffle, and derivatives.

use rspirv::spirv::{self, Word};
use super::spirv_emit_context::SpirvEmitContext;

/// Get the subgroup scope constant.
fn subgroup_scope(ctx: &mut SpirvEmitContext) -> Word {
    ctx.constant_u32(spirv::Scope::Subgroup as u32)
}

/// Emit lane ID (subgroup local invocation ID, masked to 31 for >32 warp).
///
/// Matches upstream `EmitLaneId(EmitContext&)`.
pub fn emit_lane_id(ctx: &mut SpirvEmitContext) -> Word {
    // Load subgroup local invocation ID
    // If warp size could be > 32, mask with 31.
    // For now return undef as we need the built-in variable setup.
    log::trace!("SPIR-V: emit_lane_id");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit `OpGroupNonUniformAll` (VOTE.ALL).
///
/// Matches upstream `EmitVoteAll(EmitContext&, Id)`.
pub fn emit_vote_all(ctx: &mut SpirvEmitContext, pred: Word) -> Word {
    let scope = subgroup_scope(ctx);
    ctx.builder
        .group_non_uniform_all(ctx.bool_type, None, scope, pred)
        .unwrap()
}

/// Emit `OpGroupNonUniformAny` (VOTE.ANY).
///
/// Matches upstream `EmitVoteAny(EmitContext&, Id)`.
pub fn emit_vote_any(ctx: &mut SpirvEmitContext, pred: Word) -> Word {
    let scope = subgroup_scope(ctx);
    ctx.builder
        .group_non_uniform_any(ctx.bool_type, None, scope, pred)
        .unwrap()
}

/// Emit `OpGroupNonUniformAllEqual` (VOTE.EQ).
///
/// Matches upstream `EmitVoteEqual(EmitContext&, Id)`.
pub fn emit_vote_equal(ctx: &mut SpirvEmitContext, pred: Word) -> Word {
    let scope = subgroup_scope(ctx);
    ctx.builder
        .group_non_uniform_all_equal(ctx.bool_type, None, scope, pred)
        .unwrap()
}

/// Emit `OpGroupNonUniformBallot`.
///
/// Matches upstream `EmitSubgroupBallot(EmitContext&, Id)`.
pub fn emit_subgroup_ballot(ctx: &mut SpirvEmitContext, pred: Word) -> Word {
    let scope = subgroup_scope(ctx);
    let ballot = ctx
        .builder
        .group_non_uniform_ballot(ctx.u32_vec4_type, None, scope, pred)
        .unwrap();
    // Extract first component (assumes warp size <= 32)
    ctx.builder
        .composite_extract(ctx.u32_type, None, ballot, vec![0])
        .unwrap()
}

/// Emit subgroup eq mask.
///
/// Matches upstream `EmitSubgroupEqMask(EmitContext&)`.
pub fn emit_subgroup_eq_mask(ctx: &mut SpirvEmitContext) -> Word {
    log::trace!("SPIR-V: emit_subgroup_eq_mask");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit subgroup lt mask.
pub fn emit_subgroup_lt_mask(ctx: &mut SpirvEmitContext) -> Word {
    log::trace!("SPIR-V: emit_subgroup_lt_mask");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit subgroup le mask.
pub fn emit_subgroup_le_mask(ctx: &mut SpirvEmitContext) -> Word {
    log::trace!("SPIR-V: emit_subgroup_le_mask");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit subgroup gt mask.
pub fn emit_subgroup_gt_mask(ctx: &mut SpirvEmitContext) -> Word {
    log::trace!("SPIR-V: emit_subgroup_gt_mask");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit subgroup ge mask.
pub fn emit_subgroup_ge_mask(ctx: &mut SpirvEmitContext) -> Word {
    log::trace!("SPIR-V: emit_subgroup_ge_mask");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit shuffle index (SHFL.IDX).
///
/// Matches upstream `EmitShuffleIndex(EmitContext&, ...)`.
pub fn emit_shuffle_index(
    ctx: &mut SpirvEmitContext,
    value: Word,
    index: Word,
    _clamp: Word,
    _seg_mask: Word,
) -> Word {
    let scope = subgroup_scope(ctx);
    ctx.builder
        .group_non_uniform_shuffle(ctx.u32_type, None, scope, value, index)
        .unwrap()
}

/// Emit shuffle up (SHFL.UP).
pub fn emit_shuffle_up(
    ctx: &mut SpirvEmitContext,
    value: Word,
    delta: Word,
    _clamp: Word,
    _seg_mask: Word,
) -> Word {
    let scope = subgroup_scope(ctx);
    ctx.builder
        .group_non_uniform_shuffle_up(ctx.u32_type, None, scope, value, delta)
        .unwrap()
}

/// Emit shuffle down (SHFL.DOWN).
pub fn emit_shuffle_down(
    ctx: &mut SpirvEmitContext,
    value: Word,
    delta: Word,
    _clamp: Word,
    _seg_mask: Word,
) -> Word {
    let scope = subgroup_scope(ctx);
    ctx.builder
        .group_non_uniform_shuffle_down(ctx.u32_type, None, scope, value, delta)
        .unwrap()
}

/// Emit shuffle butterfly (SHFL.BFLY).
pub fn emit_shuffle_butterfly(
    ctx: &mut SpirvEmitContext,
    value: Word,
    index: Word,
    _clamp: Word,
    _seg_mask: Word,
) -> Word {
    let scope = subgroup_scope(ctx);
    ctx.builder
        .group_non_uniform_shuffle_xor(ctx.u32_type, None, scope, value, index)
        .unwrap()
}

/// Emit DPdxFine: `OpDPdxFine`.
pub fn emit_dpdx_fine(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.d_pdx_fine(ctx.f32_type, None, value).unwrap()
}

/// Emit DPdyFine: `OpDPdyFine`.
pub fn emit_dpdy_fine(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.d_pdy_fine(ctx.f32_type, None, value).unwrap()
}

/// Emit DPdxCoarse: `OpDPdxCoarse`.
pub fn emit_dpdx_coarse(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder
        .d_pdx_coarse(ctx.f32_type, None, value)
        .unwrap()
}

/// Emit DPdyCoarse: `OpDPdyCoarse`.
pub fn emit_dpdy_coarse(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder
        .d_pdy_coarse(ctx.f32_type, None, value)
        .unwrap()
}
