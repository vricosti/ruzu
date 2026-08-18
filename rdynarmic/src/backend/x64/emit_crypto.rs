use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::fp_helpers;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::ir::inst::Inst;
use crate::ir::value::InstRef;

// ---------------------------------------------------------------------------
// AES-NI native instructions
// All AES opcodes: (data: U128) → U128 (except MixColumns which takes one arg)
// ---------------------------------------------------------------------------

/// AESEncryptSingleRound: one round of AES encryption.
/// ARM AESE is SubBytes + ShiftRows, then caller XORs the round key.
/// x86 AESENC does ShiftRows + SubBytes + MixColumns + XOR key.
/// For a "single round" without key: use AESENCLAST with zero key (SubBytes + ShiftRows only).
pub fn emit_aes_encrypt_single_round(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    let zero = ra.scratch_xmm();
    ra.asm.xorps(zero, zero).unwrap();
    // AESENCLAST data, zero → SubBytes + ShiftRows + XOR(zero) = SubBytes + ShiftRows
    ra.asm.aesenclast(data, zero).unwrap();
    ra.release(zero);
    ra.define_value(inst_ref, data);
}

/// AESDecryptSingleRound: one round of AES decryption.
/// ARM AESD is InvSubBytes + InvShiftRows.
/// x86 AESDECLAST with zero key = InvShiftRows + InvSubBytes + XOR(zero).
pub fn emit_aes_decrypt_single_round(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    let zero = ra.scratch_xmm();
    ra.asm.xorps(zero, zero).unwrap();
    ra.asm.aesdeclast(data, zero).unwrap();
    ra.release(zero);
    ra.define_value(inst_ref, data);
}

/// AESInverseMixColumns: InvMixColumns transformation.
pub fn emit_aes_inverse_mix_columns(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    ra.asm.aesimc(data, data).unwrap();
    ra.define_value(inst_ref, data);
}

/// AESMixColumns: MixColumns transformation.
/// Implemented as AESENC with zero round key: ShiftRows + SubBytes + MixColumns + XOR(0).
/// But ARM AESMC is just MixColumns, not including ShiftRows/SubBytes.
/// Correct implementation: AESIMC(AESIMC(x)) or use AESENC approach.
/// dynarmic uses: AESDECLAST(data, zero) then AESENC(data, zero) which gives MixColumns.
pub fn emit_aes_mix_columns(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    let zero = ra.scratch_xmm();
    ra.asm.xorps(zero, zero).unwrap();
    // InvShiftRows + InvSubBytes first
    ra.asm.aesdeclast(data, zero).unwrap();
    // Then full encrypt round (ShiftRows + SubBytes + MixColumns + XOR 0)
    ra.asm.aesenc(data, zero).unwrap();
    ra.release(zero);
    ra.define_value(inst_ref, data);
}

// ---------------------------------------------------------------------------
// SHA-256 native instructions
// ---------------------------------------------------------------------------

/// SHA256Hash: two rounds of SHA-256.
/// Args: (state0: U128, state1: U128, msg: U128, part: U1)
pub fn emit_sha256_hash(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let state0 = ra.use_scratch_xmm(&mut args[0]);
    let state1 = ra.use_xmm(&mut args[1]);
    let msg = ra.use_xmm(&mut args[2]);
    // part (args[3]) selects which rounds — for x86, sha256rnds2 uses XMM0 implicitly

    // SHA256RNDS2 expects the message schedule words in XMM0
    let xmm0 = ra.scratch_xmm_at(crate::backend::x64::hostloc::HostLoc::Xmm(0));
    ra.asm.movaps(xmm0, msg).unwrap();
    ra.asm.sha256rnds2(state0, state1).unwrap();
    ra.release(xmm0);
    ra.define_value(inst_ref, state0);
}

/// SHA256MessageSchedule0: first part of message expansion.
/// Args: (w0_3: U128, w4_7: U128) → U128
pub fn emit_sha256_message_schedule_0(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let op2 = ra.use_xmm(&mut args[1]);
    ra.asm.sha256msg1(result, op2).unwrap();
    ra.define_value(inst_ref, result);
}

/// SHA256MessageSchedule1: second part of message expansion.
/// Args: (w0_3: U128, w8_11: U128, w12_15: U128) → U128
pub fn emit_sha256_message_schedule_1(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let _w8_11 = ra.use_xmm(&mut args[1]);
    let w12_15 = ra.use_xmm(&mut args[2]);
    // SHA256MSG2 takes the partially computed schedule and w12_15
    ra.asm.sha256msg2(result, w12_15).unwrap();
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// SM4 — software fallback (not universally available on x86)
// ---------------------------------------------------------------------------

/// SM4AccessSubstitutionBox: apply SM4 S-box to each byte.
/// Args: (input: U128, _: U128) → U128
pub fn emit_sm4_access_substitution_box(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let (first, rest) = args.split_at_mut(1);
    ra.host_call(
        Some(inst_ref),
        &mut [Some(&mut first[0]), Some(&mut rest[0]), None, None],
    );
    let func = fp_helpers::sm4_access_sbox as usize;
    ra.asm.mov(rxbyak::RAX, func as i64).unwrap();
    ra.asm.call_reg(rxbyak::RAX).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_crypto_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_aes_encrypt_single_round;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_aes_decrypt_single_round;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_aes_inverse_mix_columns;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_aes_mix_columns;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_sha256_hash;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_sha256_message_schedule_0;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_sha256_message_schedule_1;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_sm4_access_substitution_box;
    }
}
