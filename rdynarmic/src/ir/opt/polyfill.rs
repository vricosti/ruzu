use crate::ir::block::Block;
use crate::ir::emitter::IREmitter;
use crate::ir::opcode::Opcode;
use crate::ir::value::Value;

/// Host-dependent IR legalization options.
///
/// Mirrors upstream `Optimization::PolyfillOptions`. The widening multiply
/// member is retained here even though that upstream slice is not yet ported,
/// so the owner and configuration shape do not drift again.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PolyfillOptions {
    pub sha256: bool,
    pub vector_multiply_widen: bool,
}

fn remap_value(value: Value, replacements: &[Value]) -> Value {
    match value {
        Value::Inst(reference) => replacements[reference.index()],
        immediate => immediate,
    }
}

fn sha_choose(ir: &mut IREmitter<'_>, x: Value, y: Value, z: Value) -> Value {
    let yz = ir.eor_32(y, z);
    let selected = ir.and_32(yz, x);
    ir.eor_32(selected, z)
}

fn sha_majority(ir: &mut IREmitter<'_>, x: Value, y: Value, z: Value) -> Value {
    let xy = ir.and_32(x, y);
    let x_or_y = ir.or_32(x, y);
    let remaining = ir.and_32(x_or_y, z);
    ir.or_32(xy, remaining)
}

fn sha_hash_sigma0(ir: &mut IREmitter<'_>, x: Value) -> Value {
    let carry = Value::ImmU1(false);
    let tmp1 = ir.rotate_right_32(x, Value::ImmU8(2), carry);
    let tmp2 = ir.rotate_right_32(x, Value::ImmU8(13), carry);
    let tmp3 = ir.rotate_right_32(x, Value::ImmU8(22), carry);
    let tail = ir.eor_32(tmp2, tmp3);
    ir.eor_32(tmp1, tail)
}

fn sha_hash_sigma1(ir: &mut IREmitter<'_>, x: Value) -> Value {
    let carry = Value::ImmU1(false);
    let tmp1 = ir.rotate_right_32(x, Value::ImmU8(6), carry);
    let tmp2 = ir.rotate_right_32(x, Value::ImmU8(11), carry);
    let tmp3 = ir.rotate_right_32(x, Value::ImmU8(25), carry);
    let tail = ir.eor_32(tmp2, tmp3);
    ir.eor_32(tmp1, tail)
}

fn polyfill_sha256_message_schedule_0(ir: &mut IREmitter<'_>, x: Value, y: Value) -> Value {
    let t = ir.vector_extract(x, y, 32);
    let mut result = ir.zero_vector();
    for index in 0..4 {
        let element = ir.vector_get_element(32, t, index);
        let carry = Value::ImmU1(false);
        let tmp1 = ir.rotate_right_32(element, Value::ImmU8(7), carry);
        let tmp2 = ir.rotate_right_32(element, Value::ImmU8(18), carry);
        let tmp3 = ir.logical_shift_right_32(element, Value::ImmU8(3), carry);
        let tail = ir.eor_32(tmp2, tmp3);
        let modified = ir.eor_32(tmp1, tail);
        result = ir.vector_set_element(32, result, index, modified);
    }
    ir.vector_add(32, result, x)
}

fn polyfill_sha256_message_schedule_1(
    ir: &mut IREmitter<'_>,
    x: Value,
    y: Value,
    z: Value,
) -> Value {
    let t0 = ir.vector_extract(y, z, 32);

    let t = ir.vector_rotate_whole_vector_right(z, 64);
    let tmp1 = ir.vector_rotate_right(32, t, 17);
    let tmp2 = ir.vector_rotate_right(32, t, 19);
    let tmp3 = ir.vector_logical_shift_right(32, t, 10);
    let tail = ir.vector_eor(tmp2, tmp3);
    let tmp4 = ir.vector_eor(tmp1, tail);
    let x_plus_t0 = ir.vector_add(32, x, t0);
    let tmp5 = ir.vector_add(32, tmp4, x_plus_t0);
    let lower_half = ir.vector_zero_upper(tmp5);

    let tmp1 = ir.vector_rotate_right(32, lower_half, 17);
    let tmp2 = ir.vector_rotate_right(32, lower_half, 19);
    let tmp3 = ir.vector_logical_shift_right(32, lower_half, 10);
    let tail = ir.vector_eor(tmp2, tmp3);
    let tmp4 = ir.vector_eor(tmp1, tail);
    let shuffled_x = ir.vector_rotate_whole_vector_right(x, 64);
    let shuffled_t0 = ir.vector_rotate_whole_vector_right(t0, 64);
    let shuffled_sum = ir.vector_add(32, shuffled_x, shuffled_t0);
    let tmp5 = ir.vector_add(32, tmp4, shuffled_sum);
    let upper_half = ir.vector_get_element(64, tmp5, 0);

    ir.vector_set_element(64, lower_half, 1, upper_half)
}

fn polyfill_sha256_hash(
    ir: &mut IREmitter<'_>,
    mut x: Value,
    mut y: Value,
    w: Value,
    part1: bool,
) -> Value {
    let carry = Value::ImmU1(false);
    for index in 0..4 {
        let low_x = ir.vector_get_element(32, x, 0);
        let after_low_x = ir.vector_get_element(32, x, 1);
        let before_high_x = ir.vector_get_element(32, x, 2);
        let high_x = ir.vector_get_element(32, x, 3);
        let low_y = ir.vector_get_element(32, y, 0);
        let after_low_y = ir.vector_get_element(32, y, 1);
        let before_high_y = ir.vector_get_element(32, y, 2);
        let high_y = ir.vector_get_element(32, y, 3);

        let choice = sha_choose(ir, low_y, after_low_y, before_high_y);
        let majority = sha_majority(ir, low_x, after_low_x, before_high_x);
        let w_element = ir.vector_get_element(32, w, index);
        let sigma1 = sha_hash_sigma1(ir, low_y);
        let choice_plus_w = ir.add_32(choice, w_element, carry);
        let inner = ir.add_32(sigma1, choice_plus_w, carry);
        let t = ir.add_32(high_y, inner, carry);

        let sigma0 = sha_hash_sigma0(ir, low_x);
        let sigma0_plus_majority = ir.add_32(sigma0, majority, carry);
        let new_low_x = ir.add_32(t, sigma0_plus_majority, carry);
        let new_low_y = ir.add_32(t, high_x, carry);

        let shuffled_x = ir.vector_rotate_whole_vector_right(x, 96);
        let shuffled_y = ir.vector_rotate_whole_vector_right(y, 96);
        x = ir.vector_set_element(32, shuffled_x, 0, new_low_x);
        y = ir.vector_set_element(32, shuffled_y, 0, new_low_y);
    }

    if part1 {
        x
    } else {
        y
    }
}

fn polyfill_vector_multiply_widen(
    ir: &mut IREmitter<'_>,
    esize: usize,
    signed: bool,
    n: Value,
    m: Value,
) -> Value {
    let wide_n = if signed {
        ir.vector_sign_extend(esize, n)
    } else {
        ir.vector_zero_extend(esize, n)
    };
    let wide_m = if signed {
        ir.vector_sign_extend(esize, m)
    } else {
        ir.vector_zero_extend(esize, m)
    };
    ir.vector_multiply(esize * 2, wide_n, wide_m)
}

/// Apply upstream host-dependent polyfills before all other optimization
/// passes. Rebuilding is the Rust arena adaptation of inserting before the
/// current C++ list iterator: every original SSA value is mapped to its newly
/// emitted equivalent while instruction order remains topological.
pub fn polyfill(block: &mut Block, options: PolyfillOptions) {
    if options == PolyfillOptions::default() {
        return;
    }

    let original = std::mem::take(&mut block.instructions);
    let mut replacements = Vec::with_capacity(original.len());

    for inst in original {
        if inst.is_tombstone() {
            replacements.push(Value::Void);
            continue;
        }

        let args: Vec<Value> = inst
            .arg_values()
            .copied()
            .map(|arg| remap_value(arg, &replacements))
            .collect();

        let replacement = {
            let mut ir = IREmitter::new(block);
            match inst.opcode {
                Opcode::SHA256MessageSchedule0 if options.sha256 => Some(
                    polyfill_sha256_message_schedule_0(&mut ir, args[0], args[1]),
                ),
                Opcode::SHA256MessageSchedule1 if options.sha256 => Some(
                    polyfill_sha256_message_schedule_1(&mut ir, args[0], args[1], args[2]),
                ),
                Opcode::SHA256Hash if options.sha256 => {
                    let Value::ImmU1(part1) = args[3] else {
                        panic!("SHA256Hash part selector must be immediate")
                    };
                    Some(polyfill_sha256_hash(
                        &mut ir, args[0], args[1], args[2], part1,
                    ))
                }
                Opcode::VectorMultiplySignedWiden8 if options.vector_multiply_widen => Some(
                    polyfill_vector_multiply_widen(&mut ir, 8, true, args[0], args[1]),
                ),
                Opcode::VectorMultiplySignedWiden16 if options.vector_multiply_widen => Some(
                    polyfill_vector_multiply_widen(&mut ir, 16, true, args[0], args[1]),
                ),
                Opcode::VectorMultiplySignedWiden32 if options.vector_multiply_widen => Some(
                    polyfill_vector_multiply_widen(&mut ir, 32, true, args[0], args[1]),
                ),
                Opcode::VectorMultiplyUnsignedWiden8 if options.vector_multiply_widen => Some(
                    polyfill_vector_multiply_widen(&mut ir, 8, false, args[0], args[1]),
                ),
                Opcode::VectorMultiplyUnsignedWiden16 if options.vector_multiply_widen => Some(
                    polyfill_vector_multiply_widen(&mut ir, 16, false, args[0], args[1]),
                ),
                Opcode::VectorMultiplyUnsignedWiden32 if options.vector_multiply_widen => Some(
                    polyfill_vector_multiply_widen(&mut ir, 32, false, args[0], args[1]),
                ),
                _ => None,
            }
        };

        if let Some(replacement) = replacement {
            replacements.push(replacement);
        } else {
            let reference = block.append(inst.opcode, &args);
            replacements.push(Value::Inst(reference));
        }
    }

    block.recompute_use_counts();
    block.rebuild_pseudo_op_links();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::location::LocationDescriptor;

    #[test]
    fn sha256_polyfill_removes_all_host_sha_opcodes() {
        let mut block = Block::new(LocationDescriptor(0));
        let x = block.append(Opcode::ZeroVector, &[]);
        let y = block.append(Opcode::ZeroVector, &[]);
        let z = block.append(Opcode::ZeroVector, &[]);
        let schedule0 = block.append(
            Opcode::SHA256MessageSchedule0,
            &[Value::Inst(x), Value::Inst(y)],
        );
        let schedule1 = block.append(
            Opcode::SHA256MessageSchedule1,
            &[Value::Inst(x), Value::Inst(y), Value::Inst(z)],
        );
        let hash = block.append(
            Opcode::SHA256Hash,
            &[
                Value::Inst(schedule0),
                Value::Inst(y),
                Value::Inst(schedule1),
                Value::ImmU1(true),
            ],
        );
        block.append(
            Opcode::A64SetQ,
            &[
                Value::ImmA64Vec(crate::frontend::a64::types::Vec::V0),
                Value::Inst(hash),
            ],
        );

        polyfill(
            &mut block,
            PolyfillOptions {
                sha256: true,
                vector_multiply_widen: false,
            },
        );

        assert!(!block.instructions.iter().any(|inst| matches!(
            inst.opcode,
            Opcode::SHA256Hash | Opcode::SHA256MessageSchedule0 | Opcode::SHA256MessageSchedule1
        )));
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorRotateWholeVectorRight));
    }

    #[test]
    fn widening_multiply_polyfill_uses_extend_then_multiply() {
        let mut block = Block::new(LocationDescriptor(0));
        let n = block.append(Opcode::ZeroVector, &[]);
        let m = block.append(Opcode::ZeroVector, &[]);
        let product = block.append(
            Opcode::VectorMultiplySignedWiden16,
            &[Value::Inst(n), Value::Inst(m)],
        );
        block.append(
            Opcode::A64SetQ,
            &[
                Value::ImmA64Vec(crate::frontend::a64::types::Vec::V0),
                Value::Inst(product),
            ],
        );

        polyfill(
            &mut block,
            PolyfillOptions {
                sha256: false,
                vector_multiply_widen: true,
            },
        );

        assert!(!block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorMultiplySignedWiden16));
        assert_eq!(
            block
                .instructions
                .iter()
                .filter(|inst| inst.opcode == Opcode::VectorSignExtend16)
                .count(),
            2
        );
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorMultiply32));
    }

    #[test]
    fn rebuilding_preserves_associated_pseudo_operations() {
        let mut block = Block::new(LocationDescriptor(0));
        let (sub_ref, carry_ref) = {
            let mut ir = IREmitter::new(&mut block);
            let sub = ir.sub_32(Value::ImmU32(7), Value::ImmU32(3), Value::ImmU1(true));
            let carry = ir.get_carry_from_op(sub);
            let Value::Inst(sub_ref) = sub else {
                unreachable!()
            };
            let Value::Inst(carry_ref) = carry else {
                unreachable!()
            };
            (sub_ref, carry_ref)
        };

        polyfill(
            &mut block,
            PolyfillOptions {
                sha256: false,
                vector_multiply_widen: true,
            },
        );

        assert_eq!(
            block.get_associated_pseudo_operation(sub_ref, Opcode::GetCarryFromOp),
            Some(carry_ref)
        );
        assert_eq!(block.get(sub_ref).next_pseudoop, Some(carry_ref));
    }
}
