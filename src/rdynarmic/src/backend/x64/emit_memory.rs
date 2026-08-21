use rxbyak::{qword_ptr, xmmword_ptr, Reg, RegExp, EDX, RAX, RCX, RDX, RSP};

use crate::backend::x64::abi;
use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::hostloc::HostLoc;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::backend::x64::value_classify::ir_value_resolves_to_xmm;
use crate::ir::inst::Inst;
use crate::ir::opcode::Opcode;
use crate::ir::value::{InstRef, Value};

/// Returns true if `value` resolves (through any number of `Identity` chain
/// links) to a `VectorBroadcast64` whose argument is `ImmU64(0)`. Used by
/// the `RUZU_ASSERT_XMM1_ZERO` debug assertion to gate trap emission so
/// only legitimately-zero 128-bit writes are checked.
fn ir_value_resolves_to_broadcast_zero(ctx: &EmitContext, value: &Value) -> bool {
    let Some(block) = ctx.block else { return false };
    let mut cur = match value {
        Value::Inst(r) => *r,
        _ => return false,
    };
    // Walk Identity chain.
    loop {
        let inst = block.get(cur);
        if inst.opcode == Opcode::Identity {
            match inst.args[0] {
                Value::Inst(next) => cur = next,
                _ => return false,
            }
        } else {
            // Non-Identity: check if it's VectorBroadcast64 of ImmU64(0).
            if inst.opcode == Opcode::VectorBroadcast64 {
                return matches!(inst.args[0], Value::ImmU64(0));
            }
            return false;
        }
    }
}

// ---------------------------------------------------------------------------
// Memory read operations (via host callbacks)
// ---------------------------------------------------------------------------

/// A64ReadMemory8: result = mem[vaddr] (8-bit, zero-extended to 64)
pub fn emit_a64_read_memory_8(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_read(ctx, ra, inst_ref, inst, 8);
}

/// A64ReadMemory16: result = mem[vaddr] (16-bit, zero-extended to 64)
pub fn emit_a64_read_memory_16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_read(ctx, ra, inst_ref, inst, 16);
}

/// A64ReadMemory32: result = mem[vaddr] (32-bit, zero-extended to 64)
pub fn emit_a64_read_memory_32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_read(ctx, ra, inst_ref, inst, 32);
}

/// A64ReadMemory64: result = mem[vaddr] (64-bit)
pub fn emit_a64_read_memory_64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_read(ctx, ra, inst_ref, inst, 64);
}

/// A64ReadMemory128: result = mem[vaddr] (128-bit)
pub fn emit_a64_read_memory_128(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_read(ctx, ra, inst_ref, inst, 128);
}

fn emit_memory_read(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
    bitsize: usize,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());

    // args[0] = location descriptor (upper), args[1] = vaddr, args[2] = acc_type
    // Callbacks are ArgCallback: trampoline(context_ptr, vaddr) → RDI=context, RSI=vaddr
    // host_call position 0 is left None (ArgCallback fills RDI with context pointer)
    // host_call position 1 → RSI = vaddr

    if bitsize == 128 {
        let result = ra.scratch_xmm();
        ra.host_call(None, &mut [None, Some(&mut args[1]), None, None]);

        #[cfg(all(target_os = "windows", target_env = "msvc"))]
        {
            let frame_size = 16 + abi::ABI_SHADOW_SPACE;
            ra.alloc_stack_space(frame_size);
            ctx.config
                .callbacks
                .memory_read_128
                .emit_call(&mut *ra.asm, &|code, params| {
                    code.lea(
                        params[1],
                        qword_ptr(RegExp::from(RSP) + abi::ABI_SHADOW_SPACE as i32),
                    )
                })
                .unwrap();
            ra.asm
                .movups(
                    result,
                    xmmword_ptr(RegExp::from(RSP) + abi::ABI_SHADOW_SPACE as i32),
                )
                .unwrap();
            ra.release_stack_space(frame_size);
        }

        #[cfg(not(all(target_os = "windows", target_env = "msvc")))]
        {
            ctx.config
                .callbacks
                .memory_read_128
                .emit_call_simple(&mut *ra.asm)
                .unwrap();
            ra.asm.movq(result, RAX).unwrap();
            ra.asm.pinsrq(result, RDX, 1).unwrap();
        }
        ra.define_value(inst_ref, result);
        return;
    }

    ra.host_call(Some(inst_ref), &mut [None, Some(&mut args[1]), None, None]);

    // Call the appropriate read callback.
    // The callback receives: RDI = vaddr
    // Returns: RAX = value (zero-extended for < 64-bit reads)
    let callback = match bitsize {
        8 => &ctx.config.callbacks.memory_read_8,
        16 => &ctx.config.callbacks.memory_read_16,
        32 => &ctx.config.callbacks.memory_read_32,
        64 => &ctx.config.callbacks.memory_read_64,
        _ => unreachable!("Invalid memory read bitsize: {}", bitsize),
    };

    callback.emit_call_simple(&mut *ra.asm).unwrap();

    // Result is in RAX (defined by host_call via result_def = Some(inst_ref))
}

// ---------------------------------------------------------------------------
// Memory write operations (via host callbacks)
// ---------------------------------------------------------------------------

/// A64WriteMemory8: mem[vaddr] = value (8-bit)
pub fn emit_a64_write_memory_8(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_write(ctx, ra, inst_ref, inst, 8);
}

/// A64WriteMemory16: mem[vaddr] = value (16-bit)
pub fn emit_a64_write_memory_16(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_write(ctx, ra, inst_ref, inst, 16);
}

/// A64WriteMemory32: mem[vaddr] = value (32-bit)
pub fn emit_a64_write_memory_32(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_write(ctx, ra, inst_ref, inst, 32);
}

/// A64WriteMemory64: mem[vaddr] = value (64-bit)
pub fn emit_a64_write_memory_64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_write(ctx, ra, inst_ref, inst, 64);
}

/// A64WriteMemory128: mem[vaddr] = value (128-bit)
pub fn emit_a64_write_memory_128(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_memory_write(ctx, ra, inst_ref, inst, 128);
}

fn emit_memory_write(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
    bitsize: usize,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());

    // args[0] = location descriptor (upper), args[1] = vaddr, args[2] = value, args[3] = acc_type

    if bitsize == 128 {
        // Mirror upstream `EmitMemoryWrite<128>` (a64_emit_x64_memory.cpp.inc:
        // 149-154):
        //   Use(args[1], ABI_PARAM2);  // pin vaddr to RSI
        //   Use(args[2], HostLoc::XMM1); // pin value to XMM1
        //   EndOfAllocScope();
        //   HostCall(nullptr);          // spills all unlocked caller-saves
        //   CallFunction(memory_write_128);
        //
        // Why the dance: host_call's caller-save scratch loop only spills
        // unlocked locations. If we kept value_xmm read-locked across the
        // call, the JIT bookkeeping would claim it still holds the value
        // even though the user callback clobbers caller-save XMMs.
        // Subsequent uses of the same IR value would then read garbage.
        //
        // EndOfAllocScope releases the per-arg locks, host_call(None)
        // then spills both vaddr and value to stack slots (the spill is a
        // memory STORE — the hardware register contents survive). After
        // the call, future uses of these IR values reload from the spill
        // slots, not the clobbered registers.
        //
        // Without this, STK's `MOVI V31.4S, #0` followed by 5 stores of
        // V31 produced correct zeros for the first store and 0xFF for
        // the rest, because subsequent A64WriteMemory128 emits read XMM1
        // after the trampoline had clobbered it.
        let (first, rest) = args.split_at_mut(2);
        ra.use_loc(&mut first[1], abi::ABI_PARAMS[1]); // vaddr → RSI
        ra.use_loc(&mut rest[0], HostLoc::Xmm(1)); // value → XMM1

        // RUZU_ASSERT_XMM1_ZERO=1 — emit a runtime trap before the call if
        // xmm1 != 0. Used to debug STK's bad_alloc where v31 (correctly
        // zeroed by MOVI v31.4s, #0 + A64SetQ V31) was stored to memory
        // with the wrong value (lo=0xFFFFFFFFFFFFFFFF, hi=0x00FFFFFF00FFFFFF).
        // The trap fires at multiple points to bisect WHERE xmm1 is
        // corrupted: after use_loc (point A), after host_call (point B),
        // immediately before the call (point C). The assertion uses
        // `ptest xmm1, xmm1; jz +2; ud2` — UD2 raises SIGILL if xmm1 != 0.
        // RUZU_ASSERT_XMM1_ZERO=A|B|C — emit `ptest xmm1, xmm1; je +2; ud2`
        // at one of three bisection points to find where xmm1 diverges from
        // the JIT-emit expectation of zero. Only emits when args[2] is
        // Identity (or chained Identity) of `VectorBroadcast64 ImmU64(0)`
        // — i.e., the MOVI vN.4s, #0 pattern that's known to fail in STK.
        // RUZU_ASSERT_XMM1_ZERO_PC=0xPC[,...] — restrict to specific blocks.
        let assert_point = std::env::var("RUZU_ASSERT_XMM1_ZERO").ok();
        let value_is_broadcast_zero = ir_value_resolves_to_broadcast_zero(ctx, &inst.args[2]);
        let block_pc = ctx.arch.extract_pc(ctx.location);
        let pc_filter_ok = match std::env::var("RUZU_ASSERT_XMM1_ZERO_PC") {
            Ok(spec) => {
                let pcs: Vec<u64> = spec
                    .split(',')
                    .filter_map(|p| u64::from_str_radix(p.trim().trim_start_matches("0x"), 16).ok())
                    .collect();
                pcs.contains(&block_pc)
            }
            Err(_) => true,
        };
        let value_is_broadcast_zero = value_is_broadcast_zero && pc_filter_ok;
        if assert_point.is_some() && value_is_broadcast_zero {
            eprintln!(
                "[ASSERT_XMM1_ZERO] emitting assertion for block PC=0x{:016X}",
                block_pc
            );
        }
        if assert_point.as_deref() == Some("A") && value_is_broadcast_zero {
            ra.asm.ptest(Reg::xmm(1), Reg::xmm(1)).unwrap();
            ra.asm.db(0x74).unwrap();
            ra.asm.db(0x02).unwrap();
            ra.asm.ud2().unwrap();
        }

        ra.end_of_alloc_scope();
        ra.host_call(None, &mut [None, None, None, None]);

        if assert_point.as_deref() == Some("B") && value_is_broadcast_zero {
            ra.asm.ptest(Reg::xmm(1), Reg::xmm(1)).unwrap();
            ra.asm.db(0x74).unwrap();
            ra.asm.db(0x02).unwrap();
            ra.asm.ud2().unwrap();
        }

        // RSI already holds vaddr (preserved across the spill — spill is
        // a memory store). Extract XMM1's lo/hi into RDX/RCX before the
        // call, since the trampoline expects (inner, vaddr, lo, hi) per
        // SystemV ABI: RDI/RSI/RDX/RCX. RDI is filled by the ArgCallback
        // wrapper inside emit_call_simple.
        ra.asm.movq(RDX, Reg::xmm(1)).unwrap();
        ra.asm.pextrq(RCX, Reg::xmm(1), 1).unwrap();

        if assert_point.as_deref() == Some("C") && value_is_broadcast_zero {
            ra.asm.ptest(Reg::xmm(1), Reg::xmm(1)).unwrap();
            ra.asm.db(0x74).unwrap();
            ra.asm.db(0x02).unwrap();
            ra.asm.ud2().unwrap();
        }

        ctx.config
            .callbacks
            .memory_write_128
            .emit_call_simple(&mut *ra.asm)
            .unwrap();
        return;
    }

    if matches!(bitsize, 32 | 64) && ir_value_resolves_to_xmm(ctx, ra, &args[2].value) {
        // `A64WriteMemory32/64` can consume scalar FP/SIMD values represented
        // as U128 in XMM (for example `REV64 V0.2S` followed by `STR D0`).
        // Pin the XMM value and extract its low lane into the integer ABI
        // value register, matching the fastmem 32/64 path.
        let (first, rest) = args.split_at_mut(2);
        ra.use_loc(&mut first[1], abi::ABI_PARAMS[1]); // vaddr -> RSI
        ra.use_loc(&mut rest[0], HostLoc::Xmm(1)); // value -> XMM1
        ra.end_of_alloc_scope();
        ra.host_call(None, &mut [None, None, None, None]);
        if bitsize == 64 {
            ra.asm.movq(RDX, Reg::xmm(1)).unwrap();
        } else {
            ra.asm.movd(EDX, Reg::xmm(1)).unwrap();
        }
        let callback = match bitsize {
            32 => &ctx.config.callbacks.memory_write_32,
            64 => &ctx.config.callbacks.memory_write_64,
            _ => unreachable!(),
        };
        callback.emit_call_simple(&mut *ra.asm).unwrap();
        return;
    }

    // Callbacks are ArgCallback: trampoline(context_ptr, vaddr, value) → RDI=context, RSI=vaddr, RDX=value
    // host_call position 0 is left None (ArgCallback fills RDI with context pointer)
    // host_call position 1 → RSI = vaddr, position 2 → RDX = value
    let (first, rest) = args.split_at_mut(2);
    ra.host_call(
        None,
        &mut [None, Some(&mut first[1]), Some(&mut rest[0]), None],
    );

    let callback = match bitsize {
        8 => &ctx.config.callbacks.memory_write_8,
        16 => &ctx.config.callbacks.memory_write_16,
        32 => &ctx.config.callbacks.memory_write_32,
        64 => &ctx.config.callbacks.memory_write_64,
        _ => unreachable!("Invalid memory write bitsize: {}", bitsize),
    };

    callback.emit_call_simple(&mut *ra.asm).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_op_signatures() {
        // Just verify the functions exist with the right signatures
        // Actual emission requires callbacks which need host function pointers
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_a64_read_memory_8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_a64_read_memory_64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_a64_write_memory_8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_a64_write_memory_64;
    }
}
