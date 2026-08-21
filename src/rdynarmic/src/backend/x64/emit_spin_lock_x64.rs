//! Inline x64 spin-lock emit helpers.
//!
//! Port of upstream Dynarmic `EmitSpinLockLock` / `EmitSpinLockUnlock` from
//! `dynarmic/common/spin_lock_x64.cpp`. The JIT-emitted sequence operates on
//! a `volatile int*` (4-byte word). Our `ExclusiveMonitor::SpinLock` uses
//! `AtomicU32` which has the same layout, so we can write `dword [ptr]`
//! directly from JIT code.
//!
//! Upstream `EmitSpinLockLock`:
//! ```asm
//!     jmp start
//! loop:
//!     pause
//! start:
//!     mov tmp, 1
//!     lock xchg [ptr], tmp
//!     test tmp, tmp
//!     jnz loop
//! ```
//!
//! Upstream `EmitSpinLockUnlock`:
//! ```asm
//!     xor tmp, tmp
//!     xchg [ptr], tmp
//!     mfence
//! ```
//!
//! The unlock uses a normal (non-locked) xchg because xchg on a memory
//! operand is implicitly locked on x86, and the trailing mfence drains
//! the store buffer before any subsequent guest instruction observes a
//! still-held lock.

use rxbyak::{dword_ptr, CodeAssembler, JmpType, Reg, RegExp};

/// Emit the inline spin-lock acquire sequence.
///
/// `ptr` (Reg64) holds the address of the lock's u32 storage (loaded by
/// the caller — typically `mov ptr, <imm64 monitor lock address>`).
/// `tmp32` (Reg32) is a scratch register clobbered by the xchg.
pub fn emit_spin_lock_lock(asm: &mut CodeAssembler, ptr: Reg, tmp32: Reg) {
    let start = asm.create_label();
    let loop_label = asm.create_label();

    asm.jmp(&start, JmpType::Short).unwrap();
    asm.bind(&loop_label).unwrap();
    asm.pause().unwrap();
    asm.bind(&start).unwrap();
    asm.mov(tmp32, 1u32).unwrap();
    asm.lock().unwrap();
    asm.xchg(dword_ptr(RegExp::from(ptr)), tmp32).unwrap();
    asm.test(tmp32, tmp32).unwrap();
    asm.jnz(&loop_label, JmpType::Short).unwrap();
}

/// Emit the inline spin-lock release sequence.
pub fn emit_spin_lock_unlock(asm: &mut CodeAssembler, ptr: Reg, tmp32: Reg) {
    asm.xor_(tmp32, tmp32).unwrap();
    asm.xchg(dword_ptr(RegExp::from(ptr)), tmp32).unwrap();
    asm.mfence().unwrap();
}
