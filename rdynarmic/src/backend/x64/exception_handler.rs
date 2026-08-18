//! SIGSEGV/Mach exception handler for fastmem fallback.
//!
//! On Linux: matches upstream `dynarmic/backend/exception_handler_posix.cpp`
//! using SIGSEGV + ucontext_t / gregs[REG_RIP].
//!
//! On macOS: upstream uses Mach exceptions (`exception_handler_macos.cpp`),
//! which requires a dedicated Mach port thread and x86_thread_state64_t /
//! arm_thread_state64_t access. Since rdynarmic only has an x64 backend and
//! macOS arm64 cannot run x64 JIT code natively, we provide a stub that
//! disables fastmem on macOS (matching upstream's `SupportsFastmem() = false`
//! path when the Mach handler is absent).

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::ir::location::LocationDescriptor;

/// Identifies a microinstruction within a block, for the do-not-fastmem set.
///
/// Matches upstream `using DoNotFastmemMarker = std::tuple<IR::LocationDescriptor, unsigned>;`
/// where the second element is `inst->GetName()` — a unique id of the
/// microinstruction within its block. In rdynarmic we use the `InstRef`
/// index value (as `u32`).
pub type DoNotFastmemMarker = (LocationDescriptor, u32);

/// Information recorded for each fastmem memory instruction.
#[derive(Debug)]
pub struct FastmemPatchInfo {
    /// Address to resume after the fallback stub returns.
    pub resume_rip: u64,
    /// Address of the per-register fallback stub to call.
    pub callback: u64,
    /// Marker identifying the source microinstruction; inserted into
    /// `do_not_fastmem` if `recompile` is set and a fault occurs.
    pub marker: Option<DoNotFastmemMarker>,
    /// Whether to recompile the block without fastmem on repeated faults.
    pub recompile: bool,
    /// Set by the exception handler and drained after generated code returns.
    pending_recompile: AtomicBool,
}

impl FastmemPatchInfo {
    pub fn new(
        resume_rip: u64,
        callback: u64,
        marker: Option<DoNotFastmemMarker>,
        recompile: bool,
    ) -> Self {
        Self {
            resume_rip,
            callback,
            marker,
            recompile,
            pending_recompile: AtomicBool::new(false),
        }
    }
}

/// Redirected call information returned by the fastmem callback.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FakeCall {
    /// Address of fallback function to jump to.
    pub call_rip: u64,
    /// Address to return to after fallback completes (pushed on stack).
    pub ret_rip: u64,
}

/// Callback type: given faulting RIP, returns FakeCall or None.
type FastmemCallback = Box<dyn Fn(u64) -> Option<FakeCall> + Send>;

/// Whether this x64 backend has a working host exception path for fastmem.
///
/// Mirrors upstream `ExceptionHandler::SupportsFastmem()`. Linux/x86-64 and
/// Windows/x86-64 have native handlers below; every other target uses the
/// callback/page-table paths instead of emitting faulting direct accesses.
pub const fn supports_fastmem() -> bool {
    cfg!(any(
        all(target_os = "linux", target_arch = "x86_64"),
        all(target_os = "windows", target_arch = "x86_64")
    ))
}

// ── Linux-only: SIGSEGV-based fastmem handler ─────────────────────────────────
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use std::sync::Mutex;

/// Code block range with its associated fastmem callback.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
struct CodeBlockInfo {
    code_begin: u64,
    code_end: u64,
    callback: FastmemCallback,
}

/// Global signal handler state.
/// There's only one SIGSEGV handler per process, so this must be global.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
struct SigHandlerState {
    code_blocks: Vec<CodeBlockInfo>,
    old_sa: libc::sigaction,
    installed: bool,
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
unsafe impl Send for SigHandlerState {}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
static SIG_HANDLER: Mutex<Option<SigHandlerState>> = Mutex::new(None);

/// Register a JIT code region with the SIGSEGV handler (Linux) or no-op (macOS).
///
/// On Linux: installs SIGSEGV handler on first call, records the code range.
/// On macOS: fastmem is not supported; this function does nothing.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub fn register_code_block(code_begin: *const u8, code_end: *const u8, callback: FastmemCallback) {
    let mut guard = SIG_HANDLER.lock().unwrap();
    let state = guard.get_or_insert_with(|| install_signal_handler());
    state.code_blocks.push(CodeBlockInfo {
        code_begin: code_begin as u64,
        code_end: code_end as u64,
        callback,
    });
}

#[cfg(not(any(
    all(target_os = "linux", target_arch = "x86_64"),
    all(target_os = "windows", target_arch = "x86_64")
)))]
pub fn register_code_block(
    _code_begin: *const u8,
    _code_end: *const u8,
    _callback: FastmemCallback,
) {
}

/// Register a per-thread alternate signal stack for the current thread.
/// Linux only — no-op on macOS.
#[cfg(not(any(
    all(target_os = "linux", target_arch = "x86_64"),
    all(target_os = "windows", target_arch = "x86_64")
)))]
pub fn register_thread_signal_stack() {}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub fn register_thread_signal_stack() {
    thread_local! {
        static ALTSTACK_INSTALLED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    }
    ALTSTACK_INSTALLED.with(|installed| {
        if installed.get() {
            return;
        }
        unsafe {
            let stack_size = 2 * 1024 * 1024;
            let stack_ptr = libc::mmap(
                std::ptr::null_mut(),
                stack_size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            );
            if stack_ptr != libc::MAP_FAILED {
                let ss = libc::stack_t {
                    ss_sp: stack_ptr,
                    ss_flags: 0,
                    ss_size: stack_size,
                };
                if libc::sigaltstack(&ss, std::ptr::null_mut()) == 0 {
                    installed.set(true);
                }
            }
        }
    });
}

/// Unregister a JIT code region. Linux only — no-op on macOS.
#[cfg(not(any(
    all(target_os = "linux", target_arch = "x86_64"),
    all(target_os = "windows", target_arch = "x86_64")
)))]
pub fn unregister_code_block(_code_begin: *const u8) {}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub fn unregister_code_block(code_begin: *const u8) {
    let mut guard = SIG_HANDLER.lock().unwrap();
    if let Some(state) = guard.as_mut() {
        state
            .code_blocks
            .retain(|b| b.code_begin != code_begin as u64);
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn install_signal_handler() -> SigHandlerState {
    unsafe {
        // Allocate alternate signal stack (2 MB, matching upstream)
        let stack_size = 2 * 1024 * 1024;
        let stack_ptr = libc::mmap(
            std::ptr::null_mut(),
            stack_size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        );
        if stack_ptr != libc::MAP_FAILED {
            let ss = libc::stack_t {
                ss_sp: stack_ptr,
                ss_flags: 0,
                ss_size: stack_size,
            };
            libc::sigaltstack(&ss, std::ptr::null_mut());
        }

        let mut old_sa: libc::sigaction = std::mem::zeroed();
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = sig_action as usize;
        sa.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK | libc::SA_RESTART;
        libc::sigemptyset(&mut sa.sa_mask);

        libc::sigaction(libc::SIGSEGV, &sa, &mut old_sa);

        SigHandlerState {
            code_blocks: Vec::new(),
            old_sa,
            installed: true,
        }
    }
}

/// SIGSEGV signal handler. Linux only.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
extern "C" fn sig_action(
    sig: libc::c_int,
    _info: *mut libc::siginfo_t,
    raw_context: *mut libc::c_void,
) {
    unsafe {
        let ucontext = &mut *(raw_context as *mut libc::ucontext_t);
        let mctx = &mut ucontext.uc_mcontext;
        let rip = mctx.gregs[libc::REG_RIP as usize] as u64;
        let rsp_ref = &mut mctx.gregs[libc::REG_RSP as usize];

        // Try to handle via registered code blocks
        let guard = SIG_HANDLER.lock().unwrap();
        if let Some(state) = guard.as_ref() {
            let mut hit_range = false;
            for block in &state.code_blocks {
                if rip >= block.code_begin && rip < block.code_end {
                    hit_range = true;
                    if let Some(fake_call) = (block.callback)(rip) {
                        // RUZU_TRACE_FASTMEM_FAULT=1 — log each SIGSEGV
                        // fastmem fallback dispatch (async-signal-safe).
                        if std::env::var_os("RUZU_TRACE_FASTMEM_FAULT").is_some() {
                            let prefix: &[u8] = b"[FASTMEM_FAULT] rip=0x";
                            let mut buf = [0u8; 96];
                            let mut nb = prefix.len();
                            buf[..nb].copy_from_slice(prefix);
                            for shift in (0..64).step_by(4).rev() {
                                let nib = ((rip >> shift) & 0xF) as u8;
                                buf[nb] = if nib < 10 {
                                    b'0' + nib
                                } else {
                                    b'a' + nib - 10
                                };
                                nb += 1;
                            }
                            buf[nb] = b'\n';
                            nb += 1;
                            let _ = libc::write(2, buf.as_ptr() as *const _, nb);
                        }
                        // "Fake call": push ret_rip, set RIP to call_rip
                        *rsp_ref -= 8;
                        let stack_ptr = *rsp_ref as *mut u64;
                        *stack_ptr = fake_call.ret_rip;
                        mctx.gregs[libc::REG_RIP as usize] = fake_call.call_rip as i64;
                        return;
                    }
                }
            }
            // Async-signal-safe log of any SIGSEGV that reaches this
            // point (either outside all JIT code blocks, or inside
            // them but with no patch entry for the faulting RIP).
            //
            // Without this print, the default handler chain would
            // just kill the process. Format manually into a fixed
            // stack buffer; format!() and env::var() both allocate
            // which is unsafe in a signal handler.
            let prefix: &[u8] = if hit_range {
                b"[SIGSEGV] unhandled JIT fault (in code range, no patch): rip=0x"
            } else {
                b"[SIGSEGV] fault outside JIT code: rip=0x"
            };
            let mut buf = [0u8; 96];
            let mut n = prefix.len();
            buf[..n].copy_from_slice(prefix);
            // 16-digit big-endian hex of `rip`.
            for shift in (0..64).step_by(4).rev() {
                let nib = ((rip >> shift) & 0xF) as u8;
                buf[n] = if nib < 10 {
                    b'0' + nib
                } else {
                    b'a' + nib - 10
                };
                n += 1;
            }
            buf[n] = b'\n';
            n += 1;
            let _ = libc::write(2, buf.as_ptr() as *const _, n);

            // Dump 16 bytes around the faulting RIP (rip-4..rip+12) so we
            // can identify the actual faulting instruction. Useful for
            // diagnosing ordered-fastmem patches that don't match: if
            // the fault is at the `LOCK XADD` we expect 0xF0 at byte +0,
            // if it's at `XCHG` we expect a REX prefix (0x4?) then 0x87.
            if hit_range {
                let mut bb = [0u8; 96];
                let p3 = b"[SIGSEGV]   bytes @ rip-4..+11:";
                let mut k = p3.len();
                bb[..k].copy_from_slice(p3);
                let start = rip.wrapping_sub(4) as *const u8;
                for i in 0..16usize {
                    let byte = *start.add(i);
                    bb[k] = b' ';
                    k += 1;
                    let nib_hi = (byte >> 4) & 0xF;
                    let nib_lo = byte & 0xF;
                    bb[k] = if nib_hi < 10 {
                        b'0' + nib_hi
                    } else {
                        b'a' + nib_hi - 10
                    };
                    k += 1;
                    bb[k] = if nib_lo < 10 {
                        b'0' + nib_lo
                    } else {
                        b'a' + nib_lo - 10
                    };
                    k += 1;
                    if i == 3 {
                        bb[k] = b'|';
                        k += 1;
                    }
                }
                bb[k] = b'\n';
                k += 1;
                let _ = libc::write(2, bb.as_ptr() as *const _, k);
            }

            // Also emit `sig_action`'s own runtime address so callers
            // can compute `binary_base = sig_action_runtime - file_off`
            // and feed `(rip - binary_base + file_base_of_sig_action)`
            // to addr2line to symbolicate the fault site.
            let sa_addr = sig_action as usize as u64;
            let mut buf2 = [0u8; 80];
            let p2 = b"[SIGSEGV]   reference: sig_action runtime addr=0x";
            let mut m = p2.len();
            buf2[..m].copy_from_slice(p2);
            for shift in (0..64).step_by(4).rev() {
                let nib = ((sa_addr >> shift) & 0xF) as u8;
                buf2[m] = if nib < 10 {
                    b'0' + nib
                } else {
                    b'a' + nib - 10
                };
                m += 1;
            }
            buf2[m] = b'\n';
            m += 1;
            let _ = libc::write(2, buf2.as_ptr() as *const _, m);

            // Dump key host GPRs for the rip=0-from-NULL-call diagnosis.
            // R15 holds JitState pointer; reading guest PC from R15+0 lets
            // us correlate the host fault with the guest instruction that
            // caused it.
            let dump_reg = |label: &[u8], val: u64| {
                let mut b = [0u8; 64];
                let mut k = 0;
                b[..label.len()].copy_from_slice(label);
                k += label.len();
                for shift in (0..64).step_by(4).rev() {
                    let nib = ((val >> shift) & 0xF) as u8;
                    b[k] = if nib < 10 {
                        b'0' + nib
                    } else {
                        b'a' + nib - 10
                    };
                    k += 1;
                }
                b[k] = b'\n';
                k += 1;
                let _ = libc::write(2, b.as_ptr() as *const _, k);
            };
            dump_reg(
                b"[SIGSEGV]   host RAX=0x",
                mctx.gregs[libc::REG_RAX as usize] as u64,
            );
            dump_reg(
                b"[SIGSEGV]   host RCX=0x",
                mctx.gregs[libc::REG_RCX as usize] as u64,
            );
            dump_reg(
                b"[SIGSEGV]   host RDX=0x",
                mctx.gregs[libc::REG_RDX as usize] as u64,
            );
            dump_reg(
                b"[SIGSEGV]   host RSP=0x",
                mctx.gregs[libc::REG_RSP as usize] as u64,
            );
            dump_reg(
                b"[SIGSEGV]   host RBP=0x",
                mctx.gregs[libc::REG_RBP as usize] as u64,
            );
            dump_reg(
                b"[SIGSEGV]   host R12=0x",
                mctx.gregs[libc::REG_R12 as usize] as u64,
            );
            dump_reg(
                b"[SIGSEGV]   host R13=0x",
                mctx.gregs[libc::REG_R13 as usize] as u64,
            );
            dump_reg(
                b"[SIGSEGV]   host R14=0x",
                mctx.gregs[libc::REG_R14 as usize] as u64,
            );
            dump_reg(
                b"[SIGSEGV]   host R15=0x",
                mctx.gregs[libc::REG_R15 as usize] as u64,
            );
            // [RSP] = return address pushed by the bad CALL — points at
            // the JIT-emitted host code immediately after the call.
            let rsp = mctx.gregs[libc::REG_RSP as usize] as u64;
            if rsp != 0 {
                let ret_addr = unsafe { (rsp as *const u64).read_unaligned() };
                dump_reg(b"[SIGSEGV]   [RSP] (caller's RIP)=0x", ret_addr);
            }

            // RBP-chain walk if the binary was built with frame pointers
            // (`RUSTFLAGS="-C force-frame-pointers=yes"`). Each frame:
            //   [rbp+8] = return address (caller's RIP after `call`)
            //   [rbp]   = saved RBP of the caller's frame
            // Walks up to 32 frames or until a clearly bogus pointer.
            // Async-signal-safe.
            let rbp = mctx.gregs[libc::REG_RBP as usize] as u64;
            if rbp != 0 && rbp >= 0x1000 {
                let header =
                    b"[SIGSEGV]   frame walk (RBP chain; requires -C force-frame-pointers=yes):\n";
                let _ = libc::write(2, header.as_ptr() as *const _, header.len());
                let mut cur_rbp = rbp;
                for depth in 0..32usize {
                    // Sanity: stop if cur_rbp is clearly invalid.
                    if cur_rbp < 0x1000 || cur_rbp & 0x7 != 0 {
                        break;
                    }
                    let saved_rbp = unsafe { (cur_rbp as *const u64).read_unaligned() };
                    let ret_addr = unsafe { ((cur_rbp + 8) as *const u64).read_unaligned() };
                    let mut b = [0u8; 96];
                    let mut k = 0;
                    let prefix = b"[SIGSEGV]     frame[";
                    b[..prefix.len()].copy_from_slice(prefix);
                    k += prefix.len();
                    // depth as decimal (1-2 digits).
                    if depth >= 10 {
                        b[k] = b'0' + (depth / 10) as u8;
                        k += 1;
                    }
                    b[k] = b'0' + (depth % 10) as u8;
                    k += 1;
                    let mid = b"] rbp=0x";
                    b[k..k + mid.len()].copy_from_slice(mid);
                    k += mid.len();
                    for shift in (0..64).step_by(4).rev() {
                        let nib = ((cur_rbp >> shift) & 0xF) as u8;
                        b[k] = if nib < 10 {
                            b'0' + nib
                        } else {
                            b'a' + nib - 10
                        };
                        k += 1;
                    }
                    let mid2 = b" ret=0x";
                    b[k..k + mid2.len()].copy_from_slice(mid2);
                    k += mid2.len();
                    for shift in (0..64).step_by(4).rev() {
                        let nib = ((ret_addr >> shift) & 0xF) as u8;
                        b[k] = if nib < 10 {
                            b'0' + nib
                        } else {
                            b'a' + nib - 10
                        };
                        k += 1;
                    }
                    b[k] = b'\n';
                    k += 1;
                    let _ = libc::write(2, b.as_ptr() as *const _, k);
                    if saved_rbp == 0 || saved_rbp <= cur_rbp {
                        break;
                    }
                    cur_rbp = saved_rbp;
                }
            }

            // Dump 64 quadwords starting at RSP. Fallback for builds
            // without frame pointers: the user can offline-symbolicate any
            // value that lands in the binary's text range via `addr2line`.
            // Async-signal-safe: only reads of stack memory + libc::write.
            if rsp != 0 {
                let header = b"[SIGSEGV]   stack dump (RSP..+0x200 in qwords; addr2line each):\n";
                let _ = libc::write(2, header.as_ptr() as *const _, header.len());
                for i in 0..64usize {
                    let addr = rsp + (i as u64) * 8;
                    let val = unsafe { (addr as *const u64).read_unaligned() };
                    let mut b = [0u8; 64];
                    let mut k = 0;
                    let prefix = b"[SIGSEGV]     [RSP+0x";
                    b[..prefix.len()].copy_from_slice(prefix);
                    k += prefix.len();
                    // Offset hex (3 nibbles enough for 0x200).
                    let off = (i as u64) * 8;
                    for shift in (0..12).step_by(4).rev() {
                        let nib = ((off >> shift) & 0xF) as u8;
                        b[k] = if nib < 10 {
                            b'0' + nib
                        } else {
                            b'a' + nib - 10
                        };
                        k += 1;
                    }
                    let sep = b"]=0x";
                    b[k..k + sep.len()].copy_from_slice(sep);
                    k += sep.len();
                    for shift in (0..64).step_by(4).rev() {
                        let nib = ((val >> shift) & 0xF) as u8;
                        b[k] = if nib < 10 {
                            b'0' + nib
                        } else {
                            b'a' + nib - 10
                        };
                        k += 1;
                    }
                    b[k] = b'\n';
                    k += 1;
                    let _ = libc::write(2, b.as_ptr() as *const _, k);
                }
            }

            libc::abort();

            // Not handled by any code block — chain to previous handler
            if state.old_sa.sa_flags & libc::SA_SIGINFO != 0 {
                if let Some(handler) = std::mem::transmute::<
                    usize,
                    Option<extern "C" fn(libc::c_int, *mut libc::siginfo_t, *mut libc::c_void)>,
                >(state.old_sa.sa_sigaction)
                {
                    drop(guard);
                    handler(sig, _info, raw_context);
                    return;
                }
            }
        }

        // No handler — re-raise with default
        drop(guard);
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = libc::SIG_DFL;
        libc::sigaction(libc::SIGSEGV, &sa, std::ptr::null_mut());
    }
}

// ── Windows SEH-based fastmem handler ─────────────────────────────────────────
//
// Matches upstream `dynarmic/backend/x64/exception_handler_windows.cpp`.
//
// On Windows the OS uses Structured Exception Handling (SEH).  When a page
// fault fires inside JIT code the CPU unwinds through the registered
// RUNTIME_FUNCTION table and invokes our exception handler.  There is no
// signal mechanism.
//
// Implementation notes:
//  - `register_code_block` emits two small stubs into the code buffer and
//    writes UNWIND_INFO + RUNTIME_FUNCTION data after them.  Both structures
//    live inside the code buffer (RWX region), which is valid on Windows.
//  - The stubs are emitted as part of the initial setup before any blocks are
//    compiled; `EndAddress` covers the total capacity so we never need to
//    re-register as new blocks arrive.
//  - The `with_cb` stub calls the fixed Rust function `seh_fastmem_dispatch`
//    which looks up the faulting RIP in the global state and patches CONTEXT.
//  - UNWIND_INFO describes the prologue of `gen_run_code` (8 GPR pushes + sub
//    rsp 0xNNN + movaps for XMM6-15). Offsets and register codes match
//    exactly the instructions emitted by `block_of_code.rs`.
//
// References used for struct layouts:
//  Microsoft PE/COFF spec, chapter "x64 Exception Handling"
//  NT headers: RUNTIME_FUNCTION, UNWIND_INFO, UNWIND_CODE, CONTEXT

#[cfg(all(target_os = "windows", target_arch = "x86_64"))]
mod windows_seh {
    use std::sync::Mutex;

    use super::{FakeCall, FastmemCallback};

    // ── Win32 types (declared locally to avoid winapi/windows-sys dep) ─────────

    extern "system" {
        fn RtlAddFunctionTable(
            function_table: *mut RuntimeFunction,
            entry_count: u32,
            base_address: u64,
        ) -> u8;
        fn RtlDeleteFunctionTable(function_table: *mut RuntimeFunction) -> u8;
        #[cfg(test)]
        fn RtlVirtualUnwind(
            handler_type: u32,
            image_base: u64,
            control_pc: u64,
            function_entry: *mut RuntimeFunction,
            context_record: *mut u8,
            handler_data: *mut *mut core::ffi::c_void,
            establisher_frame: *mut u64,
            context_pointers: *mut core::ffi::c_void,
        ) -> *mut core::ffi::c_void;
    }

    #[repr(C)]
    struct RuntimeFunction {
        begin_address: u32,
        end_address: u32,
        unwind_data: u32,
    }

    // UNWIND_CODE field constants
    const UWOP_PUSH_NONVOL: u8 = 0;
    const UWOP_ALLOC_LARGE: u8 = 1;
    const UWOP_SAVE_XMM128: u8 = 8;

    // Register codes for UWOP_PUSH_NONVOL / UWOP_SAVE_XMM128
    const UWRC_RBX: u8 = 3;
    const UWRC_RSP: u8 = 4; // unused but kept for clarity
    const UWRC_RBP: u8 = 5;
    const UWRC_RSI: u8 = 6;
    const UWRC_RDI: u8 = 7;
    const UWRC_R12: u8 = 12;
    const UWRC_R13: u8 = 13;
    const UWRC_R14: u8 = 14;
    const UWRC_R15: u8 = 15;

    // UNW_FLAG_EHANDLER — the UNWIND_INFO has an exception handler
    const UNW_FLAG_EHANDLER: u8 = 1;

    // ExceptionContinueSearch / ExceptionContinueExecution
    const EXCEPTION_CONTINUE_EXECUTION: i32 = 0;
    const EXCEPTION_CONTINUE_SEARCH: i32 = 1;

    // Windows CONTEXT struct offsets (x64, from WinNT.h):
    //   DWORD64 Rbx at +0x90 through R15 at +0xF0,
    //   DWORD64 Rsp at +0x98, DWORD64 Rip at +0xF8.
    const CTX_RBX_OFF: usize = 0x90;
    const CTX_RSP_OFF: usize = 0x98;
    const CTX_RBP_OFF: usize = 0xA0;
    const CTX_RSI_OFF: usize = 0xA8;
    const CTX_RDI_OFF: usize = 0xB0;
    const CTX_R12_OFF: usize = 0xD8;
    const CTX_R13_OFF: usize = 0xE0;
    const CTX_R14_OFF: usize = 0xE8;
    const CTX_R15_OFF: usize = 0xF0;
    const CTX_RIP_OFF: usize = 0xF8;

    // ── Global state ────────────────────────────────────────────────────────────

    struct WinBlockInfo {
        code_begin: u64,
        code_end: u64,
        callback: FastmemCallback,
    }

    struct WinJitInfo {
        code_begin: u64,
        code_end: u64,
        code_blocks: Vec<WinBlockInfo>,
        runtime_fn_ptr: *mut RuntimeFunction,
        except_info_ptr: *mut u32,
        with_cb_rva: u32,
    }
    unsafe impl Send for WinJitInfo {}

    static WIN_SEH: Mutex<Vec<WinJitInfo>> = Mutex::new(Vec::new());

    // ── UNWIND_CODE helpers ─────────────────────────────────────────────────────

    fn push_nonvol(code_offset: u8, reg: u8) -> u16 {
        (code_offset as u16) | ((UWOP_PUSH_NONVOL as u16) << 8) | ((reg as u16) << 12)
    }
    fn alloc_large_op(code_offset: u8) -> u16 {
        (code_offset as u16) | ((UWOP_ALLOC_LARGE as u16) << 8) /* OpInfo=0 */
    }
    fn save_xmm128_op(code_offset: u8, xmm: u8) -> u16 {
        (code_offset as u16) | ((UWOP_SAVE_XMM128 as u16) << 8) | ((xmm as u16) << 12)
    }
    fn frame_entry(value: u16) -> u16 {
        value
    }

    /// Build the UNWIND_CODE array for our dispatcher prologue.
    ///
    /// Prologue sequence (must match `emit_push_callee_save_and_adjust_stack`):
    ///
    ///  push rbx    (1 byte,  offset 1)
    ///  push rsi    (1 byte,  offset 2)
    ///  push rdi    (1 byte,  offset 3)
    ///  push rbp    (1 byte,  offset 4)
    ///  push r12    (2 bytes, offset 6)
    ///  push r13    (2 bytes, offset 8)
    ///  push r14    (2 bytes, offset 10)
    ///  push r15    (2 bytes, offset 12)
    ///  sub  rsp, N (7 bytes, offset 19)
    ///  movaps [rsp+xmm_save_base+i*16], xmm6..xmm15
    ///
    /// `stack_allocation_size` is the exact amount subtracted from RSP by
    /// `emit_push_callee_save_and_adjust_stack`.
    fn build_unwind_codes(
        stack_allocation_size: usize,
        xmm_save_base: usize,
    ) -> (Vec<u16>, u8, u8) {
        let alloc_n = stack_allocation_size;
        // UWOP_ALLOC_LARGE OpInfo=0: next entry holds size / 8 as u16.
        assert!(alloc_n % 8 == 0, "alloc must be multiple of 8");
        let alloc_n8 = (alloc_n / 8) as u16;

        let mut prolog_offset = 19usize;
        let mut xmm_operations = Vec::with_capacity(10);
        for xmm in 6u8..=15 {
            let frame_offset = xmm_save_base + (xmm as usize - 6) * 16;
            let has_extended_register = xmm >= 8;
            let has_32_bit_displacement = frame_offset > i8::MAX as usize;
            prolog_offset += match (has_extended_register, has_32_bit_displacement) {
                (false, false) => 5,
                (true, false) => 6,
                (false, true) => 8,
                (true, true) => 9,
            };
            xmm_operations.push((prolog_offset as u8, xmm, frame_offset));
        }
        let prolog_size = prolog_offset as u8;

        let mut codes: Vec<u16> = Vec::with_capacity(30);
        for &(code_offset, xmm, frame_offset) in xmm_operations.iter().rev() {
            codes.push(save_xmm128_op(code_offset, xmm));
            codes.push(frame_entry((frame_offset / 16) as u16));
        }
        codes.extend_from_slice(&[
            // sub rsp, N (two-entry encoding: op then size)
            alloc_large_op(19),
            frame_entry(alloc_n8),
            // GPR pushes (CodeOffset = byte at which the instruction ends)
            push_nonvol(12, UWRC_R15),
            push_nonvol(10, UWRC_R14),
            push_nonvol(8, UWRC_R13),
            push_nonvol(6, UWRC_R12),
            push_nonvol(4, UWRC_RBP),
            push_nonvol(3, UWRC_RDI),
            push_nonvol(2, UWRC_RSI),
            push_nonvol(1, UWRC_RBX),
        ]);

        let count = codes.len() as u8;
        // CountOfCodes must be even for alignment (pad if needed).
        if codes.len() % 2 != 0 {
            codes.push(0);
        }
        (codes, count, prolog_size)
    }

    // ── Rust dispatch function (called from JIT stub) ───────────────────────────

    /// Called from the JIT-emitted `with_cb` stub when an SEH exception fires
    /// inside a registered JIT code range.
    ///
    /// `context_ptr` points to the Windows `CONTEXT` structure provided by the OS.
    /// Returns `EXCEPTION_CONTINUE_EXECUTION` (0) if the fault was handled,
    /// `EXCEPTION_CONTINUE_SEARCH` (1) otherwise.
    unsafe extern "system" fn seh_fastmem_dispatch(context_ptr: *mut u8) -> i32 {
        let rip = *(context_ptr.add(CTX_RIP_OFF) as *const u64);

        let guard = WIN_SEH.lock().unwrap();
        for jit in guard.iter() {
            for block in &jit.code_blocks {
                if rip >= block.code_begin && rip < block.code_end {
                    if let Some(FakeCall { call_rip, ret_rip }) = (block.callback)(rip) {
                        // Push ret_rip onto the guest stack (decrement Rsp, write value).
                        let rsp_ptr = context_ptr.add(CTX_RSP_OFF) as *mut u64;
                        *rsp_ptr -= 8;
                        let new_rsp = *rsp_ptr;
                        *(new_rsp as *mut u64) = ret_rip;
                        // Redirect execution to the fallback stub.
                        *(context_ptr.add(CTX_RIP_OFF) as *mut u64) = call_rip;
                        return EXCEPTION_CONTINUE_EXECUTION;
                    }
                }
            }
        }
        EXCEPTION_CONTINUE_SEARCH
    }

    // ── Code-buffer setup ────────────────────────────────────────────────────────

    /// Emit the two exception handler stubs and the UNWIND_INFO / RUNTIME_FUNCTION
    /// structures into the code buffer, then call `RtlAddFunctionTable`.
    ///
    /// Must be called after the dispatcher prelude is complete (so we know the
    /// current code size) but before the first user block is emitted.
    ///
    /// # Parameters
    /// - `code_buf_base`: base address of the code buffer (mmap'd RWX region)
    /// - `total_capacity`: total allocated size of the buffer (covers all future blocks)
    /// - `stack_allocation_size`: exact byte count subtracted from RSP
    ///
    /// # Returns
    /// The byte offset (from `code_buf_base`) of the `RUNTIME_FUNCTION` entry, so
    /// the caller can later call `RtlDeleteFunctionTable` via `unregister_all`.
    pub fn setup_seh_in_code_buffer(
        code_buf_base: *mut u8,
        total_capacity: usize,
        stack_allocation_size: usize,
        xmm_save_base: usize,
        current_size: &mut usize,
    ) {
        // ── Helpers ───────────────────────────────────────────────────────────

        let write_bytes = |offset: &mut usize, bytes: &[u8]| {
            unsafe {
                std::ptr::copy_nonoverlapping(
                    bytes.as_ptr(),
                    code_buf_base.add(*offset),
                    bytes.len(),
                );
            }
            *offset += bytes.len();
        };

        let align_to = |offset: &mut usize, align: usize| {
            let r = *offset % align;
            if r != 0 {
                *offset += align - r;
            }
        };

        // ── Stub 1: exception_handler_without_cb ─────────────────────────────
        // Returns ExceptionContinueSearch (1).
        //   mov eax, 1   → B8 01 00 00 00
        //   ret          → C3
        align_to(current_size, 16);
        let without_cb_rva = *current_size as u32;
        write_bytes(current_size, &[0xB8, 0x01, 0x00, 0x00, 0x00, 0xC3]);

        // ── Stub 2: exception_handler_with_cb ────────────────────────────────
        // Receives: RCX=EXCEPTION_RECORD*, RDX=Frame*, R8=CONTEXT*, R9=DISP_CTX*
        // We pass CONTEXT* (R8) as the single argument to `seh_fastmem_dispatch`.
        //
        // Windows x64 calling convention (the stub itself IS the handler so
        // it's entered with RCX/RDX/R8/R9 already set by the OS).
        //
        //   sub  rsp, 0x28        ; 48 83 EC 28  (4 bytes) shadow+align
        //   mov  rcx, r8          ; 4C 89 C1     (3 bytes) CONTEXT* → param
        //   mov  rax, <dispatch>  ; 48 B8 xx×8   (10 bytes)
        //   call rax              ; FF D0        (2 bytes)
        //   add  rsp, 0x28        ; 48 83 C4 28  (4 bytes)
        //   ret                   ; C3           (1 byte)
        align_to(current_size, 16);
        let with_cb_rva = *current_size as u32;

        let dispatch_addr = seh_fastmem_dispatch as usize as u64;
        let mut stub: Vec<u8> = vec![
            0x48, 0x83, 0xEC, 0x28, // sub rsp, 0x28
            0x4C, 0x89, 0xC1, // mov rcx, r8
            0x48, 0xB8, // mov rax, imm64 (prefix)
        ];
        stub.extend_from_slice(&dispatch_addr.to_le_bytes());
        stub.extend_from_slice(&[
            0xFF, 0xD0, // call rax
            0x48, 0x83, 0xC4, 0x28, // add rsp, 0x28
            0xC3, // ret
        ]);
        write_bytes(current_size, &stub);

        // ── UNWIND_INFO ───────────────────────────────────────────────────────
        align_to(current_size, 4);
        let unwind_info_rva = *current_size as u32;

        let (codes, count_codes, prolog_size) =
            build_unwind_codes(stack_allocation_size, xmm_save_base);

        // UNWIND_INFO header (4 bytes):
        //   byte 0: Version(3 bits)=1 | Flags(5 bits)=UNW_FLAG_EHANDLER
        //   byte 1: SizeOfProlog
        //   byte 2: CountOfCodes
        //   byte 3: FrameRegister(4 bits)=0 | FrameOffset(4 bits)=0
        let header = [
            1 | (UNW_FLAG_EHANDLER << 3), // Version=1, Flags=UNW_FLAG_EHANDLER
            prolog_size,
            count_codes,
            0u8, // no frame register
        ];
        write_bytes(current_size, &header);

        // UNWIND_CODE array (each entry is a u16, little-endian).
        for code in &codes {
            write_bytes(current_size, &code.to_le_bytes());
        }

        // UNW_EXCEPTION_INFO: ULONG ExceptionHandler (RVA of handler stub).
        // We start with without_cb; SetFastmemCallback updates it to with_cb.
        align_to(current_size, 4);
        let except_info_offset = *current_size; // we'll need to patch this later
        write_bytes(current_size, &without_cb_rva.to_le_bytes());

        // ── RUNTIME_FUNCTION ─────────────────────────────────────────────────
        align_to(current_size, 4);
        let rfunc_offset = *current_size;
        // BeginAddress = 0 (start of code buffer)
        // EndAddress   = total_capacity (covers all future compiled blocks)
        // UnwindData   = RVA of UNWIND_INFO
        let begin_addr: u32 = 0;
        let end_addr: u32 = total_capacity as u32;
        write_bytes(current_size, &begin_addr.to_le_bytes());
        write_bytes(current_size, &end_addr.to_le_bytes());
        write_bytes(current_size, &unwind_info_rva.to_le_bytes());

        // ── Register with Windows ────────────────────────────────────────────
        let rfunc_ptr = unsafe { code_buf_base.add(rfunc_offset) as *mut RuntimeFunction };
        unsafe { RtlAddFunctionTable(rfunc_ptr, 1, code_buf_base as u64) };

        // Store this code buffer's state for later callback updates and cleanup.
        let mut guard = WIN_SEH.lock().unwrap();
        guard.push(WinJitInfo {
            code_begin: code_buf_base as u64,
            code_end: code_buf_base as u64 + total_capacity as u64,
            code_blocks: Vec::new(),
            runtime_fn_ptr: rfunc_ptr,
            except_info_ptr: unsafe { code_buf_base.add(except_info_offset) as *mut u32 },
            with_cb_rva,
        });
    }

    // ── Public API ────────────────────────────────────────────────────────────

    /// Register a JIT code range with the SEH dispatcher and activate the
    /// with_cb handler stub.
    pub fn register_code_block_impl(
        code_begin: *const u8,
        code_end: *const u8,
        callback: FastmemCallback,
    ) {
        let mut guard = WIN_SEH.lock().unwrap();
        let address = code_begin as u64;
        let Some(jit) = guard
            .iter_mut()
            .find(|jit| address >= jit.code_begin && address < jit.code_end)
        else {
            return;
        };
        jit.code_blocks.push(WinBlockInfo {
            code_begin: code_begin as u64,
            code_end: code_end as u64,
            callback,
        });

        // Switch the UNWIND_INFO exception handler to the with_cb stub.
        if !jit.except_info_ptr.is_null() {
            unsafe { jit.except_info_ptr.write(jit.with_cb_rva) };
        }
    }

    pub fn unregister_code_block_impl(code_begin: *const u8) {
        let mut guard = WIN_SEH.lock().unwrap();
        if let Some(index) = guard
            .iter()
            .position(|jit| jit.code_begin == code_begin as u64)
        {
            let jit = guard.remove(index);
            if !jit.runtime_fn_ptr.is_null() {
                unsafe { RtlDeleteFunctionTable(jit.runtime_fn_ptr) };
            }
        }
    }

    pub fn unregister_all() {
        let mut guard = WIN_SEH.lock().unwrap();
        for jit in guard.drain(..) {
            if !jit.runtime_fn_ptr.is_null() {
                unsafe { RtlDeleteFunctionTable(jit.runtime_fn_ptr) };
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[repr(C, align(16))]
        struct ContextBuffer([u8; 1232]);

        fn code_offset(code: u16) -> u8 {
            code as u8
        }

        unsafe fn write_context_u64(context: &mut ContextBuffer, offset: usize, value: u64) {
            (context.0.as_mut_ptr().add(offset) as *mut u64).write_unaligned(value);
        }

        unsafe fn read_context_u64(context: &ContextBuffer, offset: usize) -> u64 {
            (context.0.as_ptr().add(offset) as *const u64).read_unaligned()
        }

        #[test]
        fn unwind_codes_match_windows_prologue_order_and_allocation() {
            let stack_allocation_size =
                crate::backend::x64::block_of_code::stack_frame_allocation_size(
                    core::mem::size_of::<crate::backend::x64::stack_layout::StackLayout>(),
                );
            let xmm_save_base =
                crate::backend::x64::block_of_code::xmm_save_base(core::mem::size_of::<
                    crate::backend::x64::stack_layout::StackLayout,
                >());
            let (codes, count, prolog_size) =
                build_unwind_codes(stack_allocation_size, xmm_save_base);

            assert_eq!(count, 30);
            assert_eq!(codes.len(), 30);
            assert_eq!(prolog_size, 107);

            let operation_indices = [
                0usize, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 23, 24, 25, 26, 27, 28, 29,
            ];
            let operation_offsets: Vec<u8> = operation_indices
                .iter()
                .map(|index| code_offset(codes[*index]))
                .collect();
            assert_eq!(
                operation_offsets,
                vec![107, 98, 89, 80, 71, 62, 53, 44, 35, 27, 19, 12, 10, 8, 6, 4, 3, 2, 1]
            );
            assert_eq!(codes[21], (stack_allocation_size / 8) as u16);
        }

        #[test]
        fn registered_unwind_info_restores_dispatcher_stack_pointer() {
            let mut code =
                crate::backend::x64::block_of_code::BlockOfCode::with_size(4096).unwrap();
            let frame_size = core::mem::size_of::<crate::backend::x64::stack_layout::StackLayout>();
            let allocation =
                crate::backend::x64::block_of_code::stack_frame_allocation_size(frame_size);
            code.emit_push_callee_save_and_adjust_stack(frame_size)
                .unwrap();
            code.prelude_complete();

            let guard = WIN_SEH.lock().unwrap();
            let jit = guard
                .iter()
                .find(|jit| jit.code_begin == code.code_base_ptr() as u64)
                .unwrap();

            let mut stack = [0u64; 512];
            let caller_rsp = unsafe { stack.as_mut_ptr().add(384) } as u64;
            let return_rip = 0x0123_4567_89AB_CDEF;
            unsafe {
                (caller_rsp as *mut u64).write(return_rip);
            }

            let saved_registers = [
                0x0B0B_u64, 0x0606, 0x0707, 0x0505, 0x1212, 0x1313, 0x1414, 0x1515,
            ];
            for (index, value) in saved_registers.into_iter().enumerate() {
                unsafe {
                    ((caller_rsp - ((index + 1) * 8) as u64) as *mut u64).write(value);
                }
            }

            let mut context = ContextBuffer([0; 1232]);
            let dispatcher_rsp = caller_rsp - (saved_registers.len() * 8 + allocation) as u64;
            unsafe {
                write_context_u64(&mut context, CTX_RSP_OFF, dispatcher_rsp);
                write_context_u64(&mut context, CTX_RIP_OFF, jit.code_begin + 128);
            }

            let mut handler_data = core::ptr::null_mut();
            let mut establisher_frame = 0u64;
            unsafe {
                RtlVirtualUnwind(
                    0,
                    jit.code_begin,
                    jit.code_begin + 128,
                    jit.runtime_fn_ptr,
                    context.0.as_mut_ptr(),
                    &mut handler_data,
                    &mut establisher_frame,
                    core::ptr::null_mut(),
                );
            }

            assert_eq!(
                unsafe { read_context_u64(&context, CTX_RSP_OFF) },
                caller_rsp + 8
            );
            assert_eq!(
                unsafe { read_context_u64(&context, CTX_RIP_OFF) },
                return_rip
            );
            for (offset, expected) in [
                (CTX_RBX_OFF, saved_registers[0]),
                (CTX_RSI_OFF, saved_registers[1]),
                (CTX_RDI_OFF, saved_registers[2]),
                (CTX_RBP_OFF, saved_registers[3]),
                (CTX_R12_OFF, saved_registers[4]),
                (CTX_R13_OFF, saved_registers[5]),
                (CTX_R14_OFF, saved_registers[6]),
                (CTX_R15_OFF, saved_registers[7]),
            ] {
                assert_eq!(unsafe { read_context_u64(&context, offset) }, expected);
            }
        }
    }
}

// ── Windows public surface ─────────────────────────────────────────────────────

#[cfg(all(target_os = "windows", target_arch = "x86_64"))]
pub fn register_code_block(code_begin: *const u8, code_end: *const u8, callback: FastmemCallback) {
    windows_seh::register_code_block_impl(code_begin, code_end, callback);
}

#[cfg(all(target_os = "windows", target_arch = "x86_64"))]
pub fn unregister_code_block(code_begin: *const u8) {
    windows_seh::unregister_code_block_impl(code_begin);
}

/// Called from `BlockOfCode` after the dispatcher prelude is complete to emit
/// SEH stubs + UNWIND_INFO + RUNTIME_FUNCTION into the code buffer.
///
/// `current_size` is a mutable reference to the assembler's current byte count;
/// it is advanced as data is written.
#[cfg(all(target_os = "windows", target_arch = "x86_64"))]
pub fn setup_seh_in_code_buffer(
    code_buf_base: *mut u8,
    total_capacity: usize,
    stack_allocation_size: usize,
    xmm_save_base: usize,
    current_size: &mut usize,
) {
    windows_seh::setup_seh_in_code_buffer(
        code_buf_base,
        total_capacity,
        stack_allocation_size,
        xmm_save_base,
        current_size,
    );
}

#[cfg(all(target_os = "windows", target_arch = "x86_64"))]
pub fn register_thread_signal_stack() {} // no-op on Windows

/// A per-emitter fastmem patch info table.
///
/// Records the RIP of each fastmem memory instruction and its fallback info.
/// Used by the SIGSEGV handler to redirect faulting instructions.
pub struct FastmemPatchTable {
    patches: HashMap<u64, FastmemPatchInfo>,
    /// Fast rejection for the overwhelmingly common no-fault path.
    ///
    /// The signal handler sets this together with the per-entry flag. The JIT
    /// checks it before scanning the patch table after generated code returns.
    pending_recompiles: AtomicBool,
}

impl FastmemPatchTable {
    pub fn new() -> Self {
        Self {
            patches: HashMap::new(),
            pending_recompiles: AtomicBool::new(false),
        }
    }

    /// Record a fastmem instruction at `rip` with its fallback info.
    pub fn add(&mut self, rip: u64, info: FastmemPatchInfo) {
        self.patches.insert(rip, info);
    }

    /// Look up a faulting RIP and return the FakeCall to redirect to.
    pub fn lookup(&self, rip: u64) -> Option<FakeCall> {
        self.patches.get(&rip).map(|info| FakeCall {
            call_rip: info.callback,
            ret_rip: info.resume_rip,
        })
    }

    /// Look up a fault and record upstream's recompile request without
    /// allocating or mutating JIT caches from an exception handler.
    ///
    /// Each emitter executes on one host thread at a time. The owning JIT
    /// drains this fixed-capacity queue immediately after generated code
    /// returns, then updates `do_not_fastmem` and invalidates the blocks.
    pub fn lookup_and_record_recompile(&self, rip: u64) -> Option<FakeCall> {
        let info = self.patches.get(&rip)?;
        if info.recompile && info.marker.is_some() {
            info.pending_recompile.store(true, Ordering::Release);
            self.pending_recompiles.store(true, Ordering::Release);
        }
        Some(FakeCall {
            call_rip: info.callback,
            ret_rip: info.resume_rip,
        })
    }

    /// Drain recompile requests after generated code has stopped executing.
    pub fn take_pending_recompiles(&self) -> Vec<DoNotFastmemMarker> {
        if !self.pending_recompiles.swap(false, Ordering::AcqRel) {
            return Vec::new();
        }

        self.patches
            .values()
            .filter_map(|info| {
                info.pending_recompile
                    .swap(false, Ordering::AcqRel)
                    .then_some(info.marker)
                    .flatten()
            })
            .collect()
    }

    /// Clear all patch info (called on cache clear).
    pub fn clear(&mut self) {
        self.patches.clear();
        self.pending_recompiles.store(false, Ordering::Relaxed);
    }

    pub fn len(&self) -> usize {
        self.patches.len()
    }
}

#[cfg(test)]
mod fastmem_patch_table_tests {
    use super::*;

    #[test]
    fn fastmem_support_matches_the_compiled_exception_handler() {
        assert_eq!(
            supports_fastmem(),
            cfg!(any(
                all(target_os = "linux", target_arch = "x86_64"),
                all(target_os = "windows", target_arch = "x86_64")
            ))
        );
    }

    #[test]
    fn recompile_lookup_records_marker() {
        let marker = (LocationDescriptor::new(0x1234), 7);
        let mut table = FastmemPatchTable::new();
        table.add(
            0x1000,
            FastmemPatchInfo::new(0x2000, 0x3000, Some(marker), true),
        );

        assert_eq!(
            table.lookup_and_record_recompile(0x1000),
            Some(FakeCall {
                call_rip: 0x3000,
                ret_rip: 0x2000,
            })
        );
        assert_eq!(table.take_pending_recompiles(), vec![marker]);
        assert!(table.take_pending_recompiles().is_empty());
    }

    #[test]
    fn non_recompiling_lookup_does_not_record_marker() {
        let mut table = FastmemPatchTable::new();
        table.add(
            0x1000,
            FastmemPatchInfo::new(
                0x2000,
                0x3000,
                Some((LocationDescriptor::new(0x1234), 7)),
                false,
            ),
        );

        assert!(table.lookup_and_record_recompile(0x1000).is_some());
        assert!(table.take_pending_recompiles().is_empty());
    }

    #[test]
    fn duplicate_faults_queue_one_recompile() {
        let marker = (LocationDescriptor::new(0x1234), 7);
        let mut table = FastmemPatchTable::new();
        table.add(
            0x1000,
            FastmemPatchInfo::new(0x2000, 0x3000, Some(marker), true),
        );

        for _ in 0..512 {
            assert!(table.lookup_and_record_recompile(0x1000).is_some());
        }
        assert_eq!(table.take_pending_recompiles(), vec![marker]);
    }

    #[test]
    fn no_fault_does_not_enter_patch_table_scan() {
        let mut table = FastmemPatchTable::new();
        for index in 0..1024 {
            table.add(
                0x1000 + index,
                FastmemPatchInfo::new(
                    0x2000 + index,
                    0x3000 + index,
                    Some((LocationDescriptor::new(0x4000 + index), index as u32)),
                    true,
                ),
            );
        }

        assert!(!table.pending_recompiles.load(Ordering::Relaxed));
        assert!(table.take_pending_recompiles().is_empty());
        assert!(!table.pending_recompiles.load(Ordering::Relaxed));
    }
}
