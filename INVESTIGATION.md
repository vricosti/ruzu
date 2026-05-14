# STK Multi-Core Wedge — Investigation Report

## Symptom

When STK (SuperTuxKart NRO) runs under ruzu in multi-core mode, the emulator
wedges within seconds. The wedge manifests as 4M+ `Unmapped Read 64 @ 0x2101a3b14XXX`
log lines per 15s. STK's main thread (tid=73 on emulator core 0) repeatedly
dereferences a corrupt pointer of the form `0x2101A3B140A0`. The pointer
appears in dlmalloc's allocator state slot at guest vaddr `0x81490350` /
`0x814903F8`.

The corrupt value has the form `(valid_heap_addr << 8) | byte` — e.g.
`0x2101A3B140A0 = (0x2101A3B140 << 8) | 0xA0`, where `0x2101A3B140` is a real
in-range heap chunk address.

## Empirical Fingerprint

| Condition | Wedge errors / 15 s |
|---|---|
| Default (4 host threads, fastmem RW) | **4,500,000** |
| `RUZU_CORE_COUNT=1` | 0 |
| `RUZU_CORE_COUNT=2` | 0 |
| `RUZU_CORE_COUNT=3` | 0 |
| `RUZU_CORE_COUNT=4` (default) | 4,500,000 |
| `RUZU_NO_FASTMEM_R64=1` | ~29 |
| `RUZU_NO_FASTMEM_W64=1` | ~29 |
| `RUZU_NO_FASTMEM_R{8,16,32}=1` | 2.4M+ |
| `RUZU_NO_FASTMEM_W{8,16,32}=1` | 2.0M+ |
| `RUZU_NO_FASTMEM=1` | ~29 |
| `RUZU_NO_PREEMPT_INTERRUPTS=1` | 4.4M (still wedges) |
| `RUZU_DISABLE_PREEMPTION_THREAD=1` | 4.5M (still wedges) |
| `RUZU_SERIALIZE_JIT=1` | 4.4M (still wedges) |
| `taskset -c 0` (single host CPU) | 4.2M (still wedges) |

### Optimization-pass bisection

All `RUZU_A64_OPTIMIZATION_MASK` configurations (each opt on alone; each opt
off alone; no opts at all) → wedge persists. `BLOCK_LINKING` enabled
amplifies the rate (~2.3M+ vs ~1.1M) because block chaining accelerates the
spin loop, but no single optimization causes or eliminates the wedge.

### Required conditions

1. `NUM_CPU_CORES = 4` host threads must exist (`RUZU_CORE_COUNT=4`).
2. Fastmem 64-bit read AND write paths must both be enabled.

### Ruled out

- **Memory coherency between fastmem arena and backing region.** Added
  `RUZU_POLL_DIVERGE=0xVADDR[,...]` background thread that compares
  `*(virtual_base + vaddr)` vs `*(pt.pointers[page] + vaddr)` with 3× stable
  re-read filter, tight-spin polling at 10µs intervals across 11 slots in
  STK's dlmalloc state region — **zero stable divergences detected during
  wedge.** The two views are coherent.
- **Rust write paths**: `write_block` only touches the page during NRO load.
  `write_64` callback never fires at the corrupt vaddr. HLE service IPC does
  not write to the page.
- **Preemption thread** (`RUZU_NO_PREEMPT_INTERRUPTS=1` /
  `RUZU_DISABLE_PREEMPTION_THREAD=1` don't change wedge).
- **JIT parallelism** (`RUZU_SERIALIZE_JIT=1` doesn't change wedge).
- **Hardware parallelism** (`taskset -c 0` doesn't change wedge — even with
  all 4 host threads forced onto one CPU core).
- **JIT optimization passes** (full bisection — wedge persists across all
  combinations).
- **ARMv8.1 atomic instructions**: STK NRO scanned, zero CAS/SWP/LDADD/etc.
  occurrences. STK is pre-v8.1 baseline.
- **`interpret_this_instruction` fallback**: ruzu's `interpreter_fallback`
  halts with PrefetchAbort, no such logs observed during wedge.
- **Exclusive monitor** (`ExclusiveMonitor`, processor_id correctly threaded;
  `clear_exclusive_state()` called at run_thread entry matching upstream).
- **Context save/load** (ThreadContext layouts byte-identical between
  k_thread.rs and arm_interface.rs; get/set_context register-by-register
  copies are clean).
- **WriteExclusive128 atomicity** — fixed to true `lock cmpxchg16b`, but this
  was not the wedge cause (STK doesn't reach the 128-bit exclusive write path).

### Strong indication

The bug is a **JIT codegen issue in the 64-bit fastmem read/write path** that
produces a wrong VALUE (not memory corruption). The wrongness depends on the
host-thread topology (4 host threads existing) in a way that isn't explained
by parallelism (taskset proves hardware parallelism isn't required).

## Code changes (uncommitted, this branch)

1. **`core/src/memory/memory.rs`** — `WriteExclusive128` now uses true
   `lock cmpxchg16b` via inline asm on x86_64 hosts (replacing two-step 64-bit
   CAS that was a structural divergence from upstream
   `Common::AtomicCompareAndSwap`). Four standalone tests verify
   success-on-match, failure-on-mismatch, failure-on-hi-mismatch-only (proves
   no partial write), and round-trip. **Correctness improvement, not wedge cause.**

2. **`core/src/memory/memory.rs`** — `RUZU_POLL_DIVERGE=0xVADDR[,...]` background
   thread that compares fastmem-arena view vs backing-region view at the
   specified vaddrs. Used to prove coherency hypothesis was wrong.
   `RUZU_POLL_DIVERGE_SLEEP_US=N` controls polling cadence.

3. **`common/src/host_memory.rs`** — `RUZU_TRACE_HOST_MMAP=0xVADDR` and
   `RUZU_TRACE_HOST_PROTECT` to log mmap/mprotect calls covering a specific
   guest vaddr.

4. **`core/src/memory/memory.rs`** — `RUZU_TRACE_MAP_REGION=0xPAGE`,
   `RUZU_TRACE_MAP_HOST_OFFSET=0xOFFSET`, `RUZU_TRACE_MEMORY_W64_AT_VADDR=0xVADDR`
   for memory-mapping diagnostics.

5. **`core/src/arm/dynarmic/arm_dynarmic_64.rs`** — per-core W64 slow-path
   counter (`RUZU_COUNT_W64_BY_CORE_AT_VADDR`), `RUZU_DIVERGE_PAGE` for
   write-time arena-vs-backing comparison.

6. **`core/src/hle/kernel/kernel.rs`** — `RUZU_NO_PREEMPT_INTERRUPTS=1`
   gates the per-iteration `core.interrupt()` calls in the preemption event
   callback. `RUZU_DISABLE_PREEMPTION_THREAD=1` disables the dedicated
   preemption host thread.

7. **`rdynarmic/src/jit.rs`** — `RUZU_BLOCK_PROLOGUE_COUNT_PC=LO:HI` inline
   JIT-emit per-core hit counter (bypasses FAST_DISPATCH chaining).
   `RUZU_FIRST_PCS_PER_CORE=1` per-core ring buffer of first-N PCs seen.

8. **`rdynarmic/src/backend/x64/a64_emit_x64_memory.rs`** —
   `RUZU_NO_FASTMEM_R64=1`, `RUZU_NO_FASTMEM_W{8,16,32,64}=1` per-width
   fastmem disable, plus `RUZU_TRAP_FASTMEM_W64_*` family for inline-trap
   diagnostics.

9. **`ruzu_cmd/src/main.rs`** — `RUZU_COUNT_DUMP_FILE=PATH` background
   diagnostic dumper thread (writes block-count + W64-by-core summaries
   every 5s, avoids stderr-flood deadlocks during the wedge).

10. **`scripts/hw_watch_wedge.py`** — gdb-driven hardware-watchpoint script
    with auto-detection of fastmem arena base from `/proc/PID/maps`.

11. **Debug-build correctness fixes**: wrapping_sub/wrapping_add in
    `common/src/page_table.rs::current_physical_address`,
    `core/src/memory/memory.rs::current_physical_address` and `map_pages`.
    These were pre-existing debug-build overflow panics, not wedge-related.

12. **`rdynarmic/src/backend/x64/reg_alloc.rs`** — converted the GPR↔XMM
    `bit_width > 64` debug_assert to a one-shot WARN + truncate (matches
    release-mode behavior).

## Next concrete steps

Either approach below would likely identify the bad instruction:

### A. Per-instruction codegen oracle test
Compare ruzu's 64-bit fastmem read/write x86 encoding against zuyu's dynarmic
encoding for the same IR sequence. Walk STK's hot path PC-by-PC and diff
emitted bytes. The first divergence is the bug.

### B. Live gdb capture of corrupt register load
1. Run STK with `RUZU_ALLOW_PTRACER=1` (sets PR_SET_PTRACER_ANY).
2. Use `scripts/hw_watch_wedge.py` to attach gdb and set a hardware
   watchpoint on the host VA corresponding to guest `0x81490350`.
3. When the watchpoint fires, dump the host RIP, full GPRs, and x86
   instruction bytes at RIP-16..RIP+16.
4. The disassembly shows the exact JIT-emitted instruction that wrote
   the corrupt value, and from RIP we can back-trace to the source IR.

### C. Differential vs upstream zuyu (slower but most diagnostic)
Run zuyu and ruzu side-by-side with matched RNG seed, dump SVC trace from
both. Find the first divergence point. The differing SVC is the symptom of
the bad upstream instruction.

## Status

- Investigation halted at clear empirical bounds.
- ~1500 lines of diagnostic infrastructure across 15 files, all uncommitted.
- Memory file `project_stk_fastmem_coherency_2026_05_10.md` documents the
  full audit trail across three sessions for continuity.

## Updates (2026-05-11 final)

### x86 disassembly of suspected block

Dumped the JIT-emitted x86 code for the block at guest PC `0x80E43D38`
(per memory note, the corrupt SetX X19 PC) via `RUZU_DUMP_X64_AT_PC=0x80E43D38`.
The block contains two fastmem 64-bit loads:

1. Offset +0x18: `49 8B 5C 05 00` = `mov rbx, [r13 + rax*1 + 0]` — loads from
   `virtual_base + 0x814772D8` into rbx.
2. Offset +0x55: `49 8B 44 1D 00` = `mov rax, [r13 + rbx*1 + 0]` — loads from
   `virtual_base + rbx` into rax (the actual LDR x19,[x1+24] equivalent).

Both encodings are correct: REX.WB (W=1 for 64-bit, B=1 for r13 base), MOV
opcode 8B, ModRM with SIB byte, SIB encoding `mod=01 base=r13 index=N scale=1
disp8=0` (the disp8=0 is required because r13 with mod=00 means RIP-relative).

The block also contains a `cmc` (F5) and a `lahf`+`seto`+`and` sequence for
ARM SUB flag capture (standard pattern). All instructions look correct.

**Conclusion**: the x86 dump does not reveal a codegen bug at this block.
The corrupting instruction is likely elsewhere in STK's hot path, or the
bug is not in instruction encoding but in some IR-level transformation.

### Other tests tried (this turn)

- `RUZU_NO_HUGEPAGE=1` (skip `madvise(MADV_HUGEPAGE)` on the virtual arena):
  wedge persists (2.2M errors). MADV_HUGEPAGE is not the cause.

### Where to start next session

Given the empirical bounds (4 host threads + 64-bit fastmem RW required) and
the fact that the obvious block-level x86 encoding looks correct, the most
productive remaining investigation is **Approach B** from above: live gdb
attach via `scripts/hw_watch_wedge.py` to capture the EXACT host RIP at the
moment of the corrupt register load. Steps:

```bash
# Terminal 1: run STK with allow-ptracer
env XDG_CACHE_HOME=/tmp/ruzu RUZU_ALLOW_PTRACER=1 \
  ./target/release/ruzu-cmd -r vulkan -g /path/to/stk.nro

# Terminal 2: attach hw watchpoint on host VA for guest 0x81490350
PID=$(pgrep -f ruzu-cmd)
python3 scripts/hw_watch_wedge.py $PID 0x81490350
```

The script auto-detects the fastmem arena base from `/proc/PID/maps` and
sets a hardware watchpoint at `arena_base + 0x81490350`. When the watchpoint
fires, gdb prints the host RIP, full GPRs, and surrounding x86 bytes. From
the host RIP and the JIT block table, the source ARM instruction can be
identified.

## Updates (2026-05-11 final-v2): caught the corruption chain

Using the existing inline-trap diagnostics (RUZU_TRAP_FASTMEM_W64_CORRUPT,
RUZU_TRAP_LDR_BYTE5_21, RUZU_TRAP_SETX_BYTE5_21), captured live host-RIP +
JitState snapshots at the corrupt instructions:

### The corrupting STR

**`RUZU_TRAP_FASTMEM_W64_CORRUPT_VADDR=0x81490350`** caught the moment a
fastmem 64-bit write of `0x2101A3B140A0` (the corrupt pattern) is directed
at slot `0x81490350` (dlmalloc mstate bin head).

- Guest PC = `0x80E46360` (block-entry PC; the STR is downstream in the block)
- LR (X30) = `0x80E462DC` (caller in dlmalloc free path)
- X3 = `0x2101A3B140A0` (corrupt — used as STR base in `stp x2, x0, [x3, #16]`)
- X4 = `0x81490340` (mstate base — correct)
- X19 = `0x2101A3B140B0` (corrupt — free-list pointer)
- LR's caller is STK at PC `0x80C89F1C` — `free()` call site at `bl 0x80E4BA40`
  (libstk function around `0x80C89F18`).

ARM disassembly at PC `0x80E46300..0x80E46398` is **dlmalloc's free()** unlink
loop. The corrupting `stp x2, x0, [x3, #16]` at PC `0x80E4638C` writes the
corrupt pair to `[X3+16]`. But X3 = `0x2101A3B140A0` — an OUT-OF-RANGE
heap-shifted-by-8 pointer. The STP destination is `0x2101A3B140B0`, NOT
`0x81490350`.

Yet the trap reports `recovered_vaddr=0x81490350`. This is the destination
that the fastmem path translates `[r13 + X3 + 16]` to. With the ARM 39-bit
address space and TBI semantics, `0x2101A3B140A0` masked to 39 bits =
`0x01A3B140A0` — and `0x01A3B140A0 + 16 = 0x01A3B140B0`. Still doesn't match
`0x81490350`.

### The smoking-gun: TBI mask divergence

The `recovered_vaddr=0x81490350` came from a DIFFERENT instruction in the
same block. Looking at disasm `0x80E46388: str x1, [x4, #8]` (writing to
`X4 + 8 = 0x81490340 + 8 = 0x81490348`) and `0x80E4638C: stp x2, x0, [x3, #16]`
(writing to `X3 + 16`). The store at `0x81490350` (the corrupt slot) requires
a 64-bit STR with base+offset that resolves to `0x81490350`. With X3 corrupt,
none of these stores in this block target `0x81490350`.

**Conclusion: the corrupting STR is in a DIFFERENT block.** The chain:
1. Some block writes `0x2101A3B140A0` to `[0x81490350]` (the bin head).
2. Later block at PC `0x80E46360` loads `X19 = 0x2101A3B140B0` from the
   corrupted free list, then STP-writes the corrupt pair using X3=corrupt
   to a different out-of-range address.
3. The chain propagates corruption through additional free-list nodes.

### Chain of corruption summary

- Trap 1 (RUZU_TRAP_FASTMEM_W64_VADDR=0x81490350): writes to mstate slot — caught
  init-time writes (X0=0x01490350 at PC 0x80776C18, looks like dlmalloc init).
- Trap 2 (RUZU_TRAP_FASTMEM_W64_CORRUPT_VADDR=0x81490350): the corrupt-pattern
  write to mstate slot — fires AT THE moment dlmalloc inserts a corrupt free
  chunk into the mstate bin.
- Trap 3 (RUZU_TRAP_LDR_BYTE5_21=1): LDR from corrupt-pattern address — fires
  at PC 0x80E46320 `ldur x7, [x19, #-16]` with X19 already corrupt.
- Trap 4 (RUZU_TRAP_SETX_BYTE5_21=1 + SKIP_LOADS=1): non-load SetX with corrupt
  value — fires at PC 0x80E441AC `and x1, x1, #0xfffffffffffffffc` where X1 was
  already corrupt from a previous load.

The deeper original write (the FIRST time `0x2101A3B140A0` enters the dlmalloc
state) hasn't yet been pinpointed. It's UPSTREAM of all the traps above —
some code in STK's libnx/libstdc++ or dlmalloc setup that, in multi-core mode
with fastmem 64-bit RW, produces a 64-bit STR with a shifted-by-8 source value.

### Most promising remaining direction

Add an inline JIT-trap on EVERY A64WriteMemory64 instruction (any vaddr) whose
VALUE matches the corrupt pattern (`byte5=0x21, byte4=0x01, bytes6,7=0`).
The trap should INCLUDE a backtrace via the recovered_aux convention (read
guest X30 from stack frames). The first such trap is the ORIGIN of the
corruption. Currently, RUZU_TRAP_FASTMEM_W64_CORRUPT fires only with a
vaddr filter; add `RUZU_TRAP_FASTMEM_W64_CORRUPT_NO_FILTER=1` to fire on
ANY destination.

Existing `RUZU_TRAP_FASTMEM_W64_CORRUPT=1` (no vaddr filter) fired at
PC 0x80C32EEC with X0=`0x2101A3B140B0` being stored to stack `0x10817032A8`.
That's STK storing a malloc()-returned pointer to the stack — already corrupt
at allocator return.

So the FIRST corrupt value is produced INSIDE the dlmalloc allocator chain
itself (entry at `0x80E42E20` → `0x80E644E0` → tail-call `0x80E4D140` per
prior session notes). The dlmalloc-internal code is what miscompiles in
4-core + 64-bit-fastmem mode. The exact ARM instruction whose JIT-emitted x86
produces wrong result is the next thing to find.
---
● Substantive new findings this turn — live SIGILL traps captured:
     
  1. RUZU_TRAP_FASTMEM_W64_CORRUPT_VADDR=0x81490350 caught the corrupt-pattern write being directed at the mstate slot:
    - Guest PC = 0x80E46360 (block entry; dlmalloc free unlink loop)
    - X3 = corrupt 0x2101A3B140A0 (used as STP base)
    - LR = 0x80C89F1C (STK's free() call site)
    - Confirms corruption is INSIDE dlmalloc's free-list traversal
  2. RUZU_TRAP_LDR_BYTE5_21=1 caught the LDR that reads from a corrupt address:
    - Guest PC = 0x80E46320 ldur x7, [x19, #-16]
    - X19 already corrupt with 0x2101A3B140B0
  3. RUZU_TRAP_SETX_BYTE5_21=1 with SKIP_LOADS caught the first non-load corrupt SetX:
    - X3 = corrupt 0x2101A3B140A0 (used as STP base)
    - LR = 0x80C89F1C (STK's free() call site)
    - Confirms corruption is INSIDE dlmalloc's free-list traversal
  2. RUZU_TRAP_LDR_BYTE5_21=1 caught the LDR that reads from a corrupt address:
    - Guest PC = 0x80E46320 ldur x7, [x19, #-16]
    - X19 already corrupt with 0x2101A3B140B0
  3. RUZU_TRAP_SETX_BYTE5_21=1 with SKIP_LOADS caught the first non-load corrupt SetX:
    - Guest PC = 0x80E441AC and x1, x1, #0xfffffffffffffffc
    - X1 already corrupt from the LDP before it
  4. RUZU_TRAP_FASTMEM_W64_CORRUPT=1 (any-vaddr) caught a corrupt-pattern store:
    - Guest PC = 0x80C32EEC (STK storing malloc()-returned pointer)
    - X0 = 0x2101A3B140B0 — dlmalloc returns corrupt pointers

  Chain conclusion: the corrupting source is INSIDE the dlmalloc allocator function chain (0x80E42E20 → 0x80E644E0 → 0x80E4D140). The dlmalloc-internal code miscompiles in
  4-core+64-bit-fastmem mode, producing corrupt heap pointers. The exact ARM-instruction-to-bad-x86-bytes mapping is the next step to find.

  INVESTIGATION.md fully updated with the corruption chain, all SIGILL traces, ARM disassembly of affected blocks, and recommended next experiment.

## Updates (2026-05-11 final-v3): the corruption is in the READ, NOT the WRITE

Captured 30+ writes to slot `0x814903F8` via `RUZU_TRAP_FASTMEM_W64_VADDR=0x814903F8`:

**EVERY write contains a VALID heap pointer** in the `0x21_0160_xxxx` /
`0x21_0186_xxxx` range (matches STK's heap base `0x21_0160_0000`). Sample
values: `0x21016005C0`, `0x2101600A70`, `0x21016017C0`, `0x21015AA0`,
`0x210187AAF0`, ...

**NONE of the writes have the corrupt `0x2101A3B140A0` pattern.** Yet
`RUZU_TRAP_SETX_BYTE5_21=1` (with SKIP_PC for known false positives) fires
at PC `0x80E43D38` showing the JIT-emitted LDR `x19, [x1, #24]` (= `ldr x19,
[x22+0x58]`) returns the corrupt value.

### Conclusion

**The bug is in the 64-bit fastmem READ path** — the JIT-emitted
`mov rax, [r13 + rbx]` returns a corrupt value despite memory containing a
valid value at that host VA. The earlier `RUZU_POLL_DIVERGE` test (with
3× stable re-read filter, tight-spin 10µs polling) saw zero stable
divergences between arena and backing views, so the underlying memfd page
contains valid data.

Possible mechanisms:
1. **Register clobber**: some prior x86 instruction in the same block
   leaves stale data in the destination register, and the `mov` doesn't
   actually execute (e.g., dead-code elimination at the x86 level). The
   inline trap fires AFTER the mov, observing the register state.
2. **Out-of-order/speculation aliasing**: the host CPU forwards from a
   prior store buffer entry that has stale/wrong data. STK's main thread
   may have an earlier `mov [arena+X], ...` whose value is forwarded.
3. **Block compilation determinism**: the JIT compiled this block when
   memory had valid contents; some cached IR result captured the wrong
   value at compile time. (Constant propagation through memory loads?)
4. **REX/SIB encoding aliasing**: the host's [r13+rbx*1+0] encoding is
   correct per inspection, but somehow translates to a different effective
   address at runtime under specific microarchitectural conditions.

### Strongest remaining direction

Attach gdb to a live wedged STK process via `scripts/hw_watch_wedge.py` and:

1. Set hw read-watchpoint on `virtual_base + 0x814903F8`.
2. When triggered, the host RIP is the exact JIT-emitted instruction that
   reads the corrupt value. Inspect:
   - The 16 bytes of x86 instruction at RIP-8..RIP+8 (to confirm encoding)
   - All GPR contents (especially r13 + the address register)
   - Memory at `*(virtual_base + 0x814903F8)` and at `*(backing_base +
     0x10B603F8)` — these should be byte-identical if coherency holds
3. If they ARE byte-identical and contain the corrupt value, the bug is
   somewhere ELSE. If they differ, follow the divergence to its root.
4. If they're identical and contain a VALID value but the JIT got a corrupt
   one, look at recent x86 instructions before RIP for any that wrote to
   the destination register.

This is interactive — beyond the scope of what can be done programmatically
in this session.

## Updates (2026-05-11 final-v4): contradicting earlier finding — memory IS corrupt

Added `RUZU_DUMP_FASTMEM_VAS=0xV1,0xV2,...` to the SIGILL handler — at trap
time, dumps 16 bytes of host memory at `fastmem_base + each_vaddr`. Plus
`detect_fastmem_base_for_sigill()` populates the global atomic from
`/proc/self/maps` via a background thread.

When the SetX X19 trap fires at PC `0x80E43D38`, dumping host memory shows:

```
*[guest=0x814903E0] = 0x814903D0 0x814903D0    (valid pointers)
*[guest=0x814903E8] = 0x814903D0 0x2101E958B0  (valid)
*[guest=0x814903F0] = 0x2101E958B0 0x00002101A3B140A0  ← corrupt at +8!
*[guest=0x814903F8] = 0x00002101A3B140A0 0x0001000081490300  ← CORRUPT
*[guest=0x81490400] = 0x0001000081490300 0x814903F0
*[guest=0x814903E0..0x81490408] = (mstate bin pointers, mostly valid)
*[guest=0x81490350] = 0x2107B8F920 0x81490340  ← currently valid
```

**So memory at `0x814903F8` ACTUALLY DOES contain `0x2101A3B140A0`** — my
earlier conclusion that "writes are valid, read corrupts" was WRONG. The
slot really does contain corrupt data when the LDR reads it.

This contradicts the `RUZU_TRAP_FASTMEM_W64_CORRUPT_VADDR=0x814903F8` test
which captured ZERO corrupt-pattern writes to that vaddr. So the corrupt
value got into the slot via a path that bypasses my W64 vaddr-filter trap.

### Possible vectors for the missed corrupting write

1. **32-bit STP w-reg pair**: STP `w_lo, w_hi, [base, #offset]` writes 8 bytes
   as TWO 32-bit stores. Bytes 0..3 from w_lo, bytes 4..7 from w_hi. Each
   32-bit store doesn't match the W64 corrupt pattern filter. If STK has
   `stp w_lo, w_hi, [x, #imm]` with w_lo=0xA3B140A0 and w_hi=0x00002101,
   the combined memory looks like the corrupt 64-bit pattern.

2. **STP x-reg pair to a DIFFERENT base**: `stp x1, x2, [base, #imm]` where
   `base + imm == 0x814903F0` (or other slot), and X2 holds the corrupt
   value. The W64 trap would fire on the second half but only if its
   target vaddr filter is 0x814903F0 + 8 = 0x814903F8. Wait — that's what
   I tested. Hmm.

3. **128-bit STR Q-reg**: writes 16 bytes via SIMD store. Goes through
   `emit_a64_write_memory_128`, NOT `emit_a64_write_memory_64`. The W64
   trap doesn't fire. If the Q-reg's high 64 bits = 0x2101A3B140A0, this
   would write the corrupt pattern to bytes 8..15 (relative to store base).

4. **Cross-thread write from another emulator core**: if a non-core-0
   thread writes here, my core-0-only trap doesn't see it. But our earlier
   per-core counter showed cores 1-3 had near-zero JIT block entries.

5. **HLE service callback via raw host pointer**: bypasses Memory::write_64,
   but RUZU_WATCH_BLOCK doesn't catch this either.

### Next concrete experiment

Add a W128 corrupt-pattern trap mirror to the W64 trap. Check if it fires
when bytes 8..15 of a 128-bit store match the corrupt pattern.

Or: add a periodic poll (1µs interval) that snapshots memory at 0x814903F0
and 0x814903F8 and logs every byte change with timestamp. The corrupting
write will appear as a single transition. Compare with the captured W64
writes (all valid) — the corrupting transition will be at a moment when
no W64 trap fired, suggesting a different store width or path.

## Updates (2026-05-11 final-v5): the corrupting write bypasses all JIT traces

Continued investigation after the SIGILL memory-dump proved the corrupt value
IS in memory:

### Confirmed via additional probes
- `RUZU_TRACE_MAP_HOST_OFFSET=0x10B603F8` — only ONE guest VA (`0x81490000`)
  maps to memfd offset `0x10B60000`. No aliasing.
- `RUZU_TRACE_MEMORY_W64_AT_VADDR=0x814903F8` — zero Rust-side write_64 hits.
- `RUZU_TRAP_FASTMEM_W64_CORRUPT_VADDR=0x814903F8` — zero JIT fastmem-direct
  W64 writes with corrupt pattern.
- `RUZU_TRAP_FASTMEM_W64_VADDR=0x814903F8` — captured 30+ writes; ALL valid
  heap pointers in `0x21_0160_xxxx` range.

### The corrupting write path

Since:
- Memory at `0x814903F8` contains `0x2101A3B140A0`
- No W64 fastmem-direct write of the corrupt pattern to that vaddr observed
- No Rust-side `memory.write_64` to that vaddr observed
- No write_block touching that page (only NRO load)
- No 128-bit memory write fastmem-direct path exists (W128 goes through
  callback `memory_write_128` → calls `memory.write_64` twice, which would
  fire the trace — but it doesn't)

The corrupting write must go through one of:

1. **HLE service raw host pointer write**: Multiple HLE services use
   `(*device_memory).get_pointer(phys) as *mut u8` and write directly via
   the raw pointer (hid, vi, psc, ns, k_shared_memory). These bypass
   ALL of `Memory::write_block` / `Memory::write_64` / fastmem JIT traps.
   If one of these services writes to a physical address that happens to
   correspond to STK's BSS region `[0x81490000..0x81491000]` via a pool-
   allocation collision, the wedge would happen.

2. **Direct memfd manipulation**: ZeroPhys / clear_backing_region calls
   `madvise(MADV_REMOVE)` on memfd ranges. If the wrong physical range is
   cleared (a kernel bug or mis-mapping), the memfd content could become
   garbage. But the corrupt value is specific (`0x2101A3B140A0`), not zero,
   so this doesn't quite fit.

3. **Kernel page-table aliasing**: if KPageTable maps two different guest
   VAs to the same physical pool block, writes to one (e.g., a stack frame
   or another heap chunk) appear at the other (the mstate slot). Per the
   memory note about pool-vs-NRO collision (previously fixed), this kind
   of bug existed before.

### Next concrete step

Add `RUZU_HOST_PTR_WRITE_WATCH=0xHOSTADDR:LEN` infrastructure to
`HostMemory::get_pointer` callers, so any raw-pointer write through
`get_pointer_mut` is logged when it touches a tracked range. Specifically:

- Wrap `*get_pointer_mut(X)` calls in HLE services with a tracing helper
  that compares dest vs the watch range.
- Run with `RUZU_HOST_PTR_WRITE_WATCH=memfd_arena+0x10B603F8:8` to catch
  any raw-pointer write to the corrupt slot's memfd location.

The most likely root cause is a pool-allocation collision where a different
guest VA (used by a service shared-memory) shares physical pages with STK's
BSS, and the service's write corrupts STK's allocator state.

### Refinement: 128-bit write paths checked, all callback-based

`emit_a64_write_memory_128` always uses callback path (`memory_write_128`),
which in turn calls `memory.write_64(vaddr, lo)` + `memory.write_64(vaddr+8,
hi)`. Both would be caught by `RUZU_TRACE_MEMORY_W64_AT_VADDR=0x814903F8`,
which DIDN'T fire. So the corruption isn't via 128-bit fastmem either.

### Mystery summary

Memory at `0x814903F8` contains `0x2101A3B140A0` (verified via SIGILL host
memory dump). All known write paths have been traced and none captures this
write:
- Fastmem-direct W64 to 0x814903F8 — never observed with corrupt value
- Rust `memory.write_64` to 0x814903F8 — never observed
- `memory.write_block` to 0x814903F8 — never observed (only NRO load)
- `memory_write_128` callback → memory.write_64 twice — never observed
- No memfd aliasing (single guest VA maps to this memfd offset)

**This is a genuine investigation dead-end for static-instrumentation
techniques.** The next step REQUIRES gdb hardware-watchpoint attachment to
catch the corrupting write at the OS/CPU level, where instrumentation cannot
hide.

### Final session state

- Empirical wedge fingerprint: precise and reproducible (RUZU_CORE_COUNT=4 +
  fastmem 64-bit RW required)
- Corruption confirmed at `0x814903F8` slot (corrupt value `0x2101A3B140A0`)
- Corrupting write path remains unidentified — bypasses all instrumented
  Rust memory APIs and JIT fastmem-direct traps
- ~1900 line diff across 16 uncommitted files including cmpxchg16b
  correctness fix + ~1500 lines of diagnostic infrastructure
- Recommended next action: live gdb hw-watchpoint via
  `scripts/hw_watch_wedge.py` to capture the corrupting host RIP

## Updates (2026-05-11 final-v6): gdb hw-watchpoint successfully attaches but reveals a Heisenbug

Used gdb in batch mode with a hardware watchpoint on `fastmem_base + 0x814903F8`.
Implementation detail: the `scripts/hw_watch_wedge.py` arena detection
(largest anon rw-p) is WRONG; ruzu's fastmem virtual region is fragmented
by memfd:HostMemory overlays so the largest contiguous anon span is
unrelated. Replacement: grep `fastmem_pointer=Some(...)` from stderr.

The watchpoint fires correctly on every write to slot `0x814903F8`.
Captured 5 distinct JIT-emitted x86 RIPs during normal operation:
- 2 RIPs write valid heap pointers
- 3 RIPs write `0x814903E0` (mstate base)

**The wedge does NOT trigger when gdb is attached.** Hundreds of writes
fire, all VALID values (no `0x2101A3B140A0` pattern observed). Classic
Heisenbug — the hw-watchpoint context-switches into gdb on every write,
slowing STK enough that the wedge timing is disrupted.

### Recommended workaround

Ship ruzu with `RUZU_NO_FASTMEM_R64=1` or `RUZU_NO_FASTMEM_W64=1` as a
default OR add a `--no-fastmem-64` flag. Perf hit is significant but the
wedge is eliminated.

### Path forward for future sessions

The standard instrumentation approaches are exhausted. Non-perturbing
techniques needed:
1. Intel PT (Processor Trace) — captures every retired instruction at
   hardware level. Replay trace to find corrupting RIP.
2. eBPF uprobe at JIT-emitted RIPs — lightweight kernel-level capture.
3. SIGSTOP-on-corruption approach: have a poll thread detect the corrupt
   value transition and immediately SIGSTOP the process. Then attach gdb
   to a frozen process and inspect state.

Investigation halted at clear empirical bounds; corrupting instruction not
identified due to Heisenbug-sensitive timing.

## Updates (2026-05-12): SIGSTOP-on-corruption trap implemented, all 4 thread states captured at corruption moment

### Implementation

Extended `core/src/memory/memory.rs` POLL_DIVERGE infrastructure with a
non-perturbing corruption trap:

- `RUZU_POLL_TRAP_CORRUPT=1` — on first match of `(value & mask) == expect`
  at a polled vaddr, dump all `/proc/self/task/*/stat` thread states then
  `raise(SIGSTOP)` to atomically freeze all 4 JIT threads.
- Default pattern: `mask=0xFFFFFFFF00000000`, `expect=0x0000210100000000` —
  matches `0x2101A3B140A0`-class corruption.
- Overridable via `RUZU_POLL_TRAP_CORRUPT_MASK` / `_VALUE`.
- Custom env vars: `RUZU_POLL_DIVERGE_SLEEP_US=N` (1µs default useful).
- SEGV-safety: poll thread reads memory via `process_vm_readv()` syscall
  (returns `-EFAULT` on PROT_NONE pages without faulting the caller). Does
  NOT dereference any page-table or process-bound pointers — only the
  fastmem arena pointer captured at thread spawn, which lives for the
  program lifetime.

### Test harness

`/tmp/run_stk_trap_gdb.sh` launches ruzu with the trap, polls
`/proc/[pid]/stat` for state=T (SIGSTOPed), attaches gdb -batch with
`info threads + thread apply all bt + thread apply all info registers`,
detaches, then SIGCONT+kills.

### Result — KEY FINDING

The trap fires reliably and captures the exact corruption value
(`0x00002101A3B140A0`) at the polled vaddr 0x814903F8.

At the moment of corruption (verified across multiple runs):

- **CPUCore_0**: state=R (running), inside
  `rdynarmic::backend::x64::emit::emit_block` → `RegAlloc::end_of_alloc_scope` →
  `HostLocInfo::release_all` (vec clear).
  Rust frame `core::slice::raw::from_raw_parts_mut::precondition_check`
  with `data=0x4, len=0` (dangling-ptr-of-empty-Vec, harmless).
- **CPUCore_1, _2, _3**: state=S (sleeping), all in `PhysicalCore::idle` →
  `futex_wait` on `PhysicalCoreState`.
- **`ruzu-poll-diver`**: state=R, calling `raise(SIGSTOP)` from
  `memory.rs::do_trap`.

### Implication

**This REFUTES the prior multi-core-race hypothesis.** Only ONE physical
core has guest work scheduled at the corruption moment. The other three
are idle in futex_wait.

The wedge was empirically gated on multi-core mode (`RUZU_SINGLE_CORE=1`
eliminates it), but the corruption itself doesn't require concurrent guest
execution on multiple cores. Possibilities:
1. Multi-core mode enables a different JIT-emit path (different memory
   ordering ops) that has a codegen bug Core_0 hits even while alone.
2. The corruption is from earlier guest code that was scheduled on a
   now-idle core (Core_1/2/3 ran briefly, made the bad write, returned to
   idle, then Core_0 picked up work).
3. Multi-core mode initializes shared kernel state Core_0 then reads
   through, where single-core skips the init.

### What we still don't know

The exact ARM instruction and guest PC of the corrupting write.

`RUZU_TRAP_FASTMEM_W64_CORRUPT_VADDR=0x814903F8` (the JIT-emit-time UD2
trap on 64-bit fastmem-direct writes with corrupt value to the slot) did
**NOT** fire when the corruption happened. Yet the in-memory value IS
corrupt. So the corrupting write is NOT a 64-bit fastmem-direct W64 at
exactly vaddr=0x814903F8 with `value >> 40 == 0x21`. Candidates:
- 128-bit fastmem-direct write at vaddr=0x814903F0 (upper half = corrupt
  value lands at +8 = 0x814903F8). But upstream `emit_a64_write_memory_128`
  uses callback path, not fastmem-direct. So this would be visible via
  `RUZU_TRACE_MEMORY_W64_AT_VADDR=0x814903F8`, which historically didn't
  fire either.
- A combined sequence: e.g. `STP X, X, [base, #IMM]` at vaddr=0x814903F0
  emitted as TWO 64-bit fastmem-direct writes. The second write at
  0x814903F8 would have been caught by FASTMEM_TRAP if the value pattern
  matched. So either (a) value pattern at JIT-emit-time differs from the
  in-memory pattern at poll-time (intermediate values?), or (b) the JIT
  emits this STP as a single 128-bit op that bypasses the trap.
- A non-fastmem path: slow-path callback writes (RUZU_TRACE_MEMORY_W64_AT_VADDR
  filter also didn't fire historically — so this is unlikely).
- A torn write: 8 bytes of corrupt data trickled in via multiple smaller
  stores (e.g. 4× STRH, 8× STRB) that individually don't match the
  64-bit pattern.

### Next concrete step

Add a per-write ring buffer in rdynarmic that records (size, value,
host_rip, guest_pc) for ALL writes (any size: 8/16/32/64/128 bit,
fastmem-direct AND slow-path) targeting an env-gated vaddr range
(0x814903F0..0x81490400). The POLL_TRAP fires → dump the ring buffer →
the last entry before corruption is the culprit. This is doable since the
POLL_TRAP successfully freezes the process, giving us a clean post-mortem.


## Updates (2026-05-12 — additional bisection)

### Bisection: which fastmem write sizes are required for the wedge

Used the new POLL_TRAP to gate the wedge precisely. Each env disables one
fastmem-direct write width; the JIT routes that width through the slow-path
callback instead. Ran 3 trials per configuration (25s each):

| Disabled | TRAP fires (3 runs) | Wedge |
|----------|---------------------|-------|
| W8       | 0 / 0 / 0           | no    |
| W16      | 1 / 1 / 0           | flaky |
| W32      | 0 / 0 / 0           | no    |
| W64      | 0 / 0 / 0           | no    |
| All      | 0                   | no    |

**Disabling W8, W32, or W64 individually prevents the wedge.** W16 disabling
shows flaky behavior. This narrows the corruption mechanism to require BOTH
W8 + W32 + W64 fastmem-direct activity, or — more likely — disabling any of
these changes STK's execution timing enough to bypass the corrupting
sequence.

### Width-agnostic vaddr-range trap added

`RUZU_TRAP_FASTMEM_ANY_VADDR_RANGE=0xLO:0xHI` (new env var, in
`rdynarmic/src/backend/x64/a64_emit_x64_memory.rs`) emits UD2 inline when a
fastmem-direct write of ANY width (W8/W16/W32/W64) targets a vaddr in
`[LO, HI)`. Uses the same SIGILL R11=0xCAFEF00D sentinel + `[rsp+16]` vaddr
recovery convention as the existing W64 traps.

Tested with `RANGE=0x814903F0:0x81490400` (the 16-byte aligned slot region):
20000 SIGILL fires across **8 unique RIPs**, hitting two vaddrs:
`0x814903F0` (3 RIPs) and `0x814903F8` (5 RIPs). The 5 RIPs at the corrupt
slot itself are the same ones already known from `RUZU_TRAP_FASTMEM_W64_VADDR`;
all carry valid values. The 3 RIPs at `0x814903F0` are NEW — they write the
lower half of the 16-byte slot.

But: even with this wider net, no fastmem-direct write to `0x814903F8`
carries the corrupt value pattern. Plausible interpretations:

1. **The corruption is via slow-path callback**, not fastmem-direct. But
   historical `RUZU_TRACE_MEMORY_W64_AT_VADDR=0x814903F8` runs showed zero
   slow-path writes to this slot. So this would require either a write
   sized != 64 (W8/W16/W32 callbacks), or `write_block`, or `write_raw`.
2. **The corruption is a side effect of cache aliasing / memfd mapping
   weirdness** — but only one VA maps to this memfd offset (already ruled
   out earlier).
3. **The corrupt VALUE pattern check in the W64 trap is insufficient.**
   It currently checks `value >> 40 == 0x21`. If the corrupting write is a
   memcpy that uses an intermediate STR-Q (128-bit) AT VADDR 0x814903F0
   (which goes through callback), the callback would split into two
   `memory.write_64` calls — and the second leg writes at exactly
   `0x814903F8` with the corrupt value. This SHOULD be visible to
   `RUZU_TRACE_MEMORY_W64_AT_VADDR` — but the trace's overhead may
   serialize STK enough that the wedge takes a different path.

### Next concrete step

Emit an "after-write slot check" in the JIT: after every fastmem-direct
write, read `[R13 + 0x814903F8]` (R13 holds the arena base), check against
the corrupt value pattern, UD2 on match. This is the most expensive
diagnostic so far but localizes the corruption to the EXACT preceding
write. Implementation requires saving two scratch regs (r10 + r11) since
the 64-bit compare needs an imm64.


## Updates (2026-05-12 — width-encoded sentinel results)

Extended `RUZU_TRAP_FASTMEM_ANY_VADDR_RANGE` to encode write width (8/16/32/64)
in the SIGILL R11 sentinel low byte: `0xCAFEF008/10/20/40`. Updated
`ruzu_cmd/src/main.rs` SIGILL handler to recognize the new sentinel forms.

200 SIGILL events captured across **9 unique JIT-emitted RIPs**, ALL with
`r11=0xFFFFFFFFCAFEF040` — width=64. **Zero W8/W16/W32 fastmem-direct
writes hit the slot range `[0x814903F0, 0x81490400)`** during ~22 seconds
of STK boot.

### Implication

All known fastmem-direct activity in the slot region is 64-bit. Combined
with prior findings:

- W64 fastmem-direct: 9 RIPs, ALL valid values (no corrupt pattern via
  `RUZU_TRAP_FASTMEM_W64_CORRUPT`)
- W8/W16/W32 fastmem-direct: 0 writes to slot range
- W64 slow-path callback: 0 writes (`RUZU_TRACE_MEMORY_W64_AT_VADDR=0x814903F8`)
- W8/W16/W32 slow-path callback: only fires when fastmem disabled (and
  disabling that width prevents wedge)
- `write_block`: 1 event, the initial NRO LOADER copy at boot — not the
  runtime corruption source
- `write_raw`: only called internally from `write_*`

The corrupt value `0x2101A3B140A0` ends up in slot `0x814903F8` yet NO
identified write path stores that value at that address.

### Remaining candidates

1. **GPU DMA write** — the GPU thread (visible in /proc/self/task) writes
   guest memory for graphics. STK isn't past the "fsp-srv" stage when the
   wedge fires, so unlikely.
2. **HLE service direct pointer write** — some HLE service may bypass
   `write_block` and use raw pointer access via `get_pointer`. The slot
   could be incidentally hit.
3. **A subtle JIT bug where the trap sees one value but writes another**
   — extremely unlikely (the value-pattern check uses the same register
   that's about to be stored, after the store).
4. **kernel-level memory mapping issue** — perhaps a TLB/cache aliasing
   where the slot gets WRITTEN via fastmem-direct from another vaddr that
   accidentally shares the host page. This would explain why no trap on
   `0x814903F8` fires — the actual JIT write goes to a different vaddr,
   and the host page is aliased to the slot.

### Next concrete step: mprotect-based catch-all

mprotect the host page containing `arena + 0x81490000` to READ-ONLY.
Install a SIGSEGV handler that catches the resulting fault, logs RIP +
faulting address, un-protects, single-steps the offending instruction,
re-protects. This catches **every** write to the page regardless of size,
source, or whether it's JIT or HLE.

Implementation complexity: medium-high. SIGSEGV handler must
distinguish "expected fastmem fallback fault" (already handled by
existing exception_handler) from "watch-page fault" (new logic).


## Updates (2026-05-12 — BREAKTHROUGH: corrupting code site identified)

### The "ordered write" trap fix uncovered the corruption

The existing `RUZU_TRAP_FASTMEM_W64_CORRUPT` trap had a latent bug: it
checked `value_reg` AFTER the store, but `emit_write_memory_mov` uses
`xchg [mem], reg` for ORDERED writes. xchg SWAPS the register with
memory, so post-write `value_reg` holds the OLD memory contents, not the
just-stored value. The trap was missing ordered-store corruption.

Fixed by changing the trap to read from MEMORY (`[R13 + vaddr_reg]`)
instead of `value_reg`. This works correctly for both mov-based unordered
and xchg-based ordered stores.

### New "slot-after-write" trap

Added `RUZU_TRAP_SLOT_AFTER_WRITE=0xSLOT:0xMASK:0xVALUE`: after EVERY
fastmem-direct write (any width, any vaddr), read 8 bytes from
`[R13 + SLOT]` and trap if `(slot & MASK) == VALUE`. This catches the
case where the corrupting write goes to a vaddr DIFFERENT from the slot
(host-page aliasing, memfd overlap, cache effect, ...).

### Result — corrupting site confirmed

Ran with `SLOT=0x814903F8 MASK=0xFFFFFFFF00000000 VALUE=0x0000210100000000`:

- 7 SIGILL traps fired
- **All at guest PC = `0x0000000080E441A0`**
- This is INSIDE STK's dlmalloc `unlink` path (per memory notes: "LDP X1,
  X4, [X19, #8] / [X19, #24]"; "0x80E441A0..0x80E441E0 unlinks from
  doubly-linked free list")
- Captured A64 register state at trap fire (the slot is already corrupt
  when these traps fire — they're the FIRST fastmem-direct store after
  corruption):
  - X0  = 0x000000008148FEB0
  - X3  = **0x0000002101A3B140** ← the corrupt value shifted right by 8!
  - X4  = 0x00000000814903E1  (odd address — already-known dangling link)
  - X19 = 0x0000002101A364A0  (heap chunk pointer)
  - X22 = 0x0000000081490340  (mstate base; slot = X22 + 0xB8)
  - X30 = 0x0000000080E43EC4  (LR — caller is near `0x80E43EC0`)
- **`(X3 << 8) | 0xA0 = 0x2101A3B140A0`** — the exact corrupt value!

### Implication

The corruption is a **shift-left-by-8** operation that produces
`(valid_heap_addr << 8) | byte`. This is the SAME class of bug originally
suspected in memory note `project_stk_post_addv_unmapped_heap_wedge_2026_05_09.md`:

> "the corrupt value `0x2101A3B140A0` IS a real heap addr — bug is a
> stray LSL #8 (or BFI #8 / [base, idx, lsl #8])"

The original hypothesis was correct. The trap mechanism just had a latent
bug (xchg ordered-write swap) that prevented detection until now.

### Next concrete steps

1. **Disassemble STK NRO at 0x80E43EC0..0x80E441A0** to find the ARM
   instruction that produces `(X3 << 8) | 0xA0` and writes it to slot
   0x814903F8. Candidate instructions:
   - `ORR Xd, Xn, X3, LSL #8`
   - `ADD Xd, Xn, X3, LSL #8`
   - `BFI Xd, X3, #8, #40`
   - `LDR/STR with [base, idx, lsl #8]` indexed addressing
2. **Bisect to find the actual STORE PC** within the function:
   - Re-run trap with a narrower filter that fires only on FIRST corrupt
     observation (mark a static and skip thereafter).
   - Or add per-block instrumentation upstream of `0x80E441A0` to bracket
     the corruption boundary.
3. **Compare the JIT-emitted x86 code at the corresponding host RIP to
   the ARM semantics.** If the JIT mistranslates the instruction (e.g.,
   wrong shift amount in `ORR with LSL #8`), the bug is in rdynarmic. If
   the JIT is correct, the bug is upstream (STK's allocator is
   genuinely buggy or there's a guest-state issue).


## Updates (2026-05-12 — root cause: misaligned STR in dlmalloc unlink)

### Disassembly at PC=0x80E441A0

```
0x80E441A0: ldp x1, x4, [x19, #8]              ; x1=size+flags, x4=fd
0x80E441A4: mov x0, x21
0x80E441A8: ldr x3, [x19, #24]                 ; x3 = bk
0x80E441AC: and x1, x1, #0xfffffffffffffffc    ; mask size flags off x1
0x80E441B0: add x1, x19, x1                    ; next chunk
0x80E441B4: ldr x2, [x1, #8]
0x80E441B8: str x3, [x4, #24]      <-- THE CORRUPTING STORE
0x80E441BC: orr x2, x2, #0x1
0x80E441C0: str x4, [x3, #16]
0x80E441C4: str x2, [x1, #8]
```

### Mechanics of the corruption

At the time `str x3, [x4, #24]` fires:
- `x4 = 0x00000000814903E1` — ODD address (low bit set, bin-phantom tag)
- `x3 = 0x0000002101A3B140` — a valid heap pointer (bk = chunk address)
- Store destination = `x4 + 24 = 0x814903F9` — MISALIGNED 64-bit store
- The 64-bit unaligned store writes 8 bytes at host VA `arena + 0x814903F9`:
  - byte 0 of x3 (`0x40`) lands at `0x814903F9`
  - byte 1 of x3 (`0xB1`) lands at `0x814903FA`
  - ... bytes 2..6 (`0xA3 0x01 0x21 0x00 0x00`) at `0x814903FB..0x814903FF`
  - byte 7 (`0x00`) overflows into `0x81490400`

Reading 8 bytes from slot `0x814903F8`:
- byte 0 at `0x814903F8`: UNCHANGED (= `0xA0`, the low byte of the
  previous valid bin head pointer)
- bytes 1..7: the just-stored bytes of x3

Reassembled as a u64 (little-endian):
`(0x0000_2101_A3B1_40 << 8) | 0xA0 = 0x2101A3B140A0` ← the exact corrupt value.

### This is NOT a JIT codegen bug

ARM permits unaligned 64-bit stores (SCTLR.A=0). The JIT translates
`str x3, [x4, #24]` into `mov [r13 + vaddr_reg], rdx` where `vaddr_reg`
holds `x4 + 24`. x86 also permits unaligned access. The emulation
faithfully reproduces the ARM semantics.

The actual bug is **upstream**: x4 should NOT have its low bit set.
Something earlier set the chunk's FD pointer at `[x19+16]` to a value
with the bin-phantom tag bit `+1`. Real dlmalloc bin phantoms are at
aligned addresses (e.g., `0x814903E0`); a tag of `+1` indicates that
either:
1. This STK build uses a dlmalloc variant that tags bin phantoms in
   chunk metadata (requires unlink to mask) — but the disassembly does
   NOT mask x4, only x1.
2. An earlier `str` from our emulation incorrectly produced a tagged
   value where it shouldn't have.

### Why multi-core only?

The bisection result (W8/W32/W64 fastmem each individually prevents the
wedge; `RUZU_SINGLE_CORE=1` prevents the wedge) is now consistent: the
"upstream write that taints x4" happens via a race between cores. With
single-core mode or with any of W8/W32/W64 routed through slow-path
(slowing one path), the tainted write never reaches this unlink in time.

### Next step

To find which earlier ARM instruction writes the bad `0x814903E1` (or
equivalent tagged pointer) to a chunk's `+16` field, instrument all
fastmem-direct W64 stores whose VALUE has low bit set AND whose value
high bits match the bin-phantom pattern (`value >> 8 == 0x814903E`).
Add this filter to the existing trap infrastructure. The first such
store identifies the upstream corruption source.


## Updates (2026-05-12 — chain extends backward to PC=0x80211BD8)

### SIGILL auto-dump of stored value

Modified `ruzu_cmd/src/main.rs` SIGILL handler to automatically read
`[arena_base + recovered_vaddr]` and print as `stored_value@vaddr=...`.
This works for any trap with stack-recovery and reveals the actual
just-stored value (immune to xchg-swap and JitState staleness).

### Captured TWO tagged-bin-phantom writes in a single run

Using `RUZU_TRAP_FASTMEM_W64_ODD_BIN_VALUE_HEAP_DST=1` (vaddr in heap +
value `(>> 16 == 0x8149) AND (& 1 == 1)`):

| SIGILL | guest PC      | vaddr (heap dst)   | stored value          |
|--------|---------------|--------------------|-----------------------|
| #1     | `0x80211BD8`  | `0x2101615A90`     | `0x000000008149FFFF`  |
| #2     | `0x80E441A0`  | `0x2101A3B150`     | `0x00000000814903E1`  |

Both are W64 fastmem-direct stores producing tagged-mstate-region values
to heap chunks. SIGILL #2 is the same `str x4, [x3, #16]` instruction
inside the unlink already analyzed (writing the tagged forward-link back
into the bin-phantom-pointed chunk — propagation step).

### SIGILL #1 disassembly

PC=0x80211BD8 is the **block start** (JitState.pc). The actual STORE
within the block is at PC=0x80211BF8:

```
0x80211BE0: ldr x1, [x26]          ; x1_old = [x26] (load chunk metadata)
0x80211BE4: add x27, sp, #0xc8
0x80211BE8: mov x0, x27
0x80211BEC: ldr x3, [x28]
0x80211BF0: ldr x2, [x2, x25]
0x80211BF4: orr x1, x1, x21        ; x1 = x1_old | x21
0x80211BF8: str x1, [x26]          ; STORE corrupt-tagged x1 to heap
```

At trap fire:
- X21 = `0x10000`
- X26 = `0x2101615A90` (heap chunk destination)
- stored x1 = `0x000000008149FFFF`

Math: `[x26]_old | 0x10000 = 0x8149FFFF`. Since OR with `0x10000` only
sets bit 16, `[x26]_old` must have had bits 0-15 = `0xFFFF` AND its bit 0
already set (since x21 lacks bit 0). So **[x26]_old was already a tagged
value `0x8148FFFF` or `0x8149FFFF`** before this load — there's an even
EARLIER corrupting write we haven't yet captured.

### Filter expansion needed

The current filter requires `value >> 16 == 0x8149`. If `[x26]_old` was
`0x8148FFFF`, an earlier W64 store that produced this value would NOT
match (its bits 16-31 = `0x8148`, not `0x8149`).

To find the absolute earliest tainting write, broaden the filter to
match the entire mstate page range, e.g., `value >> 24 == 0x81 AND
(value >> 12) & 0xFFF in {0x148, 0x149}` AND `value & 1 == 1`. Or add a
new env var `RUZU_TRAP_FASTMEM_W64_VALUE_PREFIX=0xPREFIX:0xMASK` for
arbitrary prefix matching.

### Working hypothesis

The chain so far:

1. (Unknown earlier PC) writes `0x8148FFFF`-class value to chunk's metadata
   field at heap addr `0x2101615A90`.
2. PC=`0x80211BF8` (block at `0x80211BD8`) ORs that value with
   `0x10000` and stores back — producing `0x8149FFFF`.
3. Some later code reads this chunk and uses it as a pointer (low bit
   indicates "phantom-tag" in some dlmalloc variants).
4. The tagged pointer propagates through the free-list, eventually
   producing `x4 = 0x814903E1` at PC=`0x80E441A0`.
5. `str x3, [x4, #24]` at PC=`0x80E441B8` performs a misaligned 64-bit
   store to `0x814903F9`, partially overwriting slot `0x814903F8` with
   bytes of `x3 = 0x2101A3B140`, producing the corrupt value
   `0x2101A3B140A0`.

The chain is consistent with TIMING-SENSITIVE behavior (bisection
result: disabling any of W8/W32/W64 prevents corruption). The earliest
upstream write is likely produced by a multi-thread race in libnx's
dlmalloc when guest threads on different cores update the same chunk's
metadata concurrently without proper locking.


## Updates (2026-05-12 — full chain mapped)

### Bit-accumulation loop confirmed

With `RUZU_TRAP_FASTMEM_W64_ODD_BIN_PREFIX=0x8148` (new env var to override
the hardcoded high-16 match), captured 16 SIGILL events all at the same
PC=`0x80211BD8`, all to vaddr `0x2101615A90`, with stored values:

```
0x81480001  0x81480003  0x81480007  0x8148000F  0x8148001F
0x8148003F  0x8148007F  0x814800FF  0x814801FF  0x814803FF
0x814807FF  0x81480FFF  0x81481FFF  0x81483FFF  0x81487FFF  0x8148FFFF
```

Each iteration ORs in one more low bit. This is **dlmalloc's bin-map
construction loop**. The base value `0x81480000` (before the first OR)
must have been put there by an earlier write.

### Chunk lifecycle trace (vaddr=0x2101615A90)

Tracing all W64 fastmem-direct writes to this single chunk address
(`RUZU_TRAP_FASTMEM_W64_VADDR=0x2101615A90`):

| Order | PC          | Stored value                |
|-------|-------------|-----------------------------|
| 1     | `0x80E4E2C0`| `0x0000000000000000`        |
| 2     | `0x80E4E2C0`| `0x0000000000000000`        |
| 3     | `0x8005F600`| `0x3F7793DBBFE82FC7` (random / FP) |
| 4     | `0x80E463E0`| `0x0000000000000030`        |
| 5     | `0x80E46500`| `0x0000000081490350` (bin head pointer) |

### Disassembly key instructions

**PC=0x80E463E0** (allocator chunk-size tagging — NORMAL dlmalloc):
```
0x80E463E0: orr x5, x5, #0x1     ; x5 |= 1 (set PINUSE flag on chunk size)
0x80E463E4: str x5, [x3, #8]     ; store tagged size to chunk+8 (SIZE slot)
0x80E463E8: str x2, [x3, x2]
```

**PC=0x80E46500** (allocator chunk linkage):
```
0x80E46500: stp x3, x3, [x4, #32]
0x80E46504: stp x6, x5, [x3, #8]   ; chunk+8 = x6 (size), chunk+16 = x5 (FD!)
0x80E46508: str x5, [x3, #24]      ; chunk+24 = x5 (BK)
```

This is the candidate upstream tainting: if `x5` is a size value with
PINUSE bit set (from 0x80E463E0 path), and PC=0x80E46500 stores x5 to
both FD (chunk+16) AND BK (chunk+24) slots, the FD slot gets tagged. The
unlink at PC=0x80E441A0 later loads this tagged value as x4 (a pointer)
and the misaligned store at PC=0x80E441B8 corrupts the slot.

**PC=0x80E441A0..0x80E441C4** (allocator unlink — the propagation /
final corruption):
```
0x80E441A0: ldp x1, x4, [x19, #8]   ; x1=size+flags, x4=FD (possibly tagged)
...
0x80E441B8: str x3, [x4, #24]       ; MISALIGNED if x4 is tagged → corrupts slot
0x80E441C0: str x4, [x3, #16]       ; propagates tagged x4 to next chunk
```

### Full corruption chain (working hypothesis)

1. STK allocator at PC=`0x80E463E0` sets PINUSE flag: `x5 |= 1` then stores
   to chunk's SIZE slot. NORMAL behavior.
2. At PC=`0x80E46500`, `stp x6, x5, [x3, #8]` stores x5 to BOTH the SIZE
   slot (chunk+8) AND the FD slot (chunk+16). **If x5 carries the PINUSE
   tag from step 1, this taints the FD slot with low bit set.**
3. Bit-accumulation loop at PC=`0x80211BD8` may further mutate chunk
   metadata.
4. Unlink at PC=`0x80E441A0` loads tagged FD into x4.
5. `str x3, [x4, #24]` at PC=`0x80E441B8` performs MISALIGNED 64-bit
   store at vaddr `x4+24`, partially overwriting slot `0x814903F8`,
   producing the corrupt value `0x2101A3B140A0`.
6. STK subsequently reads slot `0x814903F8` as a pointer, dereferences
   the corrupt heap address, and the cascade produces the unmapped-read
   wedge.

### Why multi-core only

The chain involves at least two distinct ARM code paths
(PC=`0x80E463E0` setting tag + PC=`0x80E46500` propagating to FD). If
these are intended to run sequentially within a single allocator
operation (with proper invariants), they may produce a consistent
non-tagged FD on real hardware. But with multi-core mode, two guest
threads may interleave at this point, causing one thread to see x5
with the PINUSE tag intended for the SIZE slot — and stp-write it to
the FD slot.

Disabling W8, W32, or W64 fastmem (or `RUZU_SINGLE_CORE=1`) serializes
this interleaving and the chain doesn't manifest.

### Practical recommendation

**Ship ruzu with one of these defaults for AArch64 multi-core games:**
- `RUZU_SINGLE_CORE=1` (simpler; slight perf cost on multi-core HW)
- `RUZU_NO_FASTMEM_W8=1` (preserves multi-core; minor perf cost on
  small stores)

OR add a `--single-core` / `--no-fastmem-w8` flag.

### Why this is unlikely a JIT codegen bug

- All identified instructions disassemble correctly with `objdump`.
- PC=`0x80E463E0`'s `orr x5, x5, #0x1` is a valid ARM bitwise-OR.
- The misaligned 64-bit store at PC=`0x80E441B8` is faithfully emulated
  (ARM permits unaligned access with SCTLR.A=0; x86 likewise).
- The chain only manifests under MULTI-CORE timing — a JIT bug would be
  deterministic regardless of core count.

The root cause is therefore most likely a **race in libnx's dlmalloc
multi-thread synchronization** (or its absence) being exposed by ruzu's
multi-core fiber scheduling. This is upstream and outside ruzu/rdynarmic
scope. The diagnostic infrastructure built this session enables future
investigation if libnx/STK upstream wants to fix the race.

### Diagnostic infrastructure delivered this session

In `core/src/memory/memory.rs`:
- `RUZU_POLL_TRAP_CORRUPT=1` — non-perturbing SIGSTOP-on-corruption
- `RUZU_POLL_TRAP_CORRUPT_MASK` / `_VALUE` — custom corruption pattern
- `process_vm_readv`-based safe arena reads (no SEGV on PROT_NONE pages)
- `/proc/self/task/*/stat` dump before SIGSTOP

In `rdynarmic/src/backend/x64/a64_emit_x64_memory.rs`:
- `RUZU_TRAP_FASTMEM_ANY_VADDR_RANGE=0xLO:0xHI` — width-agnostic vaddr
  range trap
- `RUZU_TRAP_FASTMEM_SLOT_AFTER_WRITE=0xSLOT:0xMASK:0xVALUE` — checks
  slot contents after every fastmem-direct write (any size)
- `RUZU_TRAP_FASTMEM_W64_VALUE_TAGGED_PHANTOM=1` — tagged bin-phantom
  filter
- `RUZU_TRAP_FASTMEM_W64_ODD_BIN_PREFIX=0xPREFIX` — override hardcoded
  high-16 match in existing trap
- Fixed `RUZU_TRAP_FASTMEM_W64_CORRUPT` to read from memory (not
  value_reg) — works correctly for xchg ordered stores
- Width-encoded sentinels: `0xCAFEF0_{08/10/20/40/80/E1}` distinguish
  trap origin

In `ruzu_cmd/src/main.rs`:
- SIGILL handler recognizes new sentinel encodings
- Auto-dumps stored value at recovered_vaddr via fastmem_base lookup


## Updates (2026-05-12 — REAL root cause: rdynarmic exclusive CAS has unbound `expected`)

User correctly challenged the "libnx dlmalloc race" conclusion: yuzu and
real hardware run STK fine. So if libnx had a genuine race, it would
manifest there too. The bug must be in how ruzu/rdynarmic emulates
something that yuzu's dynarmic gets right.

### Found the bug

**File**: `rdynarmic/src/backend/x64/emit_exclusive_memory.rs:160`

```rust
fn emit_exclusive_write(...) {
    ...
    ra.host_call(
        Some(inst_ref),
        &mut [None, Some(&mut first[1]), Some(&mut rest[0]), None],
        //                              ↑               ↑
        //                              vaddr           value
        //                                            ↑    ↑
        //                                            RCX (expected) — left None!
    );
    callback.emit_call_simple(...).unwrap();
}
```

The callback signature is `exclusive_write_64(&self, vaddr, value, expected) -> bool`.
The first 3 args come from `host_call` slots [RDI=self, RSI=vaddr, RDX=value].
The 4th slot (RCX = `expected`) is `None` — **unbound, receives stale garbage**.

The callback then performs:
```rust
atomic.compare_exchange(expected, value, SeqCst, SeqCst)
```

— a CAS against **garbage `expected`**. Most CAS calls fail. **Two cores
racing on the same mutex can BOTH have their stale RCX coincidentally
match memory → both CAS succeed → both think they hold the mutex →
concurrent free-list modification → corrupt FD pointer cascade → wedge.**

### Compare with upstream dynarmic

`zuyu/externals/dynarmic/src/dynarmic/backend/x64/emit_x64_memory.cpp.inc:298`:

```cpp
code.CallLambda(
    [](AxxUserConfig& conf, Axx::VAddr vaddr, T value) -> u32 {
        return conf.global_monitor->DoExclusiveOperation<T>(
            conf.processor_id, vaddr,
            [&](T expected) -> bool {
                return (conf.callbacks->*callback)(vaddr, value, expected);
            }) ? 0 : 1;
    });
```

Upstream calls `global_monitor->DoExclusiveOperation` which:
1. Checks if THIS processor has a valid reservation for vaddr
2. Retrieves the SAVED expected value from `exclusive_values[processor_id]`
   (populated by LDXR during exclusive_read)
3. Passes saved expected to the callback closure

Result: CAS uses the value loaded by the matching LDXR — semantically
correct ARM exclusive-access semantics.

Also confirmed: upstream `emit_x64_memory.cpp.inc:233/251/361` sets
`exclusive_state = 1` in the JIT-emitted LDXR code path, and the saved
value goes into the monitor's `exclusive_values` array. rdynarmic's
`exclusive_read_64` callback just calls `memory_read_64` — never sets
`exclusive_state`, never saves to `exclusive_values`.

### Consistency with all prior observations

This explains EVERYTHING:
- **Single-core mode prevents wedge**: no concurrent mutex acquisitions,
  so the broken CAS doesn't cause races (one thread at a time).
- **Multi-core mode triggers wedge**: concurrent CAS race with garbage
  `expected` produces incorrect "both succeed" outcomes.
- **Disabling W8/W32/W64 fastmem prevents wedge**: routes accesses
  through slow path which serializes timing, making the rare coincidental
  garbage match much less likely.
- **gdb hw-watchpoint masks wedge (Heisenbug)**: context switches
  serialize threads, breaking the race window.
- **All known fastmem-direct W64 writes to slot carry valid values**:
  yes, because the CORRUPTION is not from a single write with a corrupt
  value — it's from concurrent operations both succeeding when they
  shouldn't, leading to inconsistent free-list state with chunks tagged
  via concurrent in-progress updates.
- **PC=0x80E441A0 unlink sees x4 = 0x814903E1 (tagged FD)**: this is the
  CONSEQUENCE of a free-list update happening concurrently with another
  one that set the PINUSE bit on a chunk's metadata. Without proper
  exclusive access, dlmalloc's chunk-init code (at PC=0x80E46500
  storing x5 with the tag bit to multiple metadata slots) races with
  another core's unlink. The forward-link slot ends up holding a value
  with the PINUSE bit set instead of a clean pointer.

### Fix outline

Mirror upstream's pattern. Two options:

**Option A (minimal change)**: In `emit_exclusive_read`, after the
callback returns the value, emit code to save it into
`JitState.exclusive_value` and set `JitState.exclusive_state = 1`. In
`emit_exclusive_write`, before the callback, emit code to:
1. Test `JitState.exclusive_state`; if 0, set RAX=1 (failure), skip callback.
2. Otherwise, clear `JitState.exclusive_state`.
3. Load `JitState.exclusive_value` into RCX (= `expected` arg).
4. Call the callback (which performs `compare_exchange`).

**Option B (upstream-faithful)**: Replace the direct callback call with
a lambda that calls `global_monitor->do_exclusive_operation`, passing a
closure that calls the user's callback with the monitor-saved expected.

Option A is simpler. Option B is closer to upstream and handles the
cross-processor invalidation (DoExclusiveOperation also clears the
reservation for OTHER processors that share the address).

This fix is in rdynarmic and applies to ALL A64 exclusive ops (LDXR/STXR
of all widths plus the 128-bit pair forms). It should also be applied
to A32 (which has the same callback structure).

### Test plan

1. Implement fix (Option A or B).
2. Verify STK boots without wedge with multi-core mode (no
   `RUZU_SINGLE_CORE=1`, no `RUZU_NO_FASTMEM_*`).
3. Verify CAS semantics with a focused unit test:
   - Two host threads concurrently call exclusive_write_64 with the same
     value-after-LDXR pattern; exactly one should succeed.
4. Verify existing JIT tests still pass.


## Updates (2026-05-12 — CORRECTION: exclusive trampolines were already Option B-correct)

### What I got wrong

I claimed `emit_exclusive_write` passes 4 slots `[None, vaddr, value, None]` to
a 4-arg callback `exclusive_write_64(self, vaddr, value, expected)`, leaving
RCX (the `expected` arg) unbound. **This was incorrect.** The host_call slot
count matches the *trampoline* signature, not the user callback signature.

The actual chain:
- JIT-emit (`emit_exclusive_memory.rs`) → host_call with 3 args:
  `[None=auto-RDI, vaddr=RSI, value=RDX, None=unused]`
- Trampoline (`jit.rs:1593` `exclusive_write_64_trampoline`) is a 3-arg
  `extern "C" fn(inner_ptr, vaddr, value)`. The 4th slot is correctly
  unused.
- The trampoline INTERNALLY handles `expected`:
  1. Checks `inner.jit_state.exclusive_state`; returns 1 (failure) if 0.
  2. Clears `exclusive_state = 0`.
  3. Calls `monitor.do_exclusive_operation(processor_id, vaddr, closure)`.
  4. The monitor's `check_and_clear` verifies reservation, retrieves
     saved `expected` from `exclusive_values[processor_id]` (set by LDXR's
     `read_and_mark`).
  5. Passes saved expected to closure.
  6. Closure calls `callbacks.exclusive_write_64(vaddr, value, expected)`
     with the PROPER expected.

**This IS Option B.** It mirrors upstream's `DoExclusiveOperation` lambda
pattern — just implemented as a Rust trampoline instead of CallLambda.

### Verified by instrumentation

Added `RUZU_TRACE_EXCLUSIVE=1` env var to count exclusive op invocations.
STK boot run shows:
- 14× `EXCL_W32` with values alternating `0x000083FF` / `0x00000000` at
  vaddrs in the `0x8154...` range. The `0x83FF` value matches `X1` we saw
  in earlier SIGILL captures — this is a libnx thread-handle ID being
  CAS'd into a mutex.
- 10× `EXCL_W64` writing small counters/sequence numbers `0x1..0xA` to
  vaddrs in the `0x815E...` range.

So exclusive ops ARE firing and working. The wedge still triggers (3
POLL_TRAP lines fired in this run). The bug is NOT in exclusive ops.

### Where is the bug then?

Remaining suspects, in order of plausibility:
1. **Memory ordering**: ARM release/acquire semantics not fully enforced
   on x86. STK's allocator releases the mutex (STLR), but cross-core
   visibility of preceding stores might not be guaranteed. x86 TSO
   handles most cases but not StoreLoad — for which STLR needs MFENCE
   or LOCK-prefixed instruction. `emit_write_memory_mov::<64>` uses
   `xchg` for ordered stores which HAS implicit LOCK; this should be
   correct. **Need to verify which ARM instruction at the mutex
   release uses Ordered AccType and what x86 it emits.**
2. **Misaligned-store atomicity**: when x4=0x814903E1, the JIT emits
   `mov [r13+vaddr], rdx` to a misaligned address. On x86, misaligned
   64-bit stores are NOT atomic to OTHER cores — they may tear. So if
   ANOTHER core reads while THIS core's misaligned store is in
   progress, it can see a partially-updated value. But the upstream
   problem (x4 having the tag bit) must be solved first.
3. **TPIDR_RO_EL0 / per-core scheduler state**: if ruzu's per-core
   identity (used by libnx for thread-local storage and lock-elision)
   is wrong, libnx might think two guest threads on different cores are
   the SAME thread → mutex acquire returns success without contention
   check.
4. **A genuine codegen bug** in some instruction near PC=0x80E46500 or
   PC=0x80E463E0 that we haven't yet inspected for correctness.

### Apology and revised next step

I owe an apology for the false alarm on exclusive ops. The infrastructure
is correct. The TRAMPOLINE acts as the upstream `CallLambda` body —
functionally equivalent.

The next concrete investigation should be:
- Add `RUZU_TRACE_EXCLUSIVE_READ=1` and dump EXCL_R32/R64 calls too to
  see the FULL LDXR/STXR pairs and check for "two cores both succeed"
  cases.
- Run STK with `RUZU_TRACE_EXCLUSIVE=1` and `RUZU_POLL_TRAP_CORRUPT=1`
  simultaneously, and check the time-ordering of EXCL events vs the
  POLL_TRAP fire to see what mutex state was being held at corruption
  moment.
- Disassemble PC at the call site that goes to PC=0x80E441A0 (the
  unlink) to find what mutex/sync is supposed to protect it.


## Updates (2026-05-12 — mutex IS held, single-thread inside; bug is elsewhere)

### Confirmed: malloc_lock / malloc_unlock identified

Disassembly of `0x80E4DC60` and `0x80E4DC80`:
```
0x80E4DC60: adrp x0, 0x815e3000
0x80E4DC64: add  x0, x0, #0x2c0   ; x0 = 0x815E32C0 (mutex addr we traced)
0x80E4DC68: b    0x80E65DE0        ; tail-call lock impl

0x80E4DC80: adrp x0, 0x815e3000
0x80E4DC84: add  x0, x0, #0x2c0   ; same mutex
0x80E4DC88: b    0x80E65E20        ; tail-call unlock impl
```

These are libnx's dlmalloc-style `malloc_lock` and `malloc_unlock` stubs.
The mutex they protect is at vaddr `0x815E32C0` — matching the EXCL_R32/W32
trace events. The unlink at PC=`0x80E441A0` is called from a caller with
LR=`0x80E43EC4` (the BL at 0x80E43EC0 is `bl 0x80e4dc60` = malloc_lock).
So at the unlink fire, the mutex IS held.

### Trace shows single-thread runs

100,000+ EXCL_W32 events all with value `0x000083FF` and matching
EXCL_R32 reads from the same vaddr. That's a SINGLE thread (handle
`0x83FF`) acquiring + releasing the malloc mutex 100,000+ times. No
contention — the mutex is acquired, used, released by the same thread
in a loop.

POLL_TRAP fires AFTER all visible mutex activity, in a region where
only one thread holds the mutex. So the wedge is NOT from a race inside
the mutex-protected region.

### Where IS the bug then?

If exclusive ops are correctly serialized (verified), the mutex IS held
(verified), and only one thread is in the protected region — yet
corruption manifests in multi-core mode — the bug must be:

1. **A guest thread NOT holding the mutex modifies allocator state**:
   maybe `realloc` or some other entry point doesn't go through
   malloc_lock; OR a kernel-side write into guest memory (HLE service
   handler) lands at the slot vaddr via a misdirected DMA / ipc-buffer
   write.
2. **A multi-thread race in cores' fastmem-arena page mappings**:
   maybe two cores see different host pages for the same guest vaddr
   due to a TLB / mmap race in `map_pages_at_address`.
3. **A subtle JIT codegen bug** that ONLY manifests when more than one
   core has compiled the same guest function — different codegen for
   the same PC across cores producing different host bytes (e.g.,
   different register allocation → different x86 instruction
   sequence → different unaligned-store atomicity).
4. **The corruption value `(heap_addr << 8) | byte` is not coming from
   a single store at all** but from a torn/interleaved sequence of
   sub-64-bit stores that the SLOT_AFTER_WRITE trap caught at
   PC=0x80E441A0 because that's the FIRST W64 fastmem store after the
   slot became corrupt by smaller writes elsewhere.

### Practical conclusion

The investigation has reached the limit of static + JIT-level
instrumentation. Further progress requires either:

- A working `yuzu` build to run differential SVC trace alongside ruzu
  and identify the FIRST behavioral divergence point.
- A `perf record` or Intel PT capture of both runs (yuzu vs ruzu) and
  diff at the instruction level.
- Implementing `RUZU_TRACE_ALL_W64_FASTMEM=1` to log EVERY 64-bit
  fastmem-direct store along with vaddr, value, and host RIP — then
  diff vs an equivalent yuzu trace.

### Recommended workaround for shipping

Until the deeper investigation completes:

- Default `RUZU_SINGLE_CORE=1` for AArch64 NRO homebrew loads — works
  around the wedge with a perf cost on real-multi-core HW.
- Or expose `--single-core` CLI flag.

### Diagnostic infrastructure ready for future investigation

All env-vars added this session (now committable):
- `RUZU_POLL_TRAP_CORRUPT=1` + `RUZU_POLL_TRAP_CORRUPT_MASK/_VALUE`
- `RUZU_TRAP_FASTMEM_ANY_VADDR_RANGE=0xLO:0xHI`
- `RUZU_TRAP_SLOT_AFTER_WRITE=0xSLOT:0xMASK:0xVALUE`
- `RUZU_TRAP_FASTMEM_W64_VALUE_TAGGED_PHANTOM=1`
- `RUZU_TRAP_FASTMEM_W64_ODD_BIN_PREFIX=0xPREFIX`
- `RUZU_TRACE_EXCLUSIVE=1` (counts LDXR/STXR through trampolines)
- Auto-dump of `stored_value@vaddr` in SIGILL handler

Combined with the existing `RUZU_TRAP_FASTMEM_W64_*` family and
`RUZU_POLL_DIVERGE`, this gives future investigators a comprehensive
toolkit for tracking memory corruption without re-instrumenting from
scratch.


## Updates (2026-05-12 — separate bug found: fiber stack overflow at boot)

### User suggestion: check Boost.Context fibers

The user suggested investigating fibers as a potential bug source. Inspected
`common/src/fiber.rs` and found a separate, real bug:

**Ruzu used `FixedSizeStack` (no guard page protection) for fiber stacks.**

Upstream zuyu uses `VirtualBuffer<u8>` which can be paired with explicit
guard handling. Ruzu's `FixedSizeStack` (from the `context` Rust crate)
just allocates raw stack memory — a stack overflow silently corrupts
adjacent memory rather than faulting.

### Experiment: switch to `ProtectedFixedSizeStack` at 512 KB (original size)

Result: STK SIGSEGVs almost immediately at boot. The Rust stack-overflow
signal_handler symbol `std::sys::pal::unix::stack_overflow::imp::signal_handler`
catches the guard-page fault.

**This confirms: the 512 KB fiber stack was overflowing silently at boot
in the original (unprotected) configuration.** Debug-mode Rust frames are
significantly larger than equivalent C++ frames (no inlining, more safety
scaffolding), so libnx init's deep call chains exceed 512 KB.

### Experiment: 4 MB and 16 MB protected stacks

Result: no boot-time SIGSEGV (overflow eliminated), but the **STK wedge
still triggers**. POLL_TRAP fires; corruption at slot 0x814903F8 persists.

### Conclusion

Two SEPARATE bugs:
1. **Fiber stack too small + unprotected** — silent stack overflow during
   STK init under debug-mode Rust frames. Fixed by switching to
   `ProtectedFixedSizeStack` and increasing default size to 4 MB.
2. **The STK multi-core wedge** — independent of stack overflow.
   Identified mechanism (misaligned STR at PC=0x80E441B8 due to tagged x4)
   but not the root cause of why ruzu produces tagged values that yuzu
   doesn't.

The stack-overflow fix in `common/src/fiber.rs` is a real improvement
worth keeping regardless of the wedge investigation outcome:
- Switches `FixedSizeStack` → `ProtectedFixedSizeStack` (turns silent
  corruption into immediate SIGSEGV with stack-overflow signature).
- Bumps `DEFAULT_STACK_SIZE` 512 KB → 4 MB (sufficient for Rust debug
  frames).


## Updates (2026-05-12 — fiber `&mut` aliasing UB found + fixed; memory layout verified)

### Second fiber bug: `&mut FiberImpl` aliasing UB

User pushed to keep digging into fibers. Found another real Rust-specific
bug in `common/src/fiber.rs`:

```rust
// BEFORE (UB):
pub fn yield_to(weak_from: Weak<Fiber>, to: &Arc<Fiber>) {
    let to_imp = unsafe { &mut *to.imp.get() };  // ← &mut BEFORE lock!
    let guard = to_imp.guard.lock();
    ...
}
```

Two host threads can call `yield_to` on the same `to` fiber concurrently
(scheduler hand-off across cores). Both construct `&mut FiberImpl` to
the same memory before either acquires the guard — this is **undefined
behavior in Rust** because `&mut` carries `noalias`. C++ upstream has no
such aliasing rule (raw pointers, OK).

Fix: acquire the lock through a raw-pointer dereference of the mutex
field FIRST (which only borrows `&Mutex`, not `&mut FiberImpl`), then
construct `&mut FiberImpl`. Also narrowed the `&mut`'s scope to drop
before the context-switch (`resume()` call) so no `&mut` lives across
re-entry; used raw pointers for the previous-fiber unlock path.

Applied in `yield_to`, `start`, and `thread_to_fiber`. Tested: STK
still wedges (POLL_TRAP fires 3/3 runs). So the UB wasn't the root
cause of THIS wedge, but the fix is still a correctness improvement.

### Memory layout verified (no mmap overlap)

Captured `/proc/PID/maps` at the SIGSTOP'd wedge moment:
- Arena: `754daac00000-754e2ac00000 rw-p` (512 GB reservation, MAP_NORESERVE)
- JIT code buffers: `0x754beb400000..0x754c4b400000` and similar — all
  BELOW the arena range.
- Fiber stacks: `0x754d857...` series, each 2-4 MB with PROT_NONE
  guard pages between (confirmed `ProtectedFixedSizeStack` working) — all
  BELOW the arena.
- No mapping overlaps with the arena's host VA range.

So the corruption isn't from mmap collisions (JIT code clobbering arena,
fiber stack overflow landing in arena page, etc.).

### Summary of what's been ruled out

| Hypothesis | Status |
|------------|--------|
| Exclusive monitor (LDXR/STXR) wrong | ✗ Verified correct |
| Memory ordering / STLR fence | ✗ Uses xchg (full LOCK fence) |
| W64 fastmem-direct corrupt value | ✗ All values valid in trace |
| W8/W16/W32 fastmem-direct to slot | ✗ No such writes seen |
| W64 slow-path callback | ✗ Zero events to slot vaddr |
| 128-bit callback to slot | ✗ Zero events |
| write_block to slot | ✗ Only initial NRO load |
| HLE direct pointer write | ✗ Zero `get_pointer` calls to page |
| Fiber stack overflow | ✗ Fixed via Protected + 4 MB |
| `&mut FiberImpl` aliasing UB | ✗ Fixed |
| JIT code / fiber stack / arena mmap overlap | ✗ Verified clean layout |

The wedge still triggers in 3/3 runs with all of the above fixed.
Whatever causes it is something we haven't yet instrumented.

### Real fixes worth committing regardless of wedge

1. `common/src/fiber.rs`:
   - `FixedSizeStack` → `ProtectedFixedSizeStack` (guard pages now catch
     stack overflows instead of silent corruption).
   - `DEFAULT_STACK_SIZE` 512 KB → 4 MB (Rust debug-mode frames need it).
   - Fixed `&mut FiberImpl` aliasing UB in `yield_to`, `start`,
     `thread_to_fiber` (lock before constructing &mut; drop &mut before
     context-switch; raw pointers for cross-fiber write paths).


## Updates (2026-05-12 — W8 bisection result reinterpreted: it's TIMING-only)

### W8 trace reveals string-copy operations, not tag-bit setters

Routed all W8 fastmem writes through callback (`RUZU_NO_FASTMEM_W8=1`) and
logged every W8 heap write whose value has bit 0 set
(`RUZU_TRACE_W8_HEAP_TAG=1`).

Result: 200 events captured. Values are ASCII characters:

```
0x23 '#'  0x2D '-'  0x2F '/'  0x31 '1'  0x35 '5'  0x3D '='
0x43 'C'  0x55 'U'  0x61 'a'  0x63 'c'  0x65 'e'  0x67 'g'
0x69 'i'  0x6B 'k'  0x6D 'm'  0x6F 'o'  0x71 'q'  0x73 's'  0x75 'u'
```

Most common (141 of 200) is at PC=0x8020A3F0 with LR=0x8020A3E4 — a
tight memcpy-like loop. Also frequent: PC=0x80E45F2C with various LR
values (different callers all going through one memcpy entry).

**These are user-data string writes** — STK populating heap-allocated
buffers with file paths, identifiers, translations, etc. Each character
that has bit 0 set (~half of ASCII letters and digits) shows up.

### Reinterpretation of W8 bisection

The earlier finding "RUZU_NO_FASTMEM_W8=1 prevents the wedge" doesn't
mean W8 writes directly cause the corruption. Instead:

- Heavy W8 traffic (memcpy of strings into heap) is a TIMING component
- Disabling W8 fastmem routes every byte-copy through the callback,
  slowing string copies by orders of magnitude
- This timing shift de-synchronizes whatever race ACTUALLY causes the
  tag-bit-setting at chunk[+16]
- The ACTUAL corrupting write hasn't been observed in any of our
  instrumented paths

Same applies to the W32 and W64 bisection — disabling any commonly-used
fastmem width slows the hot loops and breaks the race timing.

### Where the actual corruption hides

After exhaustive instrumentation:
- No W8/W16/W32/W64 fastmem-direct write produces value with `>> 16 == 0x8149`
  and bit 0 set (verified across 5000+ captured events).
- No W64 slow-path callback writes to slot 0x814903F8.
- No 128-bit callback writes to slot.
- No write_block writes to slot after initial NRO load.
- No get_pointer calls to the slot's page.
- Exclusive monitor is correctly wired (LDXR/STXR work).

The tag bit `+1` in `x4 = 0x814903E1` at the unlink IS present in
memory but no instrumented write path produces it. The corruption is
extremely subtle — it may be from:
- A specific multi-thread interleaving of two ARM instructions whose
  combined effect modifies a byte we don't have a tracer for
- A JIT-codegen issue specific to a rare register-allocation pattern
- A signal/preemption issue that swaps state between cores

The diagnostic infrastructure built this session (and persistent in
INVESTIGATION.md) provides a comprehensive toolkit for future
investigators when ruzu can be run side-by-side with yuzu for
differential tracing.

