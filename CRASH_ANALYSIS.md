# MK8D Analysis — GAME HITS BKPT (ABORT) AFTER SDK INIT

## Status (2026-03-27)

**Game aborts.** After rtld + 40 SVCs (sm: connect, lm: OpenLogger), the game
enters SDK init code, progresses through ~22 preemption intervals (~220ms),
then executes **BKPT #0x5C** (Thumb 0xBE5C) at PC=0x01DFD6AC. This is
`__builtin_trap()` — an assertion failure in the SDK. The game loops on the
BKPT forever because the JIT doesn't raise a debug exception for it.

### Key addresses
- **0x01DFD6AC**: BKPT #0x5C (Thumb) — the abort point
- **0x01DF7E3C**: last valid code before abort (preempt #21, lr=0x01DF7858)
- CPSR=0xA0000020 (Thumb mode, N+C flags set)
- R0=4, R4=0x86814000 (kernel-space address — bad pointer?)

### Root cause: unknown assertion failure
The game's SDK init hits an assertion. Possible causes:
1. Missing/wrong IPC response (OpenLogger returned wrong format?)
2. Missing service that should have been initialized by another thread
3. Memory mapping issue (R4=0x86814000 is above ASLR range)
4. TLS corruption

### Session fixes (2026-03-27)
| Fix | Impact |
|-----|--------|
| CoreTiming timer thread deadlock | **Preemption now works** (150 callbacks/5s) |
| Vsync pipeline (Conductor+CoreTiming+KernelEventBridge) | 60Hz vsync events ready |
| ServerManager complete_sync_request | Real IPC dispatch wired |
| All 7 SVCs verified vs upstream | No mismatches found |

### Current execution sequence

1. rtld: relocations processed correctly (was broken by N/Z flag bug)
2. QueryMemory x29 — address space enumeration
3. GetInfo x2, GetThreadPriority, SignalProcessWideKey
4. ConnectToNamedPort("sm:") → handle 0x101fe
5. sm: QueryPointerBufferSize, Initialize, GetService("lm") → handle 0x181fd
6. lm: QueryPointerBufferSize, OpenLogger → ILogger handle 0x201fc
7. **Game binary enters**: 5 blocks at 0x1d3165c, 0x2010870, 0x1d0ad08, 0x1d376b8, 0x1df7e50
8. **Loops** — no new blocks compiled, no new SVCs

### Next investigation

The 5 game binary blocks need disassembly to identify what the code waits for.
Likely candidates: SleepThread, WaitSynchronization, CNTPCT timer loop, or
waiting for another thread (scheduler issue).

### Session fixes (2026-03-26 afternoon)

| Fix | Impact |
|-----|--------|
| GetNZFromOp: test+lahf instead of xor+lahf | **Fixed N/Z flags for all logic ops** |
| carry_in for logic imm rotate=0 | **Fixed C flag preservation** |
| corosensei fibers (replace ucontext) | ~50-250x faster context switch |
| CoreTiming timer thread + preemption event | 10ms periodic JIT interrupt |
| Scheduler update_highest_priority_threads callback | Thread distribution to cores |
| Two-pass GSE (FlagsPass + RegisterPass) | Upstream-parity IR optimization |
| replace_uses_with as Identity | Upstream-parity IR semantics |

### Root cause of rtld infinite loop (FIXED)

The rtld's relocation bytecode interpreter at 0x200440 looped on all-zero BSS
data because the relocation encoder never ran. The encoder's conditional branch
took the wrong path due to incorrect N/Z flags from `GetNZFromOp`, which used
`xor al, al; lahf` (always Z=1) instead of `test value, value; lahf`.

355/355 rdynarmic tests pass. 5000/5000 fuzz comparisons with upstream oracle pass.

## Previous status

**No crash.** Game runs correctly. Performance bottleneck identified and root
cause found: rdynarmic generates +49% more IR than upstream (17.4 vs 11.7
avg IR per block) due to redundant register reads/writes that the GSE pass
doesn't eliminate.

## Session fixes

| Fix | Impact |
|-----|--------|
| rdynarmic mprotect skip (cache hits + run entry) | 500x speedup for init |
| Block linking enabled (0x3F) | ~20x for hot loops |
| ILogger session with ServerManager wiring | Correct handle |
| CNTPCT returns real clock ticks | Correct timer |
| Step tracer post-dispatch activation | **Fixed false svcBreak** |
| RWX code cache (zero mprotect) | Matches upstream |
| Lock-free memory_read_code via cached fastmem | Matches upstream |
| reads_cpsr includes A32GetCFlag/A32GetCpsr | Correct GSE invalidation |
| IdentityRemoval + VerificationPass | Matches upstream passes |
| push_ipc_interface on ResponseBuilder | Matches upstream PushIpcInterface |
| Data processing reads only needed operands | -1 IR (MOV operand1) |

## Root cause: svcBreak was debugging artifact

The step tracer activated BEFORE `set_svc_arguments` wrote R0=0 back.
Differential register trace proved 100+ steps **byte-identical** to upstream.

## Performance bottleneck identified

### IR bloat: +49% more IR instructions per block

| Metric | Upstream | rdynarmic | Ratio |
|--------|----------|-----------|-------|
| Avg ARM insts/block | 4.4 | 3.8 | similar |
| **Avg IR insts/block** | **11.7** | **17.4** | **+49%** |
| Blocks compiled in 15s | 217,400 | 872 | 250x slower |

### Specific example: block 0x2009c0 (4 ARM instructions)

| | Upstream | rdynarmic (before) | rdynarmic (GSE rewrite) |
|---|---|---|---|
| IR instructions | **12** | **20** (+67%) | **12** (matched!) |

### Cause: redundant Set+Get pairs between ARM instructions

rdynarmic translates each ARM instruction independently:
```
GetRegister R0 → LSL → SetRegister R0 → GetRegister R0 → UXTAB → SetRegister R0 → ...
```

Upstream chains IR values directly:
```
GetRegister R0 → LSL → UXTAB (uses LSL result directly) → SetRegister R0
```

The **Get-Set Elimination (GSE) pass** was rewritten to match upstream's two-pass
approach and is now deployed.

### GSE rewrite (2026-03-26) — DONE

`replace_uses_with` now matches upstream `Inst::ReplaceUsesWith`: converts target
to `Identity(replacement)` instead of global search-replace + tombstone. The
IdentityRemovalPass (always run after optimization) chases through Identity
indirections.

The A32 GSE now implements upstream's exact two-pass approach:
- **FlagsPass** (reverse iteration): eliminates redundant CPSR flag ops, extracts
  C from NZCV via `GetCFlagFromNZCV`, downgrades `SetCpsrNZC` to `SetCpsrNZ`
  when C is identity
- **RegisterPass** (forward iteration): eliminates redundant register Get/Set pairs
  with multi-slot ext reg tracking (Single/Double/VectorDouble/VectorQuad)

353 tests pass (2 pre-existing failures unchanged).

### JIT performance comparison (20 first SVCs)

| Metric | Upstream (zuyu) | Ruzu | Ratio |
|--------|-----------------|------|-------|
| SVC 0 → SVC 19 (20 QueryMemory) | 2.87ms | 0.70ms | **ruzu 4x faster** |
| SVC 0 → SVC 28 (29 QueryMemory) | 3.13ms | 0.97ms | **ruzu 3x faster** |

**Conclusion:** The JIT is no longer the bottleneck. The 29 QueryMemory calls
execute in under 1ms. The game is now blocked on the **scheduler**
(`schedule_impl_fiber`) which fails to yield to the next guest thread after the
QueryMemory loop completes.

## Verified identical to upstream

- 36MB guest memory at first SVC: byte-identical
- 100+ step register trace: byte-identical
- All SVC responses + TLS: byte-identical
- Memory layout (29 QueryMemory entries): identical
- Block sizes: similar (4.4 vs 3.8 ARM insts)
- x86 codegen (instruction selection, regalloc): functionally identical

## Reference traces

- `traces/zuyu_full_5s_trace.log` — 50K lines, 5s upstream (14K SVCs, 30 threads)
- `traces/ruzu_mk8d_full_trace.txt` — ruzu SVC trace

## Tools

```bash
RUZU_SVC_TRACE=1                    # SVC trace with TLS dumps
RUZU_NO_FASTMEM=1                   # Disable fastmem
RUZU_A32_OPTIMIZATION_MASK=0x3F     # JIT optimization flags
# Step tracer: build with feature "step_tracer", set RUZU_STEP_AFTER_SVC=N
```
