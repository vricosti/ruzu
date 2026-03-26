# MK8D Analysis — RTLD FIXED, BLOCKED ON HLE SERVICES

## Status (2026-03-26)

**Major progress.** Game now passes rtld relocation processing and reaches HLE
service initialization (ConnectToNamedPort, SendSyncRequest). 82 SVCs in 30s
(was 58, stuck on rtld bytecode interpreter). Next blocker is HLE service
responses.

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
