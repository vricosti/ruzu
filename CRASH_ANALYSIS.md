# MK8D Analysis — NO CRASH, JIT PERFORMANCE REMAINING

## Status (2026-03-26)

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

The **Get-Set Elimination (GSE) pass** should eliminate these but the current
implementation doesn't handle:
1. **FlagsPass** needs reverse iteration (upstream does, rdynarmic doesn't)
2. **RegisterPass** needs simpler value propagation without tracking_type check
3. **Dead store elimination** for intermediate SetRegister instructions

### GSE rewrite status

A rewrite matching upstream's two-pass approach (FlagsPass reverse + RegisterPass
forward) was implemented and **achieved 12 IR for block 0x2009c0** (exact upstream
parity). However, it introduced a regression (29 SVCs vs 41 in 2 minutes) indicating
a bug in the propagation logic. **Reverted** — needs debugging with unit tests.

### Next steps for GSE fix

1. Write unit tests for the GSE pass using known block IR patterns
2. Debug the flags_pass — likely tombstones an instruction still referenced
3. Debug the register_pass — verify dead store elimination doesn't break deps
4. Verify with block 0x2009c0 + other blocks before deploying

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
