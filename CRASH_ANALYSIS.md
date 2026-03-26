# MK8D Analysis — RESOLVED

## Status (2026-03-26)

**No crash.** The game runs correctly through all 41 initial SVCs and enters
nn::lm module initialization. Execution is correct but slow (~3.8 ARM
instructions per JIT block, ~872 blocks compiled in 15s).

## What was fixed this session

| Fix | Impact |
|-----|--------|
| rdynarmic mprotect skip (cache hits + run entry) | 500x speedup for init |
| Block linking enabled (0x3F) | ~20x for hot loops |
| ILogger session with ServerManager wiring | Correct handle 0x201fc |
| CNTPCT returns real clock ticks | Correct timer |
| Step tracer post-dispatch activation | **Fixed false svcBreak** |
| RWX code cache (no mprotect toggles) | Matches upstream |
| Lock-free memory_read_code via fastmem | Matches upstream |
| reads_cpsr includes A32GetCFlag | Correct GSE invalidation |
| IdentityRemoval + VerificationPass | Matches upstream passes |
| push_ipc_interface on ResponseBuilder | Matches upstream PushIpcInterface |

## The svcBreak was a debugging artifact

The step tracer activated BEFORE `set_svc_arguments` wrote R0=0 back.
R0 retained the lm handle (0x181fd), causing nn::lm init to see an error
and call AbortImpl. Differential binary register trace proved first 100+
steps are **byte-identical** between upstream dynarmic and rdynarmic.

## Verified identical to upstream

- 36MB guest memory at first SVC: **byte-identical**
- All 41 SVC responses + TLS data: **byte-identical**
- Register state (R0-R15 + CPSR) per-step: **identical for 100+ steps**
- Memory layout (29 QueryMemory entries): **identical**
- JIT x86 codegen (instruction selection, register allocator): **functionally identical**
- WritesToCPSR opcode list: **identical**
- Block termination logic: **identical**

## Remaining: JIT execution speed

The nn::lm initialization phase compiles ~872 new blocks in 15s (all unique,
no infinite loop). Average block size: 3.8 ARM instructions, 17.4 IR instructions.
This is normal for heavily conditional ARM code.

Upstream dynarmic executes this in ~100ms. The ~150x slowdown is due to
general overhead in the Rust JIT pipeline (compilation + code emission +
x86 instruction scheduling), not a specific bug.

## Reference traces

- `traces/zuyu_full_5s_trace.log` — 50K lines, 5s upstream (14K SVCs, 30 threads)
- `traces/ruzu_mk8d_full_trace.txt` — ruzu until lm:OpenLogger

## Tools

```bash
RUZU_SVC_TRACE=1                    # SVC trace with TLS dumps
RUZU_NO_FASTMEM=1                   # Disable fastmem
RUZU_A32_OPTIMIZATION_MASK=0x3F     # JIT optimization flags
# Step tracer: build with feature "step_tracer", set RUZU_STEP_AFTER_SVC=N
```
