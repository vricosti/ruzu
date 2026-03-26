# MK8D Crash Analysis — RESOLVED

## Resolution (2026-03-26)

**The svcBreak was caused by the step tracer debugging tool, NOT by any
emulation bug.** The step tracer activated BEFORE `set_svc_arguments` wrote
R0=0 back to the JIT, so R0 retained the lm handle (0x181fd). The nn::lm
init code checked R0, saw non-zero (error), and called AbortImpl.

Differential binary register trace (17×u32 per step) confirmed:
**first 100+ steps are IDENTICAL between upstream dynarmic and rdynarmic.**

## Current status

Game runs correctly past lm:OpenLogger. No svcBreak. The nn::lm init
code takes >60s in run mode due to JIT performance (millions of ARM
instructions for rtld symbol resolution). The game IS progressing — it
just needs more time or JIT optimization.

## Performance fixes applied

| Fix | Speedup |
|-----|---------|
| rdynarmic mprotect skip (cache hits + run entry) | 500x |
| Block linking (0x3F) | ~20x |
| ILogger session with ServerManager wiring | correctness |
| CNTPCT returns real clock ticks | correctness |
| Step tracer post-dispatch activation | **fixed false abort** |

## What was proven identical to upstream

- 36MB guest memory: byte-identical
- All SVC responses: byte-identical
- Register state at SVC boundaries: identical
- Binary register trace (100+ steps): identical
- Memory layout (29 QueryMemory entries): identical

## Reference traces

- `traces/zuyu_full_5s_trace.log` — 50K lines upstream reference
- `traces/ruzu_mk8d_full_trace.txt` — ruzu SVC trace

## Tools

```bash
RUZU_SVC_TRACE=1      # SVC trace with TLS dumps
RUZU_NO_FASTMEM=1     # Disable fastmem
RUZU_A32_OPTIMIZATION_MASK=0x3F  # JIT optimization flags
# Step tracer: build with feature "step_tracer", set RUZU_STEP_AFTER_SVC=N
```
