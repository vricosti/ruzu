# MK8D Crash Analysis

## Current status (2026-03-26)

Game calls `svcBreak(0, 0, 0)` at ARM step 63408, during `nn::lm` module
initialization, after receiving a valid ILogger handle from `lm:OpenLogger`.

## Root cause: Unknown — likely rdynarmic JIT codegen bug

Every observable state is **byte-identical** between upstream zuyu and ruzu.
The game starts from the same memory, receives the same SVC responses, and
has the same register values at every SVC boundary. Yet it aborts in ruzu
but succeeds in upstream.

### What has been proven IDENTICAL

| What | Method | Result |
|------|--------|--------|
| Guest memory (36MB) | Binary dump at first SVC | **Byte-identical** |
| QueryMemory walk (29 entries) | SVC trace comparison | **Identical** |
| All 41 SVC responses (R0-R7) | Register dump at each SVC | **Identical** |
| TLS data for 5 IPC calls | TLS dump comparison | **Byte-identical** |
| Memory at `[0x22de3dc]` (flag byte) | Memory probe at each SVC | **0x00 in both** |
| EXPH heap magic at `[0x22de3a8]` | Memory probe | **0x45585048 in both** |
| NSO module segments | Loader comparison | **Identical sizes/offsets** |
| Module load addresses | Log comparison | **8 modules at same addresses** |
| Thread priority | SVC comparison | **0x2c in both** |
| TPIDR (TLS pointer) | Code review | **Set correctly** |

### Abort sequence (step-traced)

```
Step     0: SVC#40 returns (lm:OpenLogger) — PC=0x1d3c7c0, R0=0x181fd
Step 10694: Enter nn::lm init at PC=0x1d09fcc — R0=0x22de3a8 (heap), R1=0x22de438
Step 10699: LDRB R0,[R4,#0x34] → R0=0x00 (correct, same as upstream)
Step 10703: BL init_function — enters SDK init via PLT
Step 15585: Game reads TLS addr (0x2395200), thread handle (0x81ff), lm handle (0x181fd)
Step 44933: SVC 0x1d (SignalProcessWideKey)
Step 55702: SVC 0x1d (SignalProcessWideKey)
Step 63408: SVC 0x26 (svcBreak) — ABORT
```

### Symbol resolution trace before abort

Normal symbols resolved: `ServiceObjectImplBase2::ReleaseImpl`, `FreeToExpHeap` (×2).
Then the abort handler chain: `AbortImpl` → `VAbortImpl` → `InvokeAbortObserver` →
`DefaultAbortObserver` → `GetAbortObserverManager` → `Abort` → `svc::aarch32::Break`.

### What upstream does differently after lm:OpenLogger

```
Upstream: OpenLogger → Close(lm) → CloseHandle → GetService("apm") → ...continues
Ruzu:     OpenLogger → [63K ARM instructions] → SignalProcessWideKey ×2 → svcBreak
```

## Performance fixes (resolved)

- **rdynarmic mprotect fix**: skip on cache hits + run() entry (500x speedup)
- **Block linking**: 0x3F matching upstream default
- **ILogger**: returns ILogger via move handle with ServerManager wiring
- **CNTPCT**: A32 returns CoreTiming clock ticks
- MK8D init: **8.5 minutes → <1 second**

## Reference traces

- `traces/zuyu_full_5s_trace.log` — 50,731 lines, 5s of upstream MK8D
  - 14,209 SVCs, 2,717 IPC calls, 14,112 scheduler switches, 30 threads
- `traces/ruzu_mk8d_full_trace.txt` — 107 lines, ruzu until svcBreak
  - 42 SVCs, first 40 identical to upstream

## Next step: A32 differential oracle

The `a32_oracle` at `zuyu/build/a32_oracle` can execute ARM instructions
step-by-step using upstream dynarmic's C++ interpreter. Wire it into ruzu's
step tracer to compare register state after each of the 63K instructions
between lm:OpenLogger and svcBreak. The first divergence will identify
the miscompiled ARM instruction.

## Investigation tools

```bash
# SVC trace:
RUZU_SVC_TRACE=1 RUST_LOG=off ./target/release/yuzu-cmd -g <rom> 2>trace.txt

# Disable fastmem:       RUZU_NO_FASTMEM=1
# Override optimizations: RUZU_A32_OPTIMIZATION_MASK=0x3F
# svcBreak exits cleanly via process::exit(1)
```
