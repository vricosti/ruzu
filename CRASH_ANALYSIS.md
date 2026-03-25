# MK8D Crash Analysis

## Current status (2026-03-26)

Game calls `svcBreak(0, 0, 0)` during `nn::lm` module initialization,
63K ARM instructions after receiving the ILogger handle from `lm:OpenLogger`.

## Root cause: nn::lm runtime assertion failure

The abort is a deliberate `nn::diag::detail::AbortImpl` call, NOT a symbol
lookup failure or memory corruption. The nn SDK's logger initialization code
performs a runtime check that fails.

### Symbol resolution trace (step-traced)

rtld resolves these symbols in order before the abort:

| # | Symbol | Purpose |
|---|--------|---------|
| 1 | `nn::sf::impl::detail::ServiceObjectImplBase2::ReleaseImpl` | ILogger interface release |
| 2 | `nn::lmem::FreeToExpHeap` | Heap free |
| 3 | `nn::lmem::detail::FreeToExpHeap` | Heap free (detail) |
| 4 | `nn::diag::detail::AbortImpl` | **Abort handler** |
| 5 | `nn::diag::detail::VAbortImpl` | Abort variant |
| 6-11 | `InvokeAbortObserver`, `DefaultAbortObserver`, etc. | Abort chain |
| 12 | `nn::svc::aarch32::Break` | svcBreak wrapper |

Symbols 1-3 are resolved normally. Symbol 4+ are the **abort handler chain**
— the game already decided to abort and is setting up the crash path.

### ARM instruction trace before svcBreak

```
step 63374: PC=0x201b90  — rtld lookup returned (R0 has result)
step 63375: R0=0x00000000 — lookup returned NULL → NOT FOUND
step 63376: back to rtld dispatcher
...
step 63389: R0=0x01d504d0 — resolves to svcBreak wrapper address
step 63396: R0=0x00000001 — some flag/check
step 63406: LR=0x01ce5a00 — abort handler address set
step 63407: PC=0x01d504d0 — jump to svcBreak wrapper
step 63408: SVC #0x26 — svcBreak fires
```

## What's been verified IDENTICAL to upstream

- QueryMemory walk: 29 entries, all matching
- All 41 SVC responses: args + TLS data byte-identical
- ILogger handle: 0x201fc in both
- NSO loader: segments, BSS, page alignment correct
- Module addresses: all 8 modules at same addresses
- JIT codegen: x86 code manually verified for rtld hash loop
- Fastmem: data matches shared_memory
- Block manager: memory regions identical
- System NCAs/fonts: tested with yuzu's data, no change
- CNTPCT: now returns real time (was returning 0, fixed)

## Performance fixes (resolved)

- **rdynarmic mprotect fix**: skip on cache hits + run() entry (500x speedup)
- **Block linking enabled**: 0x3F matching upstream
- **ILogger fix**: returns ILogger via move handle matching upstream
- **CNTPCT fix**: A32 get_cntpct now returns CoreTiming clock ticks
- MK8D init: **8.5 minutes → <1 second**

## Hypotheses for remaining bug

1. **nn::lm initialization runtime check**: The game's lm module initialization
   performs some check after receiving the ILogger handle. This check fails in
   ruzu but passes in upstream. Since there are NO IPC calls between OpenLogger
   and the abort, the check is purely in-process — reading some memory value
   or checking some process-local state.

2. **Heap initialization**: The symbols `FreeToExpHeap` suggest the game's
   heap allocator is being set up. If `SetHeapSize` (SVC 0x01) wasn't called,
   or the heap region isn't properly configured, heap allocation fails and
   the nn SDK aborts.

3. **Missing SVC**: The game might call an SVC between OpenLogger and the
   abort that we don't handle correctly. The SVC trace shows no calls, but
   in run mode SVCs inside the JIT might not trigger properly (though step
   mode confirmed only SignalProcessWideKey × 2 then Break).

## Investigation tools

```bash
# SVC trace:
RUZU_SVC_TRACE=1 RUST_LOG=off ./target/release/yuzu-cmd -g <rom> 2>trace.txt

# Step trace after SVC N:
RUZU_STEP_AFTER_SVC=40

# Disable fastmem:
RUZU_NO_FASTMEM=1

# Override JIT optimizations:
RUZU_A32_OPTIMIZATION_MASK=0x3F
RUZU_A32_NO_OPTIMIZATIONS=1
```

## Next steps

1. Trace the exact ARM function that performs the failing check (between steps
   ~1246 and ~1297 where resolution switches from FreeToExpHeap to AbortImpl).
   Add step logging for this range.

2. Check if `SetHeapSize` (SVC 0x01) is called BEFORE the lm initialization.
   In the current trace it appears MUCH later — maybe it needs to happen first.

3. Compare the game's heap state between upstream and ruzu at the point of
   the abort.
