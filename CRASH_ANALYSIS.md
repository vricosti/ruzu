# MK8D Crash Analysis

## Current status (2026-03-25)

Game calls `svcBreak(0, 0, 0)` during initialization, 63K ARM instructions
after receiving the ILogger handle from `lm:OpenLogger`.

## What's been verified IDENTICAL to upstream

- **QueryMemory walk**: 29 entries, all matching (base, size, state, perm)
- **All 41 SVC responses**: args + TLS data byte-identical
- **ILogger handle**: 0x201fc in both, TLS response byte-identical
- **NSO loader**: segment decompression, placement, BSS, page alignment all correct
- **Module addresses**: all 8 modules load at same addresses
- **JIT codegen**: x86 code manually verified correct for rtld hash loop
- **Fastmem**: data matches shared_memory at all tested addresses
- **Block manager**: memory regions match upstream exactly

## What differs

After receiving the ILogger handle, upstream makes these SVCs:
```
SVC[0041]: Close(type=2) on lm handle 0x181fd  ← ruzu never reaches this
SVC[0042]: CloseHandle(0x181fd)
SVC[0043]: sm:GetService("apm")
...continues normally...
```

Ruzu: after SVC[0040] (OpenLogger), executes 63K ARM instructions then calls
`svcBreak`. The game NEVER calls Close on the lm session.

## Performance fixes (resolved)

- **rdynarmic mprotect fix**: skip on cache hits + run() entry (500x speedup)
- **Block linking enabled**: 0x3F matching upstream
- **ILogger fix**: returns ILogger via move handle matching upstream
- MK8D init: **8.5 minutes → <1 second**

## Investigation tools

```bash
# SVC trace (all SVCs + TLS for SendSyncRequest):
RUZU_SVC_TRACE=1 RUST_LOG=off ./target/release/yuzu-cmd -g <rom> 2>trace.txt

# Disable fastmem for debugging:
RUZU_NO_FASTMEM=1

# Override JIT optimization flags:
RUZU_A32_OPTIMIZATION_MASK=0x3F  # all optimizations
RUZU_A32_NO_OPTIMIZATIONS=1      # zero optimizations

# svcBreak now calls process::exit(1) for clean termination
```

## Hypotheses for remaining bug

1. **SessionRequestManager wiring**: ILogger session created via `create_session_for_service`
   doesn't link to parent's ServerManager. Upstream's `PushIpcInterface` passes
   `manager->GetServerManager()` to the new manager. This might affect how the game's
   IPC framework validates the session before using it.

2. **Process state difference**: Some kernel state (thread local storage layout,
   process ID format, GetInfo sub-values) differs subtly, causing the nn SDK's
   initialization checks to fail.

3. **Font file loading**: Missing Nintendo font files logged as warnings during init.
   The game might check for font availability as part of its initialization sequence.

## Next steps

1. Compare `SessionRequestManager` construction between `create_session_for_service`
   and upstream's `PushIpcInterface` — specifically the ServerManager reference.

2. Add step-trace with detailed logging for the 50 instructions before svcBreak
   to find which ARM function triggers the abort.

3. Run upstream with GDB to capture the exact SVC sequence and ARM register state
   at the point where ruzu diverges.
