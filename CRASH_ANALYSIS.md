# MK8D Crash Analysis

## Current status

The game **calls `svcBreak` (fatal abort)** during initialization.
It is NOT a JIT bug, NOT an infinite loop, NOT a scheduler issue.

## Root cause

IPC response divergences between ruzu and upstream zuyu cause the game
to receive incorrect data from HLE services. The game detects the error
and aborts via `svcBreak(0, 0, 0)` after calling
`SetTerminateResult(0x801f2)` (vi module error code).

## Evidence

### SVC trace comparison

Reference traces generated with `RUZU_SVC_TRACE=1`:
- **Upstream zuyu**: runs 60s successfully, 234K+ SVCs, reaches Vulkan pipeline caching
- **Ruzu**: aborts at 176 SVCs with `svcBreak`

### Abort sequence (step-traced)

```
step 45124: SVC #0x1d (SignalProcessWideKey)
step 55232: SVC #0x21 (SendSyncRequest) handle=0x281fc → IApplicationFunctions cmd=22 (SetTerminateResult)
step 55444: SVC #0x1d (SignalProcessWideKey)
step 63138: SVC #0x26 (Break) → svcBreak(0, 0, 0) — GAME ABORTS
```

The game calls `SetTerminateResult(0x801f2)` to record the error, then aborts.
After the abort, the game's crash handler corrupts SP/LR (SP=0xfffffff0, LR=0x0),
which was initially mistaken for a JIT bug.

### IPC response divergences found

Comparing TLS response dumps between upstream and ruzu:

1. **Extra IPC calls in ruzu** — ruzu makes additional `sm:RegisterClient` and
   `sm:GetService("lm")` calls that upstream doesn't, causing all subsequent
   session handles to be shifted by one.

2. **Handle numbering diverges**:
   - Upstream: `0x201fc`, `0x281fd`, `0x301fb`, ...
   - Ruzu:     `0x181fd`, `0x201fd`, `0x281fc`, ...

3. **Domain response data differs** — at least one domain IPC response returns
   `0x0007d402` in upstream but `0x0` in ruzu. This appears to be in a
   service call response that returns a non-trivial value.

4. **NVDrv response value differs** — upstream returns `2` where ruzu returns
   `1` in what appears to be an nvdrv file descriptor or session count.

## Service call sequence before abort

```
 1. ConnectToNamedPort("sm:") → handle 0x101fe
 2. sm: QueryPointerBufferSize → 0x8000
 3. sm: RegisterClient
 4. sm: GetService("lm") → logger service
 5. sm: GetService("apm") → performance service
 6. sm: GetService("appletOE") → applet service
 7. appletOE: ConvertToDomain → domain mode
 8. appletOE: domain[1] cmd=0 → OpenApplicationProxy → IApplicationProxy (domain obj 2)
 9. IApplicationProxy: GetApplicationFunctions (obj 3), GetLibraryAppletCreator (obj 4),
    GetCommonStateGetter (obj 5), GetSelfController (obj 6), GetWindowController (obj 7),
    GetAudioController (obj 8), GetDisplayController (obj 9), GetDebugFunctions (obj 10)
10. ICommonStateGetter: cmd 0, 1, 9
11. IWindowController: cmd 1, 10
12. ISelfController: cmd 13
13. sm: GetService("pctl:a") → parental control
14. pctl:a: ConvertToDomain, cmd 0
15. sm: GetService("fsp-srv") → filesystem
16. fsp-srv: ConvertToDomain, cmd 1 (SetCurrentProcess), cmd 200 (OpenDataStorageByCurrentProcess),
    IStorage cmd 0 (Read), cmd 203, cmd 1005, cmd 200 again, more reads
17. sm: GetService("nvdrv") → nvidia driver
18. nvdrv: QueryPointerBufferSize, Initialize, SetAruid, CloneCurrentObjectEx,
    Open device, Ioctl1(fd=1, 0xC183001B)
19. sm: GetService("vi:m"), GetService("vi:s"), GetService("vi:u")
20. vi:m: QueryPointerBufferSize, GetDisplayService → IApplicationDisplayService (handle 0x981ef)
21. *** ABORT: SetTerminateResult(0x801f2) + svcBreak(0,0,0) ***
```

## How to reproduce the trace

```bash
# Ruzu trace:
RUZU_SVC_TRACE=1 RUST_LOG=off cargo run --release --bin yuzu-cmd -- \
  -g "Mario Kart 8 Deluxe.nsp" 2>traces/ruzu_trace.txt

# Upstream trace (requires rebuilding zuyu with ZUYU_SVC_TRACE support):
ZUYU_SVC_TRACE=1 zuyu-cmd -g "Mario Kart 8 Deluxe.nsp" 2>traces/zuyu_trace.txt

# Compare:
diff <(grep "TLS_RSP" traces/zuyu_trace.txt | head -N) \
     <(grep "TLS_RSP" traces/ruzu_trace.txt)
```

## Next steps (in priority order)

1. **Fix extra IPC calls** — find why ruzu makes extra sm:RegisterClient/GetService("lm")
   calls that upstream doesn't. This shifts all handle numbers.

2. **Fix domain response data** — the `0x0007d402` vs `0x0` divergence in a domain
   response. Check which service returns this and why ruzu returns zero.

3. **Fix nvdrv ioctl response** — the fd/count `2` vs `1` difference.

4. After fixing these, re-run the trace comparison and iterate.

## Performance (resolved)

JIT performance issues are fixed:
- **rdynarmic mprotect fix**: skip `mprotect` on block cache hits (500x speedup)
- **Block linking enabled**: `optimization_flags_from_mask(0x3F)` matching upstream default
- MK8D init: **8.5 minutes → <1 second**
