# MK8D Crash Analysis — 2026-03-20

## Summary

Mario Kart 8 Deluxe (AArch32, title ID `0100152000022000`) crashes during boot with a `PREFETCH_ABORT` after completing 176 SVCs. The crash occurs in guest code immediately after the `vi:m GetDisplayService` IPC call returns successfully. No SVC or IPC error triggers the crash — it is a null pointer dereference in the game's nn::vi SDK code.

## Crash Site

```
PREFETCH_ABORT at PC=0x16b8588, SP=0xffffffec, LR=0x0
  R0=0x0 R1=0x0 R2=0x0 R3=0x0 R4=0x0 R5=0x1 R6=0x53 R7=0xeb000fc8
  R8=0x2499de0 R9=0x1 R10=0x523a3218 R11=0x2499dc0 R12=0x0
```

- **PC=0x16b8588** is in `subsdk1` module (.text at 0x16AB000-0x16BD000), offset 0xD588
- **SP=0xffffffec** is corrupted (should be ~0x2499xxx based on prior IPC calls)
- **LR=0x0** means caller cannot be traced
- **R5=0x1** is not a valid pointer (used as `this` in virtual call)
- **R7=0xeb000fc8** is garbage loaded from corrupted stack (`LDR R7, [SP, #0x80]`)

### Crashing Instructions

```arm
016b857c: LDR R0, [R5]         ; R0 = *(0x1) -> unmapped read, returns 0
016b8580: LDR R1, [R0, #8]     ; R1 = *(0x0 + 8) -> unmapped, returns 0
016b8584: MOV R0, R5           ; R0 = 0x1 (this pointer)
016b8588: BLX R1               ; BLX 0x0 -> PREFETCH_ABORT
```

This is a C++ virtual method call on a null/invalid object pointer (R5=0x1).

## Boot Sequence (176 SVCs)

The game progresses through the full service initialization before crashing:

```
SVC #1-29:    QueryMemory scan (29 entries) — identical to zuyu reference
SVC #30-31:   QueryMemory (stack + code verification)
SVC #32:      SetHeapSize(0x78000000) -> heap at 0x40000000
SVC #33-60:   sm:GetService for lm, apm, appletOE, aoc:u, pctl:a
              appletOE domain setup: OpenApplicationProxy,
              GetApplicationFunctions, GetLibraryAppletCreator,
              GetCommonStateGetter, GetSelfController, GetWindowController,
              GetAudioController, GetDisplayController, GetDebugFunctions
              ICommonStateGetter: GetEventHandle, ReceiveMessage, GetCurrentFocusState
              IWindowController: GetAppletResourceUserId, AcquireForegroundRights
              ISelfController: SetAutoSleepTimeAndDimmingTimeEnabled
SVC #61-80:   pctl:a domain setup, CloneCurrentObject
SVC #81-120:  fsp-srv domain setup: SetCurrentProcess,
              OpenDataStorageByCurrentProcess (x2), IStorage::Read (x6+),
              OpenPatchDataStorageByCurrentProcess (error->null domain obj),
              GetGlobalAccessLogMode, CloseVirtualHandle
SVC #121-140: nvdrv: QueryPointerBufferSize, Initialize, SetAruid,
              CloneCurrentObjectEx, Open, Ioctl1 (NvOsGetConfigU32)
SVC #141-170: sm:GetService for vi:m, vi:s, vi:u
              SignalProcessWideKey (x70 total across execution)
SVC #171-176: vi:m QueryPointerBufferSize, GetDisplayService(cmd 2)
              -> returns IApplicationDisplayService handle 0x981ef
              *** CRASH immediately after SVC #176 return ***
```

## Key Comparison with Zuyu Reference

### What matches (identical behavior)

| Aspect | Zuyu | Ruzu | Match? |
|--------|------|------|--------|
| QueryMemory scan (29 entries) | Same addresses/sizes/states | Identical | YES |
| SetHeapSize result | heap at 0x40000000 | Same | YES |
| Service lookup order | lm, apm, appletOE, aoc:u, pctl:a, fsp-srv, nvdrv, vi:m/s/u | Same | YES |
| appletOE sub-services | All 9 commands dispatched | Same | YES |
| fsp-srv OpenPatchDataStorage | Returns error 0x7d402 | Same | YES |
| nvdrv Ioctl NvOsGetConfigU32 | Returns ConfigVarNotFound | Same | YES |
| vi:m GetDisplayService response | move handle in TLS `[0x0, 0x8000000a, 0x20, handle, SFCO, ...]` | Same format | YES |

### What diverges

| Aspect | Zuyu | Ruzu | Impact |
|--------|------|------|--------|
| Pre-JIT initialization | Glue::TimeManager calls set:sys (14 calls) | No TimeManager, set:sys was stub | Missing clock init |
| Audio backend | "Auto-selecting cubeb" logged | No AudioCore creation | Cosmetic |
| ConvertToDomain count | 8 conversions | 3 conversions | Game takes different code path |
| vi:m domain mode | ConvertToDomain BEFORE GetDisplayService | No ConvertToDomain, calls non-domain | Different response format |
| Post-vi:m behavior | Game continues to GetManagerDisplayService, OpenDisplay, etc. | Game crashes immediately | **THE BUG** |

### Critical Divergence: Domain Mode on vi:m

In zuyu, after `GetService("vi:m")`:
1. `QueryPointerBufferSize` (Control cmd 3)
2. **`ConvertCurrentObjectToDomain`** (Control cmd 0)
3. `GetDisplayService` (Request cmd 2, **domain mode**)
4. `GetManagerDisplayService` (domain cmd 102 on returned object)
5. Game continues...

In ruzu, after `GetService("vi:m")`:
1. `QueryPointerBufferSize` (Control cmd 3)
2. `GetDisplayService` (Request cmd 2, **non-domain mode**)
3. **CRASH** — null pointer dereference

The game **skips ConvertToDomain** in ruzu and calls GetDisplayService directly in non-domain mode. The response is a move handle instead of a domain object. The game's SDK code may not handle the non-domain response correctly.

## Stack Corruption Analysis

SP=0xffffffec at crash time is not from the vi:m response — it was already wrong when the crashing function started. The function loads R7 from `[SP+0x80]` = `[0x6C]` (unmapped), gets garbage, and uses it as a loop limit.

The stack corruption happens **in guest code between the last SVC return and the crash**. No SVCs execute between them. This points to:

1. A JIT (rdynarmic) bug that incorrectly modifies SP during ARM32 execution
2. Guest code writing past the stack boundary
3. The game's IPC client code taking a wrong branch due to the non-domain response, leading to use of an uninitialized stack frame

## IPC Response Fixes Applied

Several IPC response bugs were found and fixed during investigation:

| Fix | Commit | Description |
|-----|--------|-------------|
| Auto-stub for unimplemented handlers | 19db364 | `report_unimplemented_function` now writes stub success response |
| Null domain objects | 922f148 | `Vec<Option<SessionRequestHandlerPtr>>` for error+OutInterface |
| TLS writeback for CloseVirtualHandle | 922f148 | `write_to_outgoing_command_buffer` called after all dispatch paths |
| set:sys command IDs | 060f24e | All command IDs fixed to match upstream (many were off by 4-26) |
| Domain dispatch fallthrough | d14c29a | Matches upstream fallthrough behavior for domain sessions |

## Services Implemented

| Service | Status | Notes |
|---------|--------|-------|
| sm: | Real | Service manager with GetService/RegisterService |
| set:sys | Real | ISystemSettingsServer with correct cmd IDs (060f24e) |
| appletOE | Real | Full IApplicationProxy with 9 sub-services |
| fsp-srv | Real | With IStorage, domain support, null domain objects on error |
| nvdrv | Real | Initialize, SetAruid, Open, Ioctl1 |
| vi:m/vi:s/vi:u | Real | Correct cmd IDs (0/1/2), Container, IApplicationDisplayService |
| IHOSBinderDriver | Real | TransactParcel, AdjustRefcount, GetNativeHandle, TransactParcelAuto |
| pctl:a | Stub | Generic stub |
| lm, apm | Stub | Generic stubs |

## Hypotheses for Root Cause

### H1: Missing ConvertToDomain causes wrong IPC response format (LIKELY)

The game expects vi:m to be domain-converted before calling GetDisplayService. Without domain mode, the response uses a move handle instead of a domain object. The game's SDK IPC client code may misparse the non-domain response, storing a null pointer that later causes the crash.

**Why ConvertToDomain is skipped in ruzu:** Unknown. The game's SDK should call Control cmd 0 on vi:m regardless. One possibility: an earlier IPC response (e.g., from appletOE or pctl) had different data that caused the game's service initialization code to take a different path that skips ConvertToDomain.

### H2: JIT SP corruption in rdynarmic (POSSIBLE)

An ARM32 instruction in the JIT-compiled guest code incorrectly modifies SP. This would be an rdynarmic bug. Evidence: SP=0xffffffec is not a valid stack address and was not set by any SVC.

### H3: Missing Glue::TimeManager initialization (CONTRIBUTING)

The 14 set:sys calls that happen in zuyu's pre-JIT initialization never happen in ruzu because Glue::Time::TimeManager is not constructed. This means system clock state is uninitialized. The game's SDK may check clock state during vi initialization and take an error path.

## Reproduction

```bash
mkdir -p /tmp/ruzu-data/ruzu/keys /tmp/ruzu-data/yuzu/keys
cp ~/.local/share/yuzu/keys/* /tmp/ruzu-data/ruzu/keys/
cp ~/.local/share/yuzu/keys/* /tmp/ruzu-data/yuzu/keys/
env XDG_DATA_HOME=/tmp/ruzu-data \
    XDG_CACHE_HOME=/tmp/ruzu-cache \
    XDG_CONFIG_HOME=/tmp/ruzu-config \
    RUST_LOG=info \
    cargo run --release --bin yuzu-cmd -- -g "/path/to/Mario Kart 8 Deluxe [0100152000022000][v0].nsp"
```

## Next Steps

1. **Investigate why ConvertToDomain is skipped** — Compare the exact IPC responses for pctl:a and fsp-srv between zuyu and ruzu. The game's SDK may check a response value to decide whether to use domain mode for vi.

2. **Implement Glue::Time::TimeManager** — Port the constructor that calls set:sys + time:m 14 times during service startup. This matches zuyu's pre-JIT initialization sequence.

3. **Test rdynarmic SP handling** — Add SP validation in the JIT's SVC callback to check if SP gets corrupted during guest execution. Compare SP values between SVCs.

4. **Compare arm register state at vi:m call time** — Dump full register state when vi:m GetDisplayService is called in both zuyu and ruzu to find which register/memory value differs.
