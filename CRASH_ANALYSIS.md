# MK8D Crash Analysis — 2026-03-18

## Summary

Mario Kart 8 Deluxe (AArch32, title ID `0100152000022000`) crashes during boot with a userspace `Break` SVC. The crash originates in the **rtld** (runtime linker) during NintendoSDK libc initialization (`__nnDetailInitLibc0`).

## Error codes

- `SetTerminateResult(0xe401)` — the game's self-reported fatal error code
- `SVC Break(0, 0, 0)` — userspace panic, no recovery

## Abort info struct (at r4=0x3c9a000)

| Offset | Value        | Meaning                                              |
|--------|-------------|-------------------------------------------------------|
| +0x00  | 0x00ea9708  | Code pointer (abort handler function in main module)  |
| +0x04  | 0x00000000  | (unused)                                              |
| +0x08  | 0x00000000  | (unused)                                              |
| +0x0C  | 0x00000000  | (unused)                                              |
| +0x10  | 0x00e38110  | Code pointer (assert call site in main module)        |
| +0x14  | 0x00d61761  | String pointer → **"RootHeap"** (heap name)           |
| +0x18  | 0x00ea977c  | Code pointer (abort info builder in main module)      |
| +0x1C  | 0x03c9a000  | Self-pointer (abort struct address)                   |
| +0x20  | 0x61200000  | Parameter: heap-related address/size                  |
| +0x30  | 0x00000001  | Error flag                                            |

## Boot sequence timeline

```
SVC #1-45:   sm, lm, apm, appletOE service setup — all succeed
SVC #46-87:  appletOE IPC (OpenApplicationProxy, GetSelfController,
             GetWindowController, GetCommonStateGetter, etc.) — all succeed
SVC #88:     pctl:a domain setup — succeeds
SVC #89:     SetHeapSize(0x78000000) → heap at 0x249a000 — succeeds
SVC #90-134: SignalProcessWideKey ×45 — NintendoSDK internal module init
             (nn::os, nn::fs, nn::lmem, nn::nv, etc.)
             *** ABORT HAPPENS HERE — between SVC #134 and #135 ***
             No IPC calls, no SVC errors — purely userspace code failure
SVC #135:    SetThreadPriority(0x81ff, 44) — abort handler thread setup
SVC #136:    GetThreadId — abort handler continues
SVC #137-141: GetInfo, SetThreadCoreMask, GetCurrentProcessorNumber,
              SignalProcessWideKey ×2 — abort handler thread operations
SVC #142-162: fsp-srv and filesystem setup — main thread continues
SVC #163-166: nvdrv open + Initialize — main thread continues
SVC #167:    CloseHandle — cleanup
SVC #168:    SignalProcessWideKey
SVC #169:    SetTerminateResult(0xe401) — game reports fatal error
SVC #170:    SignalProcessWideKey
SVC #171:    Break(0, 0, 0) — userspace panic, execution stops
```

## Root cause: `__nnDetailInitLibc0` failure in rtld

### What happens

The rtld (runtime linker, loaded at 0x200000) iterates through each loaded module's `.init_array` and calls `__nnDetailInitLibc0` — the NintendoSDK libc initialization entry point. One of these calls **fails**, returning a non-null abort handler pointer.

### Instruction-level trace of the abort decision

The ring-buffer trace captured the last 500 instructions before the abort handler entry at PC=0x1d31d94. The critical sequence:

```
[472] PC=0x200908: CMP R0, #0          ; check init function return value
                   R0=0x207c850        ; NON-NULL = module descriptor with abort info
[473] PC=0x20090c: BEQ skip            ; NOT taken (R0 != 0)
[474] PC=0x200910: LDRB R1, [R0, #0xC] ; R1 = module_desc->version = 0x12
[475] PC=0x200914: CMP R1, #0x10       ; version >= 0x10?
[476] PC=0x200918: BCS abort_path      ; YES → enter abort path

; Resolve abort function from module descriptor
[477] PC=0x200930: LDR R1, [R6, #0x10] ; R1 = module_base = 0x1c9c000 (subsdk4)
[478] PC=0x200934: LDR R0, [R0, #0x4]  ; R0 = abort_func_offset = 0x95d94
[479] PC=0x200938: ADD R4, R0, R1      ; R4 = 0x95d94 + 0x1c9c000 = 0x1d31d94
                                        ;    = nn::diag::detail::AbortImpl (absolute)

; Store abort function pointer and return error
[486] PC=0x201040: STR R0, [R9]        ; store abort func ptr in result slot
[487] PC=0x201044: MOV R0, #1          ; return 1 (error)
```

### What `__nnDetailInitLibc0` does

`__nnDetailInitLibc0` (at main module sym[859], offset 0x29c, absolute 0x20629c) initializes:

1. **TLS (Thread Local Storage)** — reads `TPIDRURO` (CP15 c13,c0,3) for the thread-local pointer
2. **Heap descriptors** — sets up `nn::lmem` root heap with the name "RootHeap"
3. **C runtime** — initializes `.bss` zero-fill, C++ static constructors
4. **Thread-local variables** — allocates TLS slots for SDK modules

### Why it fails

The function fails in **pure userspace code** — no SVCs return errors, all heap memory is accessible (verified with read/write tests at all offsets including 0x61200000). Possible causes:

1. **TLS/Thread pointer register** — `TPIDRURO` (CP15 c13,c0,3) may not be correctly initialized. The NintendoSDK reads this register to locate thread-local storage. If it returns 0 or an invalid pointer, the libc init fails.

2. **CP15 system register** — The SDK may read other coprocessor registers (e.g., cache type, MIDR, memory model) and abort if they return unexpected values.

3. **Memory layout mismatch** — The libc init may verify the process memory layout (heap region, stack region, code region) against expected values derived from the process's NPDM metadata.

## Eliminated causes

| Hypothesis | Status | Evidence |
|-----------|--------|----------|
| Heap memory inaccessible | **ELIMINATED** | Write/readback test at 0x249a000, 0x61200000, 0x7A000000 all pass |
| SVC returns error | **ELIMINATED** | No SVC returns non-zero before the crash |
| Missing IPC service | **ELIMINATED** | All services (sm, lm, apm, appletOE, pctl:a) respond successfully |
| nvdrv Initialize failure | **ELIMINATED** | nvdrv isn't even opened until after the abort |
| GetCurrentProcessorNumber stub | **SECONDARY** | Returns 0 always, but called after abort already started |

## Loaded modules

| Module    | Base       | End        | Has `__nnDetailInitLibc0` |
|-----------|-----------|-----------|---------------------------|
| rtld      | 0x200000  | 0x206000  | No (it's the linker)      |
| main      | 0x206000  | 0x1512000 | **Yes** (sym[859])        |
| subsdk0   | 0x1512000 | 0x16AB000 | No                        |
| subsdk1   | 0x16AB000 | 0x16D3000 | No (has NvRm symbols)     |
| subsdk2   | 0x16D3000 | 0x16E6000 | No                        |
| subsdk3   | 0x16E6000 | 0x1723000 | No                        |
| subsdk4   | 0x1723000 | 0x1C9C000 | No (has AbortImpl)        |
| sdk       | 0x1C9C000 | 0x2391000 | Not checked               |

## Next steps

1. **Trace `__nnDetailInitLibc0` execution** — Set `RUZU_STEP_AFTER_SVC` to trace from the beginning of module init (before SVC #89/SetHeapSize). The function is at absolute address 0x20629c. Add a breakpoint or ring-buffer trigger on that PC to capture exactly which instruction causes the failure.

2. **Check TPIDRURO/TLS setup** — Verify that the thread pointer register (CP15 c13,c0,3 / TPIDRURO) is set to the correct TLS base address before module init runs. The emulator sets it via `SetTpidrroEl0` / `set_tpidr_el0` but this may not be propagating to the AArch32 CP15 register correctly.

3. **Compare CP15 register state** — Dump all CP15 coprocessor registers at the point of `__nnDetailInitLibc0` entry and compare with what a real Switch or the upstream yuzu would provide.

## Investigation methodology

### Phase 1: Initial log analysis

Started with `crash.txt` — a 7750-line runtime log captured with `RUST_LOG=info`. Grepped for `ERROR`, `PANIC`, `Break`, `STUBBED`, `SetTerminateResult` to find the crash point. This identified:
- `SetTerminateResult(0xe401)` at SVC #169
- `Break(0, 0, 0)` at SVC #171
- `nvdrv Initialize (STUBBED)` at SVC #166

**Initial (wrong) hypothesis:** nvdrv Initialize is stubbed → game fails to set up GPU → crash.

### Phase 2: Timeline reconstruction

Extracted all `GetService` and `parsed_cmd=` entries to reconstruct the full IPC timeline. Mapped handle values to service names. Key insight: **the abort at SVC #135 happens BEFORE nvdrv is even opened** (SVC #164). nvdrv is not the cause.

Between SVC #88 (pctl:a setup) and SVC #135 (abort handler entry), there are only `SignalProcessWideKey` calls — no IPC at all. The failure is in **pure userspace code** during NintendoSDK module initialization.

### Phase 3: Abort struct analysis

Added diagnostic code to `yuzu_cmd/src/main.rs` at SVC #135 and #136 to:
1. Dump the abort info struct at r4 (0x3c9a000)
2. Dereference each 32-bit field as a potential string pointer
3. Filter printable ASCII strings (length >= 2, bytes 0x20-0x7E)

This revealed `[+0x14] = 0x00d61761 => "RootHeap"` — the heap name in the `nn::lmem` allocator. Initially misleading: it suggested a heap allocation failure.

### Phase 4: Heap memory verification

Added a write/readback test at SVC #90 (right after `SetHeapSize`):
```
HEAP TEST: addr=0x249a000  OK=true
HEAP TEST: addr=0x61200000 OK=true   ← the suspect address from abort struct
HEAP TEST: addr=0x7a000000 OK=true
```
All 2 GB of heap memory is fully accessible via `ProcessMemoryData` sparse pages. **Heap is not the problem.**

### Phase 5: Memory system architecture investigation

Explored how `set_heap_size` maps memory. Discovered that `KPageTableBase::operate(Map)` gates on `m_memory` and `m_impl` being `Some`:
```rust
if let (Some(memory), Some(impl_pt)) = (&self.m_memory, &mut self.m_impl) {
    memory.lock().unwrap().map_memory_region(...);
}
```
Found that `set_memory()` and `initialize_impl()` are **never called** — both are `None`. This means the kernel page table never maps the heap in the `DeviceMemory` backing store. However, the JIT uses `ProcessMemoryData` (sparse pages) directly, not `DeviceMemory`, so this is a separate issue, not the immediate crash cause.

### Phase 6: SVC error scan

Searched for any SVC returning a non-zero result before the crash:
```
grep "result=0x[^0]|-> result=[^0]" crash.log
```
Only result: `SetTerminateResult(0xe401)` — the game's own crash report, not an SVC error. **No SVC fails before the abort.**

### Phase 7: Instruction-level tracing

Used the existing `RUZU_STEP_AFTER_SVC=134` env var to enable single-step mode after SVC #134. Initially traced 5000 instructions — showed heap allocator code but didn't reach the abort (2+ seconds of execution, millions of instructions).

Replaced the linear trace with a **ring buffer** (500 entries) that only dumps when PC enters the abort handler range (0x1d31d00-0x1d32000):

```rust
static mut RING_BUF: [(u64, u32, [u64; 8]); 500] = [(0, 0, [0; 8]); 500];
// ... record every instruction ...
// When PC hits abort handler, dump the ring buffer
if thread_context.pc >= 0x1d31d00 && thread_context.pc < 0x1d32000 {
    // dump last 500 instructions
}
```

### Phase 8: Decoding the abort decision

The ring buffer captured the critical 500 instructions. Key findings:

1. **Instructions [0-467]**: `strcmp` loop at PC=0x201b78 comparing two strings byte-by-byte. The strings match (R2==R3 at terminating NUL). This is the rtld resolving a symbol name.

2. **Instruction [472]** (PC=0x200908): `CMP R0, #0` where R0=0x207c850 (non-NULL). This is the return value from a function that found a module descriptor with abort info.

3. **Instructions [477-479]**: Resolve the abort function:
   - R1 = [0x22c8000+0x10] = 0x1c9c000 (subsdk4 base)
   - R0 = [0x207c850+0x4] = 0x95d94 (offset)
   - R4 = R0 + R1 = **0x1d31d94** (`nn::diag::detail::AbortImpl`)

4. **Instructions [486-487]**: Store abort function pointer, return 1 (error).

5. **Instruction [499]** (PC=0x1d31d94): Enter `nn::diag::detail::AbortImpl` — ring buffer trigger fires, trace dumps.

### Phase 9: Identifying `__nnDetailInitLibc0`

Cross-referenced the abort path with the boot log's symbol table dump. The rtld log showed:
```
FOUND '__nnDetailInitLibc0' at strtab+0x2a (abs=0x204502)
  sym[21]: val=0x0 sz=0x0 bind=1 type=0 other=0 shndx=0   ← rtld (unresolved)
FOUND '__nnDetailInitLibc0' at strtab+0x98aa (abs=0xd524e2)
  sym[859]: val=0x29c sz=0x4 bind=1 type=2 other=0 shndx=1 ← main module (resolved, addr=0x20629c)
```

The rtld resolves `__nnDetailInitLibc0` from the `main` module and calls it for each loaded module. One of these calls returns a non-null abort handler, triggering the crash.

### Key tools used

| Tool | Purpose |
|------|---------|
| `grep -E` with regex | Pattern matching across 7750-line log |
| `RUST_LOG=info` | Runtime log level for SVC and IPC tracing |
| Custom SVC #135/136 diagnostics | Abort struct dump with string pointer dereference |
| Heap write/readback test | Verify memory accessibility at specific addresses |
| `RUZU_STEP_AFTER_SVC=N` | Per-instruction JIT tracing after SVC threshold |
| Ring buffer (500 entries) | Capture last N instructions before abort without perf penalty |
| ARM instruction manual decoding | Decode `0xe3500000` → `CMP R0, #0`, etc. |
| Symbol table from boot log | Map code addresses to function names |

## Reproduction

```bash
mkdir -p /tmp/ruzu-data/ruzu/keys && cp ~/.local/share/yuzu/keys/* /tmp/ruzu-data/ruzu/keys/
env XDG_DATA_HOME=/tmp/ruzu-data \
    XDG_CACHE_HOME=/tmp/ruzu-cache \
    XDG_CONFIG_HOME=/tmp/ruzu-config \
    RUST_LOG=info \
    RUZU_STEP_AFTER_SVC=134 \
    cargo run --bin yuzu-cmd -- -g "/home/vricosti/Games/Emulators/Switch/common/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp"
```
