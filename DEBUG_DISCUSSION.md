# Debug Discussion: MK8D Bring-Up Status in `ruzu`

## Context

We are bringing up Mario Kart 8 Deluxe in the Rust port of yuzu (`ruzu`).

Current reference points:

- Rust tree: `/home/vricosti/Dev/emulators/ruzu`
- Upstream C++ tree: `/home/vricosti/Dev/emulators/zuyu`
- Upstream `yuzu-cmd` runs MK8D normally on this machine
- `ruzu` is being tested mainly through:

```bash
env XDG_DATA_HOME=/tmp/ruzu-data \
    XDG_CACHE_HOME=/tmp/ruzu-cache \
    XDG_CONFIG_HOME=/tmp/ruzu-config \
    RUST_LOG=info \
    cargo run --bin yuzu-cmd -- --renderer null -g "..."
```

The important shift is that the old rtld failure:

```text
[rtld] Unresolved symbol: '__nnDetailInitLibc0'
PREFETCH_ABORT at PC=0x0
```

is no longer the active blocker.

That failure was real earlier, but it was caused by AArch32 execution/decoder bugs in `rdynarmic`, not by NSO mapping or a permanently broken module chain.

## Current Status

MK8D now gets:

- past rtld module discovery
- past the old unresolved `__nnDetailInitLibc0` failure
- past the early rtld bootstrap crashes
- into later kernel/SVC traffic (`QueryMemory`, `GetInfo`, `GetThreadPriority`, etc.)

At the time of writing:

- the null-renderer run no longer aborts in the early rtld window
- the process stays alive for an extended soak after later SVCs
- we do not yet have proof of fully correct game startup or gameplay

So the problem has changed from:

- "why does rtld immediately explode?"

to:

- "what still prevents MK8D from progressing all the way through startup under `ruzu`?"

## What Was Actually Blocking MK8D

### 1. `rdynarmic` misdecoded ARM media instructions

This was the most important class of bug.

The first confirmed bad decode was:

- `0xE7DF0E1F`

It was being decoded as `UBFX`, but upstream semantics require `BFC`.

That broke rtld state and symbol-resolution flow badly enough to produce the misleading unresolved-symbol crash.

Later, after that was fixed, we hit more concrete decoder gaps:

- `0xE7DF2F9F` should decode as `BFC`
- `0xE6EF1071` should decode as `UXTB`

These were all in the ARM media/extension decoding area.

### 2. VFP system register coprocessor instructions were not handled

After the first decoder fix, MK8D progressed and then died on:

- `0xEEF13A10`
- `0xEEE13A10`

These are VFP `VMRS` / `VMSR` forms, encoded via `MRC` / `MCR` on cp10/cp11.

`rdynarmic` was treating them as unsupported generic coprocessor traffic.

### 3. A callback argument ordering bug existed in A32 exception handling

The x64 backend was passing:

- `(exception, pc)`

to the A32 exception callback instead of:

- `(pc, exception)`

This made exception-site diagnosis more confusing and was wrong in its own right.

### 4. The JIT spill-slot budget was too small

Once rtld executed further, `rdynarmic` hit:

```text
All spill locations are full
```

That was not a game-specific logic failure. It was a backend resource limit in the Rust JIT.

### 5. `svc_dispatch.rs` had a wrong `GetInfo` table

After the rtld/JIT fixes, MK8D advanced far enough that kernel stub accuracy started to matter more.

The fast-path `GetInfo` handlers in:

- `core/src/hle/kernel/svc_dispatch.rs`

had the wrong numbering after `ProgramId`.

In particular:

- `InfoType::RandomEntropy` is `11`

but the table treated `11` as unknown and incorrectly attached the random-entropy stub to `20`.

That meant later startup code was getting wrong or zero data from `GetInfo`.

## What Has Been Done

### Build/runtime plumbing

- `common/src/x64/cpu_detect.rs`
  - fixed `__cpuid_count` usage by putting it in the required `unsafe` block
- workspace was pointed at the vendored Rust dynarmic tree:
  - `Cargo.toml`
  - `rdynarmic = { path = "rdynarmic" }`

### `rdynarmic` decoder fixes

- `rdynarmic/src/frontend/a32/decoder.rs`
  - fixed ARM media instruction classification
  - replaced fragile ad hoc media decode logic with explicit upstream-style mask matching for the implemented instructions
  - corrected concrete crash opcodes:
    - `0xE7DF0E1F` -> `BFC`
    - `0xE7DF2F9F` -> `BFC`
    - `0xE6EF1071` -> `UXTB`
  - corrected extension-family matching to treat rotation bits as variable where appropriate

### `rdynarmic` VFP cp10/cp11 support

- `rdynarmic/src/frontend/a32/translate/coprocessor.rs`
  - added support for:
    - `VMRS Rt, FPSCR`
    - `VMRS APSR_nzcv, FPSCR`
    - `VMSR FPSCR, Rt`
    - `VMRS Rt, FPEXC`
    - `VMSR FPEXC, Rt`

Current behavior:

- `FPSCR` reads/writes are wired to IR getters/setters
- `FPEXC` read returns `EN` set (`1 << 30`)
- `FPEXC` write is a no-op

### `rdynarmic` callback/backend fixes

- `rdynarmic/src/backend/x64/a32_emit_a32.rs`
  - fixed `A32ExceptionRaised` callback argument ordering

- `rdynarmic/src/backend/x64/stack_layout.rs`
- `rdynarmic/src/backend/x64/reg_alloc.rs`
  - increased spill-slot count from `64` to `192`

### Kernel stub fix

- `core/src/hle/kernel/svc_dispatch.rs`
  - corrected both 32-bit and 64-bit fast-path `GetInfo` enum mapping
  - aligned ids for:
    - `IdleTickCount`
    - `RandomEntropy`
    - `InitialProcessIdRange`
    - `UserExceptionContextAddress`
    - `TotalNonSystemMemorySize`
    - `UsedNonSystemMemorySize`
    - `IsApplication`
    - `FreeThreadCount`
    - `ThreadTickCount`

### Instrumentation added during triage

- `yuzu_cmd/src/main.rs`
  - module/symbol dumps for rtld and loaded NSOs
  - crash-site decode logging around prefetch aborts
  - rtld/module-object dumps after the `QueryMemory` sweep

This instrumentation was critical in proving that:

- module objects were being built
- `__nnDetailInitLibc0` was actually present in `main`
- the old failure was not primarily caused by missing NSO/module mapping

## Verification Performed

### Upstream comparison

Upstream `yuzu-cmd` was run successfully with MK8D on this machine.

That proved:

- the game dump is fine
- the environment is fine
- the remaining gap is in `ruzu`

### Local Rust verification

The following were repeatedly verified while iterating:

- `cargo check -p rdynarmic -p yuzu_cmd`
- `cargo check -p core -p yuzu_cmd`
- targeted `rdynarmic` tests for exact bad opcodes

Added regression coverage includes decode checks for:

- `0xE7DF0E1F`
- `0xE7DF2F9F`
- `0xE6EF1071`

The null-renderer MK8D run now:

- gets through the old rtld failure window
- reaches later SVCs like:
  - `GetInfo`
  - `GetThreadPriority`
- remains alive for an extended period without a new abort being logged

## Important Conclusions

### The original "empty symbol / bad module object" theory is obsolete

It was a reasonable early hypothesis, but the stronger evidence now says:

- loader/module setup was good enough for rtld to proceed
- the main breakage was in AArch32 decode/execute parity

### The early MK8D blocker was mostly JIT/decoder correctness, not graphics

Running with:

- `--renderer null`

was enough to reproduce the failures and then confirm the fixes.

The initial blocker was not Vulkan/OpenGL.

### We are now in a different debugging phase

The work is no longer about:

- immediate rtld death

It is now about:

- later startup correctness
- kernel/SVC stub parity
- possibly service behavior or additional CPU/JIT gaps that only appear after rtld completes

## Most Likely Remaining Problem Areas

These are the next areas to investigate if MK8D still does not fully boot:

1. `svc_dispatch.rs` fast-path stubs beyond `GetInfo`
   - many handlers still return fixed values
   - later startup may depend on more realistic responses

2. thread/process kernel behavior
   - `GetThreadPriority`
   - thread-core-mask paths
   - resource-limit behavior

3. service-layer parity after rtld
   - once the game gets out of bootstrap, HLE service correctness matters more

4. any additional A32/JIT gaps that only appear deeper into startup

## Suggested Next Steps

1. Reduce or gate the heavy rtld/module dumps in `yuzu_cmd/src/main.rs`
   - they were useful for bring-up
   - they now add a lot of noise to later-phase debugging

2. Add better progress markers after rtld
   - service init milestones
   - applet/AM milestones
   - frame-loop or main-thread progress markers

3. Audit the next kernel fast-path stubs actually hit by MK8D
   - start from `GetThreadPriority`
   - compare against upstream behavior

4. Determine whether the current "quiet run" is:
   - successful idle progress
   - a soft hang
   - waiting on an unimplemented service/kernel path

5. If needed, compare later post-rtld SVC sequences against upstream
   - not the old rtld symbol walk anymore
   - the next divergence window after the current fixes

## Summary

The original rtld unresolved-symbol crash in MK8D has been fixed.

The main fixes were:

- ARM media/extension decoder corrections in `rdynarmic`
- VFP `VMRS` / `VMSR` support
- A32 exception callback fix
- larger spill-slot capacity
- corrected `GetInfo` stub numbering

`ruzu` now gets substantially further into MK8D startup under the null renderer and no longer dies in the old rtld window.

What remains is a later-stage bring-up/debugging problem, not the original rtld symbol-resolution failure.

---

Updated by Codex on 2026-03-14.
